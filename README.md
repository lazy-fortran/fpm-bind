# fpm-bind

FPM plugin for generating language bindings from Fortran projects.

## Installation

```bash
git clone git@github.com:lazy-fortran/fpm-bind.git
cd fpm-bind
fpm install
```

This installs `fpm-bind` to `~/.local/bin/`. Ensure this is in your PATH.

## Requirements

- [fpm](https://github.com/fortran-lang/fpm) (Fortran Package Manager)
- [f90wrap](https://github.com/jameskermode/f90wrap) for Python bindings

## Usage

### 1. Add bind.toml to your project

Create `bind.toml` next to your `fpm.toml`:

```toml
[python]
package-name = "mypackage"
```

### 2. Generate and build bindings

```bash
cd your-fortran-project
fpm bind python --build
```

This generates and compiles Python bindings in `build/bind/python/`:
- `_<module>.so` - Compiled Python extension
- `<module>.py` - Python wrapper module

### 3. Use the bindings

```python
import sys
sys.path.insert(0, 'build/bind/python')
import mypackage
mypackage.mypackage.some_function()
```

Or generate only (without building):
```bash
fpm bind python  # Generate wrappers only
```

## Configuration

### bind.toml options

```toml
[python]
package-name = "mypackage"    # Python package name (default: fpm project name)
module-name = "mymodule"      # Python module name (default: package-name)
direct-c = true               # Use direct C bindings (default: true)
kind-map = "kind_map.json"    # Custom kind mapping file
output-dir = "build/python"   # Output directory
```

## Examples

See `example/` for complete demo projects:

- `simple/` - basic functions and subroutines
- `arrays/` - array operations
- `derived_types/` - derived types with methods

```bash
cd example/arrays
fpm bind python --build
cd build/bind/python
python -c "import arrays; print(arrays.arrays.dot_product_vec([1,2,3], [4,5,6], 3))"
# Output: 32.0
```

## How it works

fpm-bind uses f90wrap to parse Fortran source files and generate:
- Fortran wrapper modules (`f90wrap_*.f90`)
- Python wrapper code (`<module>.py`)
- C bindings when `direct-c = true`

## License

MIT
