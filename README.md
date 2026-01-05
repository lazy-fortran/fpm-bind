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

### 2. Generate bindings

```bash
cd your-fortran-project
fpm bind python
```

This generates Python bindings in `build/bind/python/`.

### 3. Build and install (optional)

```bash
fpm bind python --build    # Compile the extension
fpm bind python --install  # pip install -e
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

See `examples/` for complete demo projects:

- `simple/` - basic functions and subroutines
- `arrays/` - array operations
- `derived_types/` - derived types with methods

```bash
cd examples/simple
fpm bind python
```

## How it works

fpm-bind uses f90wrap to parse Fortran source files and generate:
- Fortran wrapper modules (`f90wrap_*.f90`)
- Python wrapper code (`<module>.py`)
- C bindings when `direct-c = true`

## License

MIT
