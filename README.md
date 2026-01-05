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

This generates Python bindings in `build/bind/python/`:
- `f90wrap_*.f90` - Fortran wrapper modules
- `_<module>.c` - C extension (direct-c mode)
- `<module>.py` - Python wrapper
- `pyproject.toml` - Package metadata

### 3. Build and install the Python package

First, build your Fortran library:
```bash
fpm build --profile release
```

Then compile and install the Python extension:
```bash
cd build/bind/python
pip install -e .
```

Note: For direct-c mode, you may need to compile the C extension manually
or add a `setup.py` with extension configuration. See f90wrap documentation
for details on building extensions.

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
