module fpm_bind_python
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit
    use fpm_bind_config, only: project_config_t, bind_config_t, find_source_files
    implicit none
    private

    public :: generate_python_bindings, build_python_bindings, install_python_bindings

contains

    subroutine generate_python_bindings(project, bind_cfg, do_build, error)
        type(project_config_t), intent(in) :: project
        type(bind_config_t), intent(in) :: bind_cfg
        logical, intent(in) :: do_build
        character(len=:), allocatable, intent(out) :: error

        character(len=:), allocatable :: package_name, module_name, output_dir
        character(len=256), allocatable :: source_files(:)
        character(len=4096) :: cmd
        character(len=512) :: files_list_path, kind_map_path
        integer :: n_files, i, exit_status, unit_num, ios

        if (allocated(bind_cfg%python%package_name)) then
            package_name = bind_cfg%python%package_name
        else
            package_name = project%name
        end if

        if (allocated(bind_cfg%python%module_name)) then
            module_name = bind_cfg%python%module_name
        else
            module_name = project%name
        end if

        if (allocated(bind_cfg%python%output_dir)) then
            output_dir = bind_cfg%python%output_dir
        else
            output_dir = trim(project%project_dir) // "/build/bind/python"
        end if

        call execute_command_line("mkdir -p " // output_dir, wait=.true.)

        call find_source_files(project%source_dir, source_files, n_files, error)
        if (allocated(error)) return

        if (n_files == 0) then
            error = "No Fortran source files found in " // project%source_dir
            return
        end if

        write(stderr, '(A,I0,A)') "Found ", n_files, " source files"

        files_list_path = trim(output_dir) // "/run_f90wrap.sh"
        open(newunit=unit_num, file=trim(files_list_path), status='replace', &
             action='write', iostat=ios)
        if (ios /= 0) then
            error = "Cannot create f90wrap script"
            return
        end if

        ! Generate default kind map for iso_fortran_env kinds
        if (.not. allocated(bind_cfg%python%kind_map)) then
            kind_map_path = trim(output_dir) // "/kind_map"
            call generate_default_kind_map(kind_map_path, error)
            if (allocated(error)) return
        else
            kind_map_path = trim(project%project_dir) // "/" // trim(bind_cfg%python%kind_map)
        end if

        write(unit_num, '(A)') "#!/bin/bash"
        write(unit_num, '(A)') "cd " // output_dir
        write(unit_num, '(A)', advance='no') "f90wrap -m " // trim(module_name)

        if (bind_cfg%python%direct_c) then
            write(unit_num, '(A)', advance='no') " --direct-c"
        end if

        if (do_build) then
            write(unit_num, '(A)', advance='no') " --build"
        end if

        write(unit_num, '(A)', advance='no') " -k " // trim(kind_map_path)

        write(unit_num, '(A)') " \"

        do i = 1, n_files
            if (i < n_files) then
                write(unit_num, '(A)') "  " // trim(source_files(i)) // " \"
            else
                write(unit_num, '(A)') "  " // trim(source_files(i))
            end if
        end do

        close(unit_num)

        write(stderr, '(A)') "Running f90wrap..."

        call execute_command_line("bash " // trim(files_list_path), wait=.true., &
                                  exitstat=exit_status)

        if (exit_status /= 0) then
            error = "f90wrap failed with exit code"
            return
        end if

        if (.not. do_build) then
            call generate_pyproject_toml(project, bind_cfg, output_dir, package_name, error)
            if (allocated(error)) return

            call generate_init_py(output_dir, package_name, module_name, error)
            if (allocated(error)) return
        end if

        write(stderr, '(A)') "Python bindings generated in " // output_dir
    end subroutine generate_python_bindings

    subroutine generate_pyproject_toml(project, bind_cfg, output_dir, package_name, error)
        type(project_config_t), intent(in) :: project
        type(bind_config_t), intent(in) :: bind_cfg
        character(len=*), intent(in) :: output_dir, package_name
        character(len=:), allocatable, intent(out) :: error

        integer :: unit_num, ios
        character(len=512) :: filepath

        filepath = trim(output_dir) // "/pyproject.toml"

        open(newunit=unit_num, file=trim(filepath), status='replace', &
             action='write', iostat=ios)
        if (ios /= 0) then
            error = "Cannot create pyproject.toml"
            return
        end if

        write(unit_num, '(A)') '[build-system]'
        write(unit_num, '(A)') 'requires = ["setuptools", "wheel", "numpy"]'
        write(unit_num, '(A)') 'build-backend = "setuptools.build_meta"'
        write(unit_num, '(A)') ''
        write(unit_num, '(A)') '[project]'
        write(unit_num, '(A)') 'name = "' // trim(package_name) // '"'
        write(unit_num, '(A)') 'version = "' // trim(project%version) // '"'
        write(unit_num, '(A)') 'requires-python = ">=3.9"'
        write(unit_num, '(A)') 'dependencies = ["numpy"]'

        close(unit_num)
    end subroutine generate_pyproject_toml

    subroutine generate_init_py(output_dir, package_name, module_name, error)
        character(len=*), intent(in) :: output_dir, package_name, module_name
        character(len=:), allocatable, intent(out) :: error

        integer :: unit_num, ios
        character(len=512) :: pkg_dir, filepath

        pkg_dir = trim(output_dir) // "/" // trim(package_name)
        call execute_command_line("mkdir -p " // trim(pkg_dir), wait=.true.)

        filepath = trim(pkg_dir) // "/__init__.py"

        open(newunit=unit_num, file=trim(filepath), status='replace', &
             action='write', iostat=ios)
        if (ios /= 0) then
            error = "Cannot create __init__.py"
            return
        end if

        write(unit_num, '(A)') '"""' // trim(package_name) // ' - Python bindings"""'
        write(unit_num, '(A)') ''
        write(unit_num, '(A)') 'try:'
        write(unit_num, '(A)') '    from ._' // trim(module_name) // ' import *'
        write(unit_num, '(A)') 'except ImportError:'
        write(unit_num, '(A)') '    pass  # Extension not built yet'

        close(unit_num)
    end subroutine generate_init_py

    subroutine generate_default_kind_map(filepath, error)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: error

        integer :: unit_num, ios

        open(newunit=unit_num, file=trim(filepath), status='replace', &
             action='write', iostat=ios)
        if (ios /= 0) then
            error = "Cannot create kind_map file"
            return
        end if

        ! Default kind map for iso_fortran_env kinds
        write(unit_num, '(A)') "{"
        write(unit_num, '(A)') "  'real': {"
        write(unit_num, '(A)') "    'dp': 'double',"
        write(unit_num, '(A)') "    'real64': 'double',"
        write(unit_num, '(A)') "    'real32': 'float'"
        write(unit_num, '(A)') "  },"
        write(unit_num, '(A)') "  'integer': {"
        write(unit_num, '(A)') "    'int32': 'int',"
        write(unit_num, '(A)') "    'int64': 'long_long'"
        write(unit_num, '(A)') "  }"
        write(unit_num, '(A)') "}"

        close(unit_num)
    end subroutine generate_default_kind_map

    subroutine build_python_bindings(project, bind_cfg, error)
        type(project_config_t), intent(in) :: project
        type(bind_config_t), intent(in) :: bind_cfg
        character(len=:), allocatable, intent(out) :: error

        character(len=:), allocatable :: output_dir, module_name
        character(len=4096) :: cmd
        integer :: exit_status

        if (allocated(bind_cfg%python%output_dir)) then
            output_dir = bind_cfg%python%output_dir
        else
            output_dir = trim(project%project_dir) // "/build/bind/python"
        end if

        if (allocated(bind_cfg%python%module_name)) then
            module_name = bind_cfg%python%module_name
        else
            module_name = project%name
        end if

        write(stderr, '(A)') "Building Python extension..."

        cmd = "cd " // output_dir // " && f2py-f90wrap -c -m _" // trim(module_name) // &
              " f90wrap_*.f90"

        write(stderr, '(A)') trim(cmd)
        call execute_command_line(trim(cmd), wait=.true., exitstat=exit_status)

        if (exit_status /= 0) then
            error = "Build failed"
            return
        end if

        write(stderr, '(A)') "Build complete"
    end subroutine build_python_bindings

    subroutine install_python_bindings(project, bind_cfg, error)
        type(project_config_t), intent(in) :: project
        type(bind_config_t), intent(in) :: bind_cfg
        character(len=:), allocatable, intent(out) :: error

        character(len=:), allocatable :: output_dir
        character(len=4096) :: cmd
        integer :: exit_status

        if (allocated(bind_cfg%python%output_dir)) then
            output_dir = bind_cfg%python%output_dir
        else
            output_dir = trim(project%project_dir) // "/build/bind/python"
        end if

        write(stderr, '(A)') "Installing Python package..."

        cmd = "pip install -e " // output_dir

        write(stderr, '(A)') trim(cmd)
        call execute_command_line(trim(cmd), wait=.true., exitstat=exit_status)

        if (exit_status /= 0) then
            error = "Install failed"
            return
        end if

        write(stderr, '(A)') "Install complete"
    end subroutine install_python_bindings

end module fpm_bind_python
