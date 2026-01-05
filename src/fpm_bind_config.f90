module fpm_bind_config
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit
    use tomlf, only: toml_table, toml_parse, toml_error, get_value, toml_array, len
    implicit none
    private

    public :: project_config_t, bind_config_t
    public :: read_fpm_toml, read_bind_toml, find_source_files

    type :: project_config_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: version
        character(len=:), allocatable :: source_dir
        character(len=:), allocatable :: project_dir
    end type project_config_t

    type :: python_config_t
        character(len=:), allocatable :: package_name
        character(len=:), allocatable :: module_name
        logical :: direct_c = .true.
        character(len=:), allocatable :: kind_map
        character(len=:), allocatable :: output_dir
    end type python_config_t

    type :: bind_config_t
        type(python_config_t) :: python
        logical :: python_enabled = .false.
    end type bind_config_t

contains

    subroutine read_fpm_toml(project_dir, config, error)
        character(len=*), intent(in) :: project_dir
        type(project_config_t), intent(out) :: config
        character(len=:), allocatable, intent(out) :: error

        type(toml_table), allocatable :: table
        type(toml_error), allocatable :: parse_error
        character(len=512) :: fpm_toml_path
        integer :: unit_num, ios

        config%project_dir = project_dir
        fpm_toml_path = trim(project_dir) // "/fpm.toml"

        open(newunit=unit_num, file=trim(fpm_toml_path), status='old', &
             action='read', iostat=ios)
        if (ios /= 0) then
            error = "Cannot open " // trim(fpm_toml_path)
            return
        end if

        call toml_parse(table, unit_num, parse_error)
        close(unit_num)

        if (allocated(parse_error)) then
            error = "Error parsing fpm.toml: " // parse_error%message
            return
        end if

        call get_value(table, "name", config%name)
        if (.not. allocated(config%name)) then
            error = "fpm.toml missing required 'name' field"
            return
        end if

        call get_value(table, "version", config%version, "0.1.0")

        config%source_dir = trim(project_dir) // "/src"
    end subroutine read_fpm_toml

    subroutine read_bind_toml(project_dir, config, error)
        character(len=*), intent(in) :: project_dir
        type(bind_config_t), intent(out) :: config
        character(len=:), allocatable, intent(out) :: error

        type(toml_table), allocatable :: table
        type(toml_table), pointer :: python_table => null()
        type(toml_error), allocatable :: parse_error
        character(len=512) :: bind_toml_path
        integer :: unit_num, ios
        logical :: file_exists

        bind_toml_path = trim(project_dir) // "/bind/python/bind.toml"
        inquire(file=trim(bind_toml_path), exist=file_exists)

        if (.not. file_exists) then
            bind_toml_path = trim(project_dir) // "/bind.toml"
            inquire(file=trim(bind_toml_path), exist=file_exists)
        end if

        if (.not. file_exists) then
            error = "No bind.toml found in project root or bind/python/"
            return
        end if

        open(newunit=unit_num, file=trim(bind_toml_path), status='old', &
             action='read', iostat=ios)
        if (ios /= 0) then
            error = "Cannot open " // trim(bind_toml_path)
            return
        end if

        call toml_parse(table, unit_num, parse_error)
        close(unit_num)

        if (allocated(parse_error)) then
            error = "Error parsing bind.toml: " // parse_error%message
            return
        end if

        call get_value(table, "python", python_table)
        if (associated(python_table)) then
            config%python_enabled = .true.
            call get_value(python_table, "package-name", config%python%package_name)
            call get_value(python_table, "module-name", config%python%module_name)
            call get_value(python_table, "direct-c", config%python%direct_c, .true.)
            call get_value(python_table, "kind-map", config%python%kind_map)
            call get_value(python_table, "output-dir", config%python%output_dir)
        else
            config%python_enabled = .true.
        end if
    end subroutine read_bind_toml

    subroutine find_source_files(source_dir, files, n_files, error)
        character(len=*), intent(in) :: source_dir
        character(len=256), allocatable, intent(out) :: files(:)
        integer, intent(out) :: n_files
        character(len=:), allocatable, intent(out) :: error

        character(len=512) :: cmd, line
        integer :: unit_num, ios, i, count
        character(len=256) :: temp_files(1000)

        cmd = "find " // trim(source_dir) // " -name '*.f90' -o -name '*.F90' " // &
              "2>/dev/null"

        open(newunit=unit_num, file="/tmp/fpm_bind_files.txt", status='replace', &
             action='write', iostat=ios)
        close(unit_num)

        call execute_command_line(trim(cmd) // " > /tmp/fpm_bind_files.txt", wait=.true.)

        open(newunit=unit_num, file="/tmp/fpm_bind_files.txt", status='old', &
             action='read', iostat=ios)
        if (ios /= 0) then
            error = "Failed to list source files"
            n_files = 0
            return
        end if

        count = 0
        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) > 0) then
                count = count + 1
                if (count <= 1000) temp_files(count) = trim(line)
            end if
        end do
        close(unit_num)

        n_files = count
        if (n_files > 0) then
            allocate(files(n_files))
            do i = 1, n_files
                files(i) = temp_files(i)
            end do
        end if
    end subroutine find_source_files

end module fpm_bind_config
