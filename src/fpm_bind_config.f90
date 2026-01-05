module fpm_bind_config
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit
    use tomlf, only: toml_table, toml_parse, toml_error, get_value, toml_array, len
    implicit none
    private

    public :: project_config_t, bind_config_t, python_config_t
    public :: read_fpm_toml, read_bind_toml, find_source_files
    public :: filter_source_files, matches_glob_pattern

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
        character(len=256), allocatable :: include(:)
        character(len=256), allocatable :: exclude(:)
        integer :: n_include = 0
        integer :: n_exclude = 0
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
        type(toml_array), pointer :: include_array => null(), exclude_array => null()
        type(toml_error), allocatable :: parse_error
        character(len=512) :: bind_toml_path
        character(len=:), allocatable :: tmp_str
        integer :: unit_num, ios, i, arr_len
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

            call get_value(python_table, "include", include_array)
            if (associated(include_array)) then
                arr_len = len(include_array)
                if (arr_len > 0) then
                    allocate(config%python%include(arr_len))
                    config%python%n_include = arr_len
                    do i = 1, arr_len
                        call get_value(include_array, i, tmp_str)
                        if (allocated(tmp_str)) then
                            config%python%include(i) = tmp_str
                        else
                            config%python%include(i) = ""
                        end if
                    end do
                end if
            end if

            call get_value(python_table, "exclude", exclude_array)
            if (associated(exclude_array)) then
                arr_len = len(exclude_array)
                if (arr_len > 0) then
                    allocate(config%python%exclude(arr_len))
                    config%python%n_exclude = arr_len
                    do i = 1, arr_len
                        call get_value(exclude_array, i, tmp_str)
                        if (allocated(tmp_str)) then
                            config%python%exclude(i) = tmp_str
                        else
                            config%python%exclude(i) = ""
                        end if
                    end do
                end if
            end if
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

    subroutine filter_source_files(files, n_files, py_config, project_dir, filtered, n_filtered)
        character(len=256), intent(in) :: files(:)
        integer, intent(in) :: n_files
        type(python_config_t), intent(in) :: py_config
        character(len=*), intent(in) :: project_dir
        character(len=256), allocatable, intent(out) :: filtered(:)
        integer, intent(out) :: n_filtered

        character(len=256) :: temp_files(1000)
        character(len=512) :: rel_path
        integer :: i, j, count, proj_len
        logical :: include_file, excluded

        proj_len = len_trim(project_dir)
        count = 0

        do i = 1, n_files
            if (len_trim(files(i)) <= proj_len) then
                rel_path = files(i)
            else if (files(i)(1:proj_len) == project_dir(1:proj_len)) then
                rel_path = files(i)(proj_len+2:)
            else
                rel_path = files(i)
            end if

            include_file = .false.
            excluded = .false.

            if (py_config%n_include > 0) then
                do j = 1, py_config%n_include
                    if (matches_glob_pattern(trim(rel_path), trim(py_config%include(j)))) then
                        include_file = .true.
                        exit
                    end if
                    if (matches_glob_pattern(trim(files(i)), trim(py_config%include(j)))) then
                        include_file = .true.
                        exit
                    end if
                end do
            else
                include_file = .true.
            end if

            if (include_file .and. py_config%n_exclude > 0) then
                do j = 1, py_config%n_exclude
                    if (matches_glob_pattern(trim(rel_path), trim(py_config%exclude(j)))) then
                        excluded = .true.
                        exit
                    end if
                    if (matches_glob_pattern(trim(files(i)), trim(py_config%exclude(j)))) then
                        excluded = .true.
                        exit
                    end if
                end do
            end if

            if (include_file .and. .not. excluded) then
                count = count + 1
                if (count <= 1000) temp_files(count) = files(i)
            end if
        end do

        n_filtered = count
        if (n_filtered > 0) then
            allocate(filtered(n_filtered))
            do i = 1, n_filtered
                filtered(i) = temp_files(i)
            end do
        end if
    end subroutine filter_source_files

    pure recursive function matches_glob_pattern(path, pattern) result(matches)
        character(len=*), intent(in) :: path, pattern
        logical :: matches

        integer :: i, j, p_len, s_len
        character(len=512) :: pat_part, path_part

        matches = .false.
        p_len = len_trim(pattern)
        s_len = len_trim(path)

        if (p_len == 0) then
            matches = (s_len == 0)
            return
        end if

        if (index(pattern, "**") > 0) then
            i = index(pattern, "**")
            if (i == 1) then
                pat_part = pattern(3:)
                if (len_trim(pat_part) == 0) then
                    matches = .true.
                    return
                end if
                if (pat_part(1:1) == "/") pat_part = pat_part(2:)
                do j = 1, s_len
                    if (matches_glob_pattern(path(j:), trim(pat_part))) then
                        matches = .true.
                        return
                    end if
                end do
            else
                pat_part = pattern(1:i-1)
                if (pat_part(len_trim(pat_part):len_trim(pat_part)) == "/") then
                    pat_part = pat_part(1:len_trim(pat_part)-1)
                end if
                if (s_len >= len_trim(pat_part)) then
                    if (matches_simple_glob(path(1:len_trim(pat_part)), trim(pat_part))) then
                        matches = matches_glob_pattern(path, pattern(i+2:))
                    end if
                end if
            end if
            return
        end if

        if (index(pattern, "*") > 0) then
            i = index(pattern, "*")
            if (i == 1) then
                pat_part = pattern(2:)
                if (len_trim(pat_part) == 0) then
                    matches = (index(path, "/") == 0)
                    return
                end if
                do j = 1, s_len
                    if (path(j:j) == "/") exit
                    if (matches_glob_pattern(path(j:), trim(pat_part))) then
                        matches = .true.
                        return
                    end if
                end do
            else
                pat_part = pattern(1:i-1)
                if (s_len >= len_trim(pat_part)) then
                    if (path(1:len_trim(pat_part)) == pat_part(1:len_trim(pat_part))) then
                        matches = matches_glob_pattern(path(len_trim(pat_part)+1:), pattern(i:))
                    end if
                end if
            end if
            return
        end if

        matches = (trim(path) == trim(pattern))
    end function matches_glob_pattern

    pure function matches_simple_glob(path, pattern) result(matches)
        character(len=*), intent(in) :: path, pattern
        logical :: matches

        matches = (trim(path) == trim(pattern))
    end function matches_simple_glob

end module fpm_bind_config
