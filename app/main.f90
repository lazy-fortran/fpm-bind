program fpm_bind_main
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit, stdout => output_unit
    use fpm_bind_config, only: project_config_t, bind_config_t, &
                                read_fpm_toml, read_bind_toml
    use fpm_bind_python, only: generate_python_bindings, build_python_bindings, &
                                install_python_bindings
    implicit none

    character(len=256) :: arg, project_dir
    character(len=:), allocatable :: error, lang
    type(project_config_t) :: project
    type(bind_config_t) :: bind_cfg
    integer :: nargs, i
    logical :: do_build, do_install, show_help

    nargs = command_argument_count()
    do_build = .false.
    do_install = .false.
    show_help = .false.
    lang = ""

    call getcwd(project_dir)

    do i = 1, nargs
        call get_command_argument(i, arg)
        select case (trim(arg))
        case ("python")
            lang = "python"
        case ("--build", "-b")
            do_build = .true.
        case ("--install", "-i")
            do_install = .true.
        case ("--help", "-h")
            show_help = .true.
        case default
            if (arg(1:1) /= "-") then
                if (len_trim(lang) == 0) then
                    lang = trim(arg)
                end if
            end if
        end select
    end do

    if (show_help .or. nargs == 0) then
        call print_help()
        stop
    end if

    if (len_trim(lang) == 0) then
        write(stderr, '(A)') "Error: No target language specified"
        write(stderr, '(A)') "Usage: fpm-bind <language> [--build] [--install]"
        write(stderr, '(A)') "Example: fpm-bind python --build"
        stop 1
    end if

    call read_fpm_toml(trim(project_dir), project, error)
    if (allocated(error)) then
        write(stderr, '(A)') "Error: " // error
        stop 1
    end if

    write(stderr, '(A)') "Project: " // project%name

    call read_bind_toml(trim(project_dir), bind_cfg, error)
    if (allocated(error)) then
        write(stderr, '(A)') "Error: " // error
        stop 1
    end if

    select case (lang)
    case ("python")
        if (.not. bind_cfg%python_enabled) then
            write(stderr, '(A)') "Error: Python bindings not enabled in bind.toml"
            stop 1
        end if

        call generate_python_bindings(project, bind_cfg, do_build .or. do_install, error)
        if (allocated(error)) then
            write(stderr, '(A)') "Error: " // error
            stop 1
        end if

        if (do_install) then
            call install_python_bindings(project, bind_cfg, error)
            if (allocated(error)) then
                write(stderr, '(A)') "Error: " // error
                stop 1
            end if
        end if

    case default
        write(stderr, '(A)') "Error: Unsupported language: " // lang
        write(stderr, '(A)') "Supported languages: python"
        stop 1
    end select

contains

    subroutine print_help()
        write(stdout, '(A)') "fpm-bind - FPM plugin for language bindings"
        write(stdout, '(A)') ""
        write(stdout, '(A)') "Usage: fpm-bind <language> [options]"
        write(stdout, '(A)') "       fpm bind <language> [options]  (via fpm plugin)"
        write(stdout, '(A)') ""
        write(stdout, '(A)') "Languages:"
        write(stdout, '(A)') "  python    Generate Python bindings via f90wrap"
        write(stdout, '(A)') ""
        write(stdout, '(A)') "Options:"
        write(stdout, '(A)') "  --build, -b     Generate and compile bindings"
        write(stdout, '(A)') "  --install, -i   Generate, compile, and pip install"
        write(stdout, '(A)') "  --help, -h      Show this help"
        write(stdout, '(A)') ""
        write(stdout, '(A)') "Configuration:"
        write(stdout, '(A)') "  Place bind.toml in project root with:"
        write(stdout, '(A)') ""
        write(stdout, '(A)') "    [python]"
        write(stdout, '(A)') "    package-name = ""mylib""  # optional"
        write(stdout, '(A)') "    direct-c = true          # default, no f2py dep"
        write(stdout, '(A)') ""
        write(stdout, '(A)') "  Selective binding (include/exclude patterns):"
        write(stdout, '(A)') ""
        write(stdout, '(A)') "    [python]"
        write(stdout, '(A)') "    include = [""src/field/*.f90"", ""src/transport/*.f90""]"
        write(stdout, '(A)') "    exclude = [""**/*_merged.f90"", ""**/test_*.f90""]"
        write(stdout, '(A)') ""
        write(stdout, '(A)') "  Glob patterns:"
        write(stdout, '(A)') "    *   matches any chars except /"
        write(stdout, '(A)') "    **  matches any chars including /"
        write(stdout, '(A)') ""
        write(stdout, '(A)') "Examples:"
        write(stdout, '(A)') "  fpm-bind python            # Generate only"
        write(stdout, '(A)') "  fpm-bind python --build    # Generate and compile"
        write(stdout, '(A)') "  fpm-bind python --install  # Full pipeline"
    end subroutine print_help

end program fpm_bind_main
