module simple
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

contains

    pure function add(a, b) result(c)
        real(dp), intent(in) :: a, b
        real(dp) :: c
        c = a + b
    end function add

    pure function multiply(a, b) result(c)
        real(dp), intent(in) :: a, b
        real(dp) :: c
        c = a * b
    end function multiply

    subroutine greet(name, greeting)
        character(len=*), intent(in) :: name
        character(len=256), intent(out) :: greeting
        greeting = "Hello, " // trim(name) // "!"
    end subroutine greet

end module simple
