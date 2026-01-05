module derived_types
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    type :: point_t
        real(dp) :: x = 0.0d0
        real(dp) :: y = 0.0d0
    end type point_t

    type :: circle_t
        type(point_t) :: center
        real(dp) :: radius = 1.0d0
    end type circle_t

contains

    subroutine point_init(p, x, y)
        type(point_t), intent(out) :: p
        real(dp), intent(in) :: x, y
        p%x = x
        p%y = y
    end subroutine point_init

    pure function point_distance(p1, p2) result(d)
        type(point_t), intent(in) :: p1, p2
        real(dp) :: d
        d = sqrt((p2%x - p1%x)**2 + (p2%y - p1%y)**2)
    end function point_distance

    subroutine circle_init(c, cx, cy, r)
        type(circle_t), intent(out) :: c
        real(dp), intent(in) :: cx, cy, r
        c%center%x = cx
        c%center%y = cy
        c%radius = r
    end subroutine circle_init

    pure function circle_area(c) result(a)
        type(circle_t), intent(in) :: c
        real(dp) :: a
        real(dp), parameter :: pi = 3.141592653589793d0
        a = pi * c%radius**2
    end function circle_area

end module derived_types
