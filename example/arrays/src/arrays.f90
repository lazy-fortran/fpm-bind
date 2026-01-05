module arrays
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

contains

    subroutine dot_product_vec(a, b, n, result)
        integer, intent(in) :: n
        real(dp), intent(in) :: a(n), b(n)
        real(dp), intent(out) :: result
        result = dot_product(a, b)
    end subroutine dot_product_vec

    subroutine scale_array(x, n, factor)
        integer, intent(in) :: n
        real(dp), intent(inout) :: x(n)
        real(dp), intent(in) :: factor
        x = x * factor
    end subroutine scale_array

    subroutine matrix_vector_mult(A, x, y, m, n)
        integer, intent(in) :: m, n
        real(dp), intent(in) :: A(m, n), x(n)
        real(dp), intent(out) :: y(m)
        y = matmul(A, x)
    end subroutine matrix_vector_mult

end module arrays
