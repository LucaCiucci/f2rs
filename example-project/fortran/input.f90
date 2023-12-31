! comment
program my_program
    !implicit none
    !use some_module, ONLY : a, b, c
    !use some_module2
    use some_module
    use iso_c_binding, only : c_int, ci => c_int
    implicit none

    !1 format (i2, 1x, f4.2)
    1 format (i2, 1x, f4)

    integer i, a, b, c, d, e, f
    logical :: flag1, flag2
    double precision :: df
    integer(c_int) :: cc
    integer(ci) :: cci
    integer*2 :: c2, c22
    integer(4) :: c3 = 42
    double complex :: z
    double precision, parameter :: pi = 3.14
    integer, parameter :: dkind = KIND(1.0d0)
    real(kind=dkind) :: r
    !integer :: some_vec(:, 2, 3), some_vec2(2, 3)
    !type(integer) iii
    !character*4 :: str
    !integer dsnjf(1:10)

    cci = -2

    a + b * c - d / e ** f + f

    do 3 i = 1, 10
        if (i == 5) then
            print *, "i is 5"
        else
            print *, "i is not 5"
        end if
    
    3 end do

    DO k=1,5 
        a=a+kmul(i,k)*arg(i) 
    END DO

    DO 2 q=1,5 
        a=a+kmul(i,k)*arg(i) 
    2 END DO

    DO 3 l=1,5 
        a=a+kmul(i,k)*arg(i) 
    END DO

    call some_subroutine(i)

    ! comment 2
end program my_program