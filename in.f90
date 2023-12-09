! comment
program my_program
    !implicit none
    use some_module, ONLY : a, b, c
    use some_module2
    implicit none

    !1 format (i2, 1x, f4.2)
    1 format (i2, 1x, f4)

    integer i
    logical :: flag1, flag2
    double precision :: d
    integer(c_int) :: c
    integer*2 :: c2
    integer(4) :: c3
    double complex :: z
    integer :: some_vec(:, 2, 3), some_vec2(2, 3)
    type(integer) iii
    character*4 :: str
    integer dsnjf(1:10)

    -2

    a + b * c - d / e ** f + f(2)

    do i = 1, 10
        if (i == 5) then
            print *, "i is 5"
        else
            print *, "i is not 5"
        end if
    end do

    call some_subroutine(a, b, c, 3)

    ! comment 2
end program my_program