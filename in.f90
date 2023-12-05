! comment
program my_program
    !implicit none
    use some_module

    integer i
    logical :: flag1, flag2
    double precision :: d
    integer(c_int) :: c
    integer :: some_vec(:, 2, 3), some_vec2(2, 3)

    do i = 1, 10
        if (i == 5) then
            print *, "i is 5"
        else
            print *, "i is not 5"
        end if
    end do

    ! comment 2
end program my_program