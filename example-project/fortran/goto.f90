program example_goto
    implicit none
    integer :: i

    ! initialize i
    i = 0

    ! increment i and print it
    10 i = i + 1
    print *, i

    if (i < 10) goto 10 ! if i < 10, repeat
end