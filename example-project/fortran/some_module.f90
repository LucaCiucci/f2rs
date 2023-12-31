

module some_module
    public :: some_subroutine
    contains
    subroutine some_subroutine(i)
        implicit none
        !integer, intent(in) :: i
        integer :: i
        print *, "Hello from some_sub", i
    end subroutine

    double precision function some_function(x)
        implicit none
        double precision :: x
        some_function = x**2
    end function some_function
end module