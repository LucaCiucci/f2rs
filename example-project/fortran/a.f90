! comment
program my_program
    DO 2 k=1,5 
        a=a+kmul(i,k)*arg(i) 
    2 print *,a
end program my_program

type my_type
    integer :: i
    real :: arg(5)
    real :: kmul(5,5)
end type my_type

double precision, parameter :: a = 0