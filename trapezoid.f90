program trapezoid
implicit none
real :: a,b,sumi,intg,func,h
integer ::i,n
external func
write(*,*) "Give the value of lower and upper limit "
read(*,*) a,b
write(*,*) "Give the no of divisions you want for the calculation"
read(*,*) n
h=(b-a)/n
sumi=0.0
do i = 1,n
sumi=(sumi+func(a+i*h))
end do
intg=h*(func(a)+func(b)+2*sumi)/2
write (*,*) "The final integrated result is",intg
end program
real function func(x)
func=exp(x)
end function

