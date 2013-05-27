        interface
          subroutine calcdr(nbmat,mater,parame,derive,g,i,q,devg,&
     &devgii,traceg,dfdl)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: parame(5)
            real(kind=8) :: derive(4)
            real(kind=8) :: g
            real(kind=8) :: i
            real(kind=8) :: q(6)
            real(kind=8) :: devg(6)
            real(kind=8) :: devgii
            real(kind=8) :: traceg
            real(kind=8) :: dfdl
          end subroutine calcdr
        end interface
