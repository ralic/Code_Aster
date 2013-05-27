        interface
          subroutine folocx(vale,n,x,prolgd,i,epsi,coli,ier)
            integer :: n
            real(kind=8) :: vale(n)
            real(kind=8) :: x
            character(*) :: prolgd
            integer :: i
            real(kind=8) :: epsi
            character(len=1) :: coli
            integer :: ier
          end subroutine folocx
        end interface
