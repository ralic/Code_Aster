        interface
          subroutine lsqpol(ordre,e1,npt,xx,yy,ordok,poly,sigma)
            integer :: npt
            integer :: ordre
            real(kind=8) :: e1
            real(kind=8) :: xx(npt)
            real(kind=8) :: yy(npt)
            integer :: ordok
            real(kind=8) :: poly(ordre+1)
            real(kind=8) :: sigma
          end subroutine lsqpol
        end interface
