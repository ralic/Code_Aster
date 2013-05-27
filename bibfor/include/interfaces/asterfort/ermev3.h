        interface
          subroutine ermev3(nno,ipg,ivf,isig,nbcmp,dfdx,dfdy,dfdz,dsx,&
     &dsy,dsz,norme)
            integer :: nno
            integer :: ipg
            integer :: ivf
            integer :: isig
            integer :: nbcmp
            real(kind=8) :: dfdx(27)
            real(kind=8) :: dfdy(27)
            real(kind=8) :: dfdz(27)
            real(kind=8) :: dsx
            real(kind=8) :: dsy
            real(kind=8) :: dsz
            real(kind=8) :: norme
          end subroutine ermev3
        end interface
