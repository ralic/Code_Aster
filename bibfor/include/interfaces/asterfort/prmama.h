        interface
          subroutine prmama(iprod,amat,na,na1,na2,bmat,nb,nb1,nb2,cmat&
     &,nc,nc1,nc2,ier)
            integer :: nc
            integer :: nb
            integer :: na
            integer :: iprod
            real(kind=8) :: amat(na,*)
            integer :: na1
            integer :: na2
            real(kind=8) :: bmat(nb,*)
            integer :: nb1
            integer :: nb2
            real(kind=8) :: cmat(nc,*)
            integer :: nc1
            integer :: nc2
            integer :: ier
          end subroutine prmama
        end interface
