        interface
          subroutine pmppr(amat,na1,na2,ka,bmat,nb1,nb2,kb,cmat,nc1,&
     &nc2)
            integer :: nc2
            integer :: nc1
            integer :: nb2
            integer :: nb1
            integer :: na2
            integer :: na1
            real(kind=8) :: amat(na1,na2)
            integer :: ka
            real(kind=8) :: bmat(nb1,nb2)
            integer :: kb
            real(kind=8) :: cmat(nc1,nc2)
          end subroutine pmppr
        end interface
