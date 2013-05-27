        interface
          subroutine edgani(dim,y,pm,dvsitr,eqsitr,mu,ani,gamma,m,n,g,&
     &maxg,dgdy)
            integer :: dim
            real(kind=8) :: y(dim)
            real(kind=8) :: pm
            real(kind=8) :: dvsitr(dim-1)
            real(kind=8) :: eqsitr
            real(kind=8) :: mu
            real(kind=8) :: ani(6,6)
            real(kind=8) :: gamma(3)
            real(kind=8) :: m(3)
            real(kind=8) :: n(3)
            real(kind=8) :: g(dim)
            real(kind=8) :: maxg
            real(kind=8) :: dgdy(dim,dim)
          end subroutine edgani
        end interface
