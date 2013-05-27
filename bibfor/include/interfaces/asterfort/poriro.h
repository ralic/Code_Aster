        interface
          subroutine poriro(itype,m,rho,omega,e,a1,a2,xl,xiy1,xiy2,&
     &xiz1,xiz2,g,alfay1,alfay2,alfaz1,alfaz2)
            integer :: itype
            real(kind=8) :: m(*)
            real(kind=8) :: rho
            real(kind=8) :: omega(3)
            real(kind=8) :: e
            real(kind=8) :: a1
            real(kind=8) :: a2
            real(kind=8) :: xl
            real(kind=8) :: xiy1
            real(kind=8) :: xiy2
            real(kind=8) :: xiz1
            real(kind=8) :: xiz2
            real(kind=8) :: g
            real(kind=8) :: alfay1
            real(kind=8) :: alfay2
            real(kind=8) :: alfaz1
            real(kind=8) :: alfaz2
          end subroutine poriro
        end interface
