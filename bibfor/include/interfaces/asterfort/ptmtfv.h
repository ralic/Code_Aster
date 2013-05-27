        interface
          subroutine ptmtfv(m,rho,e,rof,ce,a1,a2,ai1,ai2,xl,xiy1,xiy2,&
     &xiz1,xiz2,g,alfay1,alfay2,alfaz1,alfaz2,ey,ez,itype,isect)
            real(kind=8) :: m(*)
            real(kind=8) :: rho
            real(kind=8) :: e
            real(kind=8) :: rof
            real(kind=8) :: ce
            real(kind=8) :: a1
            real(kind=8) :: a2
            real(kind=8) :: ai1
            real(kind=8) :: ai2
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
            real(kind=8) :: ey
            real(kind=8) :: ez
            integer :: itype
            integer :: isect
          end subroutine ptmtfv
        end interface
