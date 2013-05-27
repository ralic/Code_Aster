        interface
          subroutine ptktfv(itype,sk,e,rof,ce,a1,ai1,a2,ai2,xl,xiy1,&
     &xiy2,xiz1,xiz2,xjx1,xjx2,g,alfay1,alfay2,alfaz1,alfaz2,ey,ez)
            integer :: itype
            real(kind=8) :: sk(*)
            real(kind=8) :: e
            real(kind=8) :: rof
            real(kind=8) :: ce
            real(kind=8) :: a1
            real(kind=8) :: ai1
            real(kind=8) :: a2
            real(kind=8) :: ai2
            real(kind=8) :: xl
            real(kind=8) :: xiy1
            real(kind=8) :: xiy2
            real(kind=8) :: xiz1
            real(kind=8) :: xiz2
            real(kind=8) :: xjx1
            real(kind=8) :: xjx2
            real(kind=8) :: g
            real(kind=8) :: alfay1
            real(kind=8) :: alfay2
            real(kind=8) :: alfaz1
            real(kind=8) :: alfaz2
            real(kind=8) :: ey
            real(kind=8) :: ez
          end subroutine ptktfv
        end interface
