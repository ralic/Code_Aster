        interface
          subroutine carapo(sect,geom,orien,xl,pgl,itype,a,xiy,xiz,xjx&
     &,alfay,alfaz,ey,ez,a2,xiy2,xiz2,xjx2,alfay2,alfaz2)
            real(kind=8) :: sect(*)
            real(kind=8) :: geom(6)
            real(kind=8) :: orien(3)
            real(kind=8) :: xl
            real(kind=8) :: pgl(3,3)
            integer :: itype
            real(kind=8) :: a
            real(kind=8) :: xiy
            real(kind=8) :: xiz
            real(kind=8) :: xjx
            real(kind=8) :: alfay
            real(kind=8) :: alfaz
            real(kind=8) :: ey
            real(kind=8) :: ez
            real(kind=8) :: a2
            real(kind=8) :: xiy2
            real(kind=8) :: xiz2
            real(kind=8) :: xjx2
            real(kind=8) :: alfay2
            real(kind=8) :: alfaz2
          end subroutine carapo
        end interface
