        interface
          subroutine cjside(mod,mater,epsd,deps,yd,gd,dy)
            character(len=8) :: mod
            real(kind=8) :: mater(14,2)
            real(kind=8) :: epsd(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: yd(*)
            real(kind=8) :: gd(6)
            real(kind=8) :: dy(*)
          end subroutine cjside
        end interface
