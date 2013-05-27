        interface
          subroutine cjsjis(mod,mater,deps,yd,yf,r,drdy)
            character(len=8) :: mod
            real(kind=8) :: mater(14,2)
            real(kind=8) :: deps(6)
            real(kind=8) :: yd(8)
            real(kind=8) :: yf(8)
            real(kind=8) :: r(8)
            real(kind=8) :: drdy(8,8)
          end subroutine cjsjis
        end interface
