        interface
          subroutine pj3dtr(cortr3,corres,nutm3d,elrf3d,geom1,geom2)
            character(len=16) :: cortr3
            character(len=16) :: corres
            integer :: nutm3d(10)
            character(len=8) :: elrf3d(10)
            real(kind=8) :: geom1(*)
            real(kind=8) :: geom2(*)
          end subroutine pj3dtr
        end interface
