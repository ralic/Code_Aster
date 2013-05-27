        interface
          subroutine pj2dtr(cortr3,corres,nutm2d,elrf2d,geom1,geom2,&
     &lraff)
            character(len=16) :: cortr3
            character(len=16) :: corres
            integer :: nutm2d(6)
            character(len=8) :: elrf2d(6)
            real(kind=8) :: geom1(*)
            real(kind=8) :: geom2(*)
            logical :: lraff
          end subroutine pj2dtr
        end interface
