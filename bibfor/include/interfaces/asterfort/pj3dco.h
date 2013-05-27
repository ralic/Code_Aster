        interface
          subroutine pj3dco(mocle,moa1,moa2,nbma1,lima1,nbno2,lino2,&
     &geom1,geom2,corres,ldmax,distma)
            character(*) :: mocle
            character(len=8) :: moa1
            character(len=8) :: moa2
            integer :: nbma1
            integer :: lima1(*)
            integer :: nbno2
            integer :: lino2(*)
            character(*) :: geom1
            character(*) :: geom2
            character(len=16) :: corres
            logical :: ldmax
            real(kind=8) :: distma
          end subroutine pj3dco
        end interface
