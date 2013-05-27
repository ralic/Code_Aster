        interface
          subroutine majour(neq,lgrot,lendo,sdnume,chaini,chadel,coef,&
     &chamaj,ordre)
            integer :: neq
            logical :: lgrot
            logical :: lendo
            character(len=19) :: sdnume
            real(kind=8) :: chaini(*)
            real(kind=8) :: chadel(*)
            real(kind=8) :: coef
            real(kind=8) :: chamaj(*)
            integer :: ordre
          end subroutine majour
        end interface
