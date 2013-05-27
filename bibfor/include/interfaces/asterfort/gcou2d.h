        interface
          subroutine gcou2d(base,resu,noma,nomno,noeud,coor,rinf,rsup,&
     &module,ldirec,dir)
            character(len=1) :: base
            character(len=24) :: resu
            character(len=8) :: noma
            character(len=24) :: nomno
            character(len=8) :: noeud
            real(kind=8) :: coor(*)
            real(kind=8) :: rinf
            real(kind=8) :: rsup
            real(kind=8) :: module
            logical :: ldirec
            real(kind=8) :: dir(3)
          end subroutine gcou2d
        end interface
