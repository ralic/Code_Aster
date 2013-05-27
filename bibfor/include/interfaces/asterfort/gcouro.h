        interface
          subroutine gcouro(base,resu,noma,nomno,coorn,lobj2,trav1,&
     &trav2,trav3,dir,nomnoe,fond,direc,stok4)
            character(len=1) :: base
            character(len=24) :: resu
            character(len=8) :: noma
            character(len=24) :: nomno
            character(len=24) :: coorn
            integer :: lobj2
            character(len=24) :: trav1
            character(len=24) :: trav2
            character(len=24) :: trav3
            real(kind=8) :: dir(3)
            character(len=8) :: nomnoe(*)
            character(len=8) :: fond
            logical :: direc
            character(len=24) :: stok4
          end subroutine gcouro
        end interface
