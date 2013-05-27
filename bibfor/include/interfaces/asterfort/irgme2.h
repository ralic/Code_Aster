        interface
          subroutine irgme2(numold,ima,connex,nbord2,tabd,tabl,tabv,&
     &partie,jtype,nbno,listno,nbcmp,ifi,iadmax)
            integer :: numold(*)
            integer :: ima
            character(len=24) :: connex
            integer :: nbord2
            integer :: tabd(*)
            integer :: tabl(*)
            integer :: tabv(*)
            character(*) :: partie
            integer :: jtype
            integer :: nbno
            integer :: listno(*)
            integer :: nbcmp
            integer :: ifi
            integer :: iadmax
          end subroutine irgme2
        end interface
