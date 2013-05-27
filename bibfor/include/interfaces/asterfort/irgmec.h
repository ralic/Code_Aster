        interface
          subroutine irgmec(numold,ima,connex,nbord2,tabd,tabl,tabv,&
     &partie,jtype,nbno,listno,icmp,ifi,iwri,iadmax,ordr,chamsy,nomcon,&
     &lresu)
            integer :: nbord2
            integer :: numold(*)
            integer :: ima
            character(len=24) :: connex
            integer :: tabd(*)
            integer :: tabl(*)
            integer :: tabv(*)
            character(*) :: partie
            integer :: jtype
            integer :: nbno
            integer :: listno(*)
            integer :: icmp
            integer :: ifi
            logical :: iwri
            integer :: iadmax
            integer :: ordr(nbord2)
            character(*) :: chamsy
            character(*) :: nomcon
            logical :: lresu
          end subroutine irgmec
        end interface
