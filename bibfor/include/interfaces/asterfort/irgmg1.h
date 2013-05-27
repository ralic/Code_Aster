        interface
          subroutine irgmg1(numold,ima,nbord2,tabd,tabl,tabv,partie,&
     &jtype,nbno,icmp,ifi,iwri,iadmax)
            integer :: numold(*)
            integer :: ima
            integer :: nbord2
            integer :: tabd(*)
            integer :: tabl(*)
            integer :: tabv(*)
            character(*) :: partie
            integer :: jtype
            integer :: nbno
            integer :: icmp
            integer :: ifi
            logical :: iwri
            integer :: iadmax
          end subroutine irgmg1
        end interface
