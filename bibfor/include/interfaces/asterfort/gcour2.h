        interface
          subroutine gcour2(resu,noma,nomo,nomno,coorn,nbnoeu,trav1,&
     &trav2,trav3,chfond,fond,connex,stok4,thlagr,thlag2,nbre,milieu,&
     &ndimte,pair)
            character(len=8) :: resu
            character(len=8) :: noma
            character(len=8) :: nomo
            character(len=24) :: nomno
            character(len=24) :: coorn
            integer :: nbnoeu
            character(len=24) :: trav1
            character(len=24) :: trav2
            character(len=24) :: trav3
            character(len=24) :: chfond
            character(len=8) :: fond
            logical :: connex
            character(len=24) :: stok4
            logical :: thlagr
            logical :: thlag2
            integer :: nbre
            logical :: milieu
            integer :: ndimte
            logical :: pair
          end subroutine gcour2
        end interface
