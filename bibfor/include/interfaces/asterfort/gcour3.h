        interface
          subroutine gcour3(resu,noma,coorn,lnoff,trav1,trav2,trav3,&
     &chfond,grlt,thlagr,thlag2,basfon,nbre,milieu,pair,ndimte)
            character(len=8) :: resu
            character(len=8) :: noma
            character(len=24) :: coorn
            integer :: lnoff
            character(len=24) :: trav1
            character(len=24) :: trav2
            character(len=24) :: trav3
            character(len=24) :: chfond
            character(len=19) :: grlt
            logical :: thlagr
            logical :: thlag2
            character(len=24) :: basfon
            integer :: nbre
            logical :: milieu
            logical :: pair
            integer :: ndimte
          end subroutine gcour3
        end interface
