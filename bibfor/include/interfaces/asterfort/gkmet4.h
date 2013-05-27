        interface
          subroutine gkmet4(nnoff,ndimte,chfond,pair,iadrgk,milieu,&
     &connex,iadgks,iadgki,abscur,num)
            integer :: ndimte
            integer :: nnoff
            character(len=24) :: chfond
            logical :: pair
            integer :: iadrgk
            logical :: milieu
            logical :: connex
            integer :: iadgks
            integer :: iadgki
            character(len=24) :: abscur
            integer :: num
          end subroutine gkmet4
        end interface
