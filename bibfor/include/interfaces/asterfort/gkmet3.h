        interface
          subroutine gkmet3(nnoff,chfond,iadrgk,milieu,connex,iadgks,&
     &iadgki,abscur,num,modele)
            integer :: nnoff
            character(len=24) :: chfond
            integer :: iadrgk
            logical :: milieu
            logical :: connex
            integer :: iadgks
            integer :: iadgki
            character(len=24) :: abscur
            integer :: num
            character(len=8) :: modele
          end subroutine gkmet3
        end interface
