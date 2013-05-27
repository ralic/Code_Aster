        interface
          subroutine focrch(nomfon,resu,noeud,parax,paray,base,int,&
     &intitu,ind,listr,sst,nsst,ier)
            character(len=19) :: nomfon
            character(len=19) :: resu
            character(len=8) :: noeud
            character(len=16) :: parax
            character(len=16) :: paray
            character(len=1) :: base
            integer :: int
            character(len=8) :: intitu
            integer :: ind
            character(len=19) :: listr
            character(len=8) :: sst
            integer :: nsst
            integer :: ier
          end subroutine focrch
        end interface
