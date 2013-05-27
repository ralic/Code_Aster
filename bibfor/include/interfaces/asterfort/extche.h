        interface
          subroutine extche(nchme2,nmaile,nummai,ncmp,nbm,nbc,indic,&
     &nssche,mcf,iocc,nbnac,nnoeud)
            character(len=19) :: nchme2
            character(len=8) :: nmaile(*)
            integer :: nummai(*)
            character(len=8) :: ncmp(*)
            integer :: nbm
            integer :: nbc
            character(len=6) :: indic
            character(len=19) :: nssche
            character(*) :: mcf
            integer :: iocc
            integer :: nbnac
            integer :: nnoeud(*)
          end subroutine extche
        end interface
