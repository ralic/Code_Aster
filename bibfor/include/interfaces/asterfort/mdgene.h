        interface
          subroutine mdgene(basemo,nbmode,numgen,masgen,riggen,amogen,&
     &nexcit,jvec,ier)
            character(len=8) :: basemo
            integer :: nbmode
            character(len=14) :: numgen
            character(len=8) :: masgen
            character(len=8) :: riggen
            character(len=8) :: amogen
            integer :: nexcit
            integer :: jvec
            integer :: ier
          end subroutine mdgene
        end interface
