        interface
          subroutine preres(solvez,base,iret,matpre,matass,npvneg,&
     &istop)
            character(*) :: solvez
            character(len=1) :: base
            integer :: iret
            character(*) :: matpre
            character(*) :: matass
            integer :: npvneg
            integer :: istop
          end subroutine preres
        end interface
