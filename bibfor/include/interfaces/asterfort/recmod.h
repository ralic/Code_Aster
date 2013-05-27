        interface
          subroutine recmod(modmec,nbmode,nbamor,bande,tymmec,grdmod)
            character(len=8) :: modmec
            integer :: nbmode
            integer :: nbamor
            real(kind=8) :: bande(2)
            character(len=8) :: tymmec
            character(len=16) :: grdmod
          end subroutine recmod
        end interface
