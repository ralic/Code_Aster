        interface
          subroutine discax(noma,nbn,iaxe,nuno,diax)
            integer :: nbn
            character(len=8) :: noma
            integer :: iaxe
            integer :: nuno(nbn)
            real(kind=8) :: diax(nbn)
          end subroutine discax
        end interface
