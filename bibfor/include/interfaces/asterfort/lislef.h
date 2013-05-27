        interface
          subroutine lislef(motfac,iexci,nomfct,typfct,phase,npuis)
            character(len=16) :: motfac
            integer :: iexci
            character(len=8) :: nomfct
            character(len=16) :: typfct
            real(kind=8) :: phase
            integer :: npuis
          end subroutine lislef
        end interface
