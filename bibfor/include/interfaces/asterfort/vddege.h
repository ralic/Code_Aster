        interface
          subroutine vddege(nomte,nb1,npgsr,xr,deggtg,deggt)
            character(len=16) :: nomte
            integer :: nb1
            integer :: npgsr
            real(kind=8) :: xr(*)
            real(kind=8) :: deggtg(8,*)
            real(kind=8) :: deggt(8,9)
          end subroutine vddege
        end interface
