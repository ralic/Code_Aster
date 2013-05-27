        interface
          subroutine vdxsig(nomte,option,xi,nb1,npgsr,sigmpg,effgt)
            character(len=16) :: nomte
            character(*) :: option
            real(kind=8) :: xi(3,9)
            integer :: nb1
            integer :: npgsr
            real(kind=8) :: sigmpg(*)
            real(kind=8) :: effgt(8,9)
          end subroutine vdxsig
        end interface
