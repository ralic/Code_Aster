        interface
          subroutine vdefge(nomte,nb1,npgsr,xr,epais,sigma,effgt)
            character(len=16) :: nomte
            integer :: nb1
            integer :: npgsr
            real(kind=8) :: xr(*)
            real(kind=8) :: epais
            real(kind=8) :: sigma(6,*)
            real(kind=8) :: effgt(8,*)
          end subroutine vdefge
        end interface
