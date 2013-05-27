        interface
          subroutine pogyro(nomte,rho,xnu,icdmat,klv,nl)
            character(*) :: nomte
            real(kind=8) :: rho
            real(kind=8) :: xnu
            integer :: icdmat
            real(kind=8) :: klv(*)
            integer :: nl
          end subroutine pogyro
        end interface
