        interface
          subroutine porigy(nomte,e,rho,xnu,icdmat,klv,nl)
            character(*) :: nomte
            real(kind=8) :: e
            real(kind=8) :: rho
            real(kind=8) :: xnu
            integer :: icdmat
            real(kind=8) :: klv(*)
            integer :: nl
          end subroutine porigy
        end interface
