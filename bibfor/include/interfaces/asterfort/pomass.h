        interface
          subroutine pomass(nomte,e,xnu,rho,kanl,mlv)
            character(*) :: nomte
            real(kind=8) :: e
            real(kind=8) :: xnu
            real(kind=8) :: rho
            integer :: kanl
            real(kind=8) :: mlv(*)
          end subroutine pomass
        end interface
