        interface
          subroutine poefgr(nomte,klc,mater,e,xnu,rho,effo)
            character(*) :: nomte
            real(kind=8) :: klc(12,12)
            integer :: mater
            real(kind=8) :: e
            real(kind=8) :: xnu
            real(kind=8) :: rho
            real(kind=8) :: effo(*)
          end subroutine poefgr
        end interface
