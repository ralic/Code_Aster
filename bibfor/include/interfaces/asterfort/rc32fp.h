        interface
          subroutine rc32fp(nbsigr,nocc,situ,sigr,fuij,ug,factus)
            integer :: nbsigr
            integer :: nocc(*)
            integer :: situ(*)
            integer :: sigr(*)
            real(kind=8) :: fuij(*)
            real(kind=8) :: ug
            real(kind=8) :: factus(*)
          end subroutine rc32fp
        end interface
