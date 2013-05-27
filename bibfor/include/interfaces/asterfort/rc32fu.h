        interface
          subroutine rc32fu(nbsigr,nocc,situ,fuij,ug,factus)
            integer :: nbsigr
            integer :: nocc(*)
            integer :: situ(*)
            real(kind=8) :: fuij(*)
            real(kind=8) :: ug
            real(kind=8) :: factus(*)
          end subroutine rc32fu
        end interface
