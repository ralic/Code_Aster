        interface
          subroutine rc36fu(nbsigr,nocc,situ,saltij,nommat,ug,factus)
            integer :: nbsigr
            integer :: nocc(*)
            integer :: situ(*)
            real(kind=8) :: saltij(*)
            character(*) :: nommat
            real(kind=8) :: ug
            real(kind=8) :: factus(*)
          end subroutine rc36fu
        end interface
