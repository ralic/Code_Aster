        interface
          subroutine rc36fp(nbsigr,nocc,situ,sigr,saltij,nommat,ug,&
     &factus)
            integer :: nbsigr
            integer :: nocc(*)
            integer :: situ(*)
            integer :: sigr(*)
            real(kind=8) :: saltij(*)
            character(*) :: nommat
            real(kind=8) :: ug
            real(kind=8) :: factus(*)
          end subroutine rc36fp
        end interface
