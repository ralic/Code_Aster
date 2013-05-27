        interface
          subroutine rc32f0(nbsigr,nocc,saltij,saltm,trouve,isk,isl,nk&
     &,nl)
            integer :: nbsigr
            integer :: nocc(*)
            real(kind=8) :: saltij(*)
            real(kind=8) :: saltm
            logical :: trouve
            integer :: isk
            integer :: isl
            integer :: nk
            integer :: nl
          end subroutine rc32f0
        end interface
