        interface
          subroutine rc36f1(nbsigr,nocc,saltij,isk,isl,nk,nl,n0,nbp12,&
     &nbp23,nbp13,sigr,yapass,typass,nsitup)
            integer :: nbsigr
            integer :: nocc(*)
            real(kind=8) :: saltij(*)
            integer :: isk
            integer :: isl
            integer :: nk
            integer :: nl
            integer :: n0
            integer :: nbp12
            integer :: nbp23
            integer :: nbp13
            integer :: sigr(*)
            logical :: yapass
            character(len=3) :: typass
            integer :: nsitup
          end subroutine rc36f1
        end interface
