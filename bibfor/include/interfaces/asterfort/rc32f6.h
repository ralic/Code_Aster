        interface
          subroutine rc32f6(nbp12,nbp23,nbp13,nbsigr,nbsg1,nbsg2,nbsg3&
     &,sigr,nocc,saltij)
            integer :: nbp12
            integer :: nbp23
            integer :: nbp13
            integer :: nbsigr
            integer :: nbsg1
            integer :: nbsg2
            integer :: nbsg3
            integer :: sigr(*)
            integer :: nocc(*)
            real(kind=8) :: saltij(*)
          end subroutine rc32f6
        end interface
