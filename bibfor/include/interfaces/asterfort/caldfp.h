        interface
          subroutine caldfp(msns,gamsns,dfpmdg,iret)
            real(kind=8) :: msns(3,3)
            real(kind=8) :: gamsns(3,3)
            real(kind=8) :: dfpmdg(3,3)
            integer :: iret
          end subroutine caldfp
        end interface
