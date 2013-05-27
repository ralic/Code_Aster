        interface
          subroutine sndbg(ifm,iclass,ival,rval,kval)
            integer :: ifm
            integer :: iclass
            integer :: ival
            real(kind=8) :: rval(*)
            character(*) :: kval
          end subroutine sndbg
        end interface
