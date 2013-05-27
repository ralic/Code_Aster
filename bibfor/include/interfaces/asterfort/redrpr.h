        interface
          subroutine redrpr(mod,imate,sigp,vip,dsde,icode)
            character(len=8) :: mod
            integer :: imate
            real(kind=8) :: sigp(*)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsde(6,6)
            integer :: icode
          end subroutine redrpr
        end interface
