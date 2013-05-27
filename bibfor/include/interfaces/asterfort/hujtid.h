        interface
          subroutine hujtid(mod,imat,sigr,vin,dsde,iret)
            character(len=8) :: mod
            integer :: imat
            real(kind=8) :: sigr(6)
            real(kind=8) :: vin(*)
            real(kind=8) :: dsde(6,6)
            integer :: iret
          end subroutine hujtid
        end interface
