        interface
          subroutine hujdp(mod,deps,sigd,sigf,mater,vin,ndec,iret)
            character(len=8) :: mod
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
            real(kind=8) :: mater(22,2)
            real(kind=8) :: vin(*)
            integer :: ndec
            integer :: iret
          end subroutine hujdp
        end interface
