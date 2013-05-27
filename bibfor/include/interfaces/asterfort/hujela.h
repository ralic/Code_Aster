        interface
          subroutine hujela(mod,crit,mater,deps,sigd,sigf,iret)
            character(len=8) :: mod
            real(kind=8) :: crit(*)
            real(kind=8) :: mater(22,2)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
            integer :: iret
          end subroutine hujela
        end interface
