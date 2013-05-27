        interface
          subroutine hujksi(carac,mater,r,ksi,iret)
            character(len=6) :: carac
            real(kind=8) :: mater(22,2)
            real(kind=8) :: r
            real(kind=8) :: ksi
            integer :: iret
          end subroutine hujksi
        end interface
