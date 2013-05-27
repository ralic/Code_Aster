        interface
          subroutine mfdcsi(fid,fname,it,numdt,numit,dt,cret)
            integer :: fid
            character(*) :: fname
            integer :: it
            integer :: numdt
            integer :: numit
            real(kind=8) :: dt
            integer :: cret
          end subroutine mfdcsi
        end interface
