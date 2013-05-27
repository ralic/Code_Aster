        interface
          subroutine mmhcow(fid,name,numdt,numit,dt,swm,n,coo,cret)
            integer :: fid
            character(*) :: name
            integer :: numdt
            integer :: numit
            real(kind=8) :: dt
            integer :: swm
            integer :: n
            real(kind=8) :: coo(*)
            integer :: cret
          end subroutine mmhcow
        end interface
