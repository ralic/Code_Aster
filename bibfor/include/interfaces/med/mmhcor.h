        interface
          subroutine mmhcor(fid,name,numdt,numit,swm,coo,cret)
            integer :: fid
            character(*) :: name
            integer :: numdt
            integer :: numit
            integer :: swm
            real(kind=8) :: coo(*)
            integer :: cret
          end subroutine mmhcor
        end interface
