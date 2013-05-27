        interface
          subroutine mlclor(fid,lname,swm,ecoo,ipcoo,wght,cret)
            integer :: fid
            character(*) :: lname
            integer :: swm
            real(kind=8) :: ecoo(*)
            real(kind=8) :: ipcoo(*)
            real(kind=8) :: wght(*)
            integer :: cret
          end subroutine mlclor
        end interface
