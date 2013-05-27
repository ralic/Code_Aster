        interface
          subroutine mlclow(fid,lname,gtype,sdim,ecoo,swm,nip,ipcoo,&
     &wght,giname,isname,cret)
            integer :: fid
            character(*) :: lname
            integer :: gtype
            integer :: sdim
            real(kind=8) :: ecoo(*)
            integer :: swm
            integer :: nip
            real(kind=8) :: ipcoo(*)
            real(kind=8) :: wght(*)
            character(*) :: giname
            character(*) :: isname
            integer :: cret
          end subroutine mlclow
        end interface
