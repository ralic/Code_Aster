        interface
          subroutine mmhraw(fid,name,numdt,numit,GEOTYPE,aname,n,val,&
     &cret)
            integer :: fid
            character(*) :: name
            integer :: numdt
            integer :: numit
            integer :: GEOTYPE
            character(*) :: aname
            integer :: n
            real(kind=8) :: val(*)
            integer :: cret
          end subroutine mmhraw
        end interface
