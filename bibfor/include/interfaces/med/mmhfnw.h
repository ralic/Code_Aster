        interface
          subroutine mmhfnw(fid,name,numdt,numit,entype,GEOTYPE,n,num,&
     &cret)
            integer :: fid
            character(*) :: name
            integer :: numdt
            integer :: numit
            integer :: entype
            integer :: GEOTYPE
            integer :: n
            integer :: num(*)
            integer :: cret
          end subroutine mmhfnw
        end interface
