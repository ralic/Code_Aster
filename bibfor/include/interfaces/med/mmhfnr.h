        interface
          subroutine mmhfnr(fid,name,numdt,numit,entype,GEOTYPE,num,&
     &cret)
            integer :: fid
            character(*) :: name
            integer :: numdt
            integer :: numit
            integer :: entype
            integer :: GEOTYPE
            integer :: num(*)
            integer :: cret
          end subroutine mmhfnr
        end interface
