        interface
          subroutine mmhcyr(fid,name,numdt,numit,entype,GEOTYPE,cmode,&
     &swm,con,cret)
            integer :: fid
            character(*) :: name
            integer :: numdt
            integer :: numit
            integer :: entype
            integer :: GEOTYPE
            integer :: cmode
            integer :: swm
            integer :: con(*)
            integer :: cret
          end subroutine mmhcyr
        end interface
