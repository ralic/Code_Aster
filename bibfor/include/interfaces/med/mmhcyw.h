        interface
          subroutine mmhcyw(fid,name,numdt,numit,dt,entype,GEOTYPE,&
     &cmode,swm,n,con,cret)
            integer :: fid
            character(*) :: name
            integer :: numdt
            integer :: numit
            real(kind=8) :: dt
            integer :: entype
            integer :: GEOTYPE
            integer :: cmode
            integer :: swm
            integer :: n
            integer :: con(*)
            integer :: cret
          end subroutine mmhcyw
        end interface
