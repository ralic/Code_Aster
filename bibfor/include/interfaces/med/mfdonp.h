        interface
          subroutine mfdonp(fid,fname,numdt,numit,etype,gtype,it,mname&
     &,dpname,dlname,n,cret)
            integer :: fid
            character(*) :: fname
            integer :: numdt
            integer :: numit
            integer :: etype
            integer :: gtype
            integer :: it
            character(*) :: mname
            character(*) :: dpname
            character(*) :: dlname
            integer :: n
            integer :: cret
          end subroutine mfdonp
        end interface
