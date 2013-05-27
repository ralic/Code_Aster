        interface
          subroutine mfdfin(fid,fname,mname,lmesh,type,cname,cunit,&
     &dtunit,nc,cret)
            integer :: fid
            character(*) :: fname
            character(*) :: mname
            integer :: lmesh
            integer :: type
            character(*) :: cname
            character(*) :: cunit
            character(*) :: dtunit
            integer :: nc
            integer :: cret
          end subroutine mfdfin
        end interface
