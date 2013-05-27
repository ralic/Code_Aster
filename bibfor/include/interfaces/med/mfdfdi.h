        interface
          subroutine mfdfdi(fid,it,fname,mname,lmesh,type,cname,cunit,&
     &dtunit,nc,cret)
            integer :: fid
            integer :: it
            character(*) :: fname
            character(*) :: mname
            integer :: lmesh
            integer :: type
            character(*) :: cname
            character(*) :: cunit
            character(*) :: dtunit
            integer :: nc
            integer :: cret
          end subroutine mfdfdi
        end interface
