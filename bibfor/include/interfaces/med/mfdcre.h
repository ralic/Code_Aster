        interface
          subroutine mfdcre(fid,fname,ftype,ncomp,cname,cunit,dtunit,&
     &mname,cret)
            integer :: fid
            character(*) :: fname
            integer :: ftype
            integer :: ncomp
            character(*) :: cname
            character(*) :: cunit
            character(*) :: dtunit
            character(*) :: mname
            integer :: cret
          end subroutine mfdcre
        end interface
