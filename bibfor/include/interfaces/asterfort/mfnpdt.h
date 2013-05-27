        interface
          subroutine mfnpdt(fid,cha,ma,n,cunit,cname,cret)
            integer :: fid
            character(*) :: cha
            character(*) :: ma
            integer :: n
            character(len=16) :: cunit(*)
            character(len=16) :: cname(*)
            integer :: cret
          end subroutine mfnpdt
        end interface
