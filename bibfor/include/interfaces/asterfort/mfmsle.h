        interface
          subroutine mfmsle(fid,iterat,nom,dim,desc,typrep,nocomp,unit&
     &,cret)
            integer :: fid
            integer :: iterat
            character(*) :: nom
            integer :: dim
            character(*) :: desc
            integer :: typrep
            character(len=16) :: nocomp(3)
            character(len=16) :: unit(3)
            integer :: cret
          end subroutine mfmsle
        end interface
