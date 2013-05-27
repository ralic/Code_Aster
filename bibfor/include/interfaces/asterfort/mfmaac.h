        interface
          subroutine mfmaac(fid,nom,dim,type,desc,descdt,typrep,nocomp&
     &,unit,cret)
            integer :: fid
            character(*) :: nom
            integer :: dim
            integer :: type
            character(*) :: desc
            character(*) :: descdt
            integer :: typrep
            character(len=16) :: nocomp(3)
            character(len=16) :: unit(3)
            integer :: cret
          end subroutine mfmaac
        end interface
