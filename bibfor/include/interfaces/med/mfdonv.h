        interface
          subroutine mfdonv(fid,fname,numdt,numit,etype,gtype,mname,&
     &pit,stm,pname,psize,lname,nip,n,cret)
            integer :: fid
            character(*) :: fname
            integer :: numdt
            integer :: numit
            integer :: etype
            integer :: gtype
            character(*) :: mname
            integer :: pit
            integer :: stm
            character(*) :: pname
            integer :: psize
            character(*) :: lname
            integer :: nip
            integer :: n
            integer :: cret
          end subroutine mfdonv
        end interface
