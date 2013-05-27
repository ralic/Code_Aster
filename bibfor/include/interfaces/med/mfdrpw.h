        interface
          subroutine mfdrpw(fid,fname,numdt,numit,dt,etype,gtype,stm,&
     &pname,lname,swm,cs,n,val,cret)
            integer :: fid
            character(*) :: fname
            integer :: numdt
            integer :: numit
            real(kind=8) :: dt
            integer :: etype
            integer :: gtype
            integer :: stm
            character(*) :: pname
            character(*) :: lname
            integer :: swm
            integer :: cs
            integer :: n
            real(kind=8) :: val(*)
            integer :: cret
          end subroutine mfdrpw
        end interface
