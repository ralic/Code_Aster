        interface
          subroutine mfdrpr(fid,fname,numdt,numit,etype,gtype,stm,&
     &pname,swm,cs,val,cret)
            integer :: fid
            character(*) :: fname
            integer :: numdt
            integer :: numit
            integer :: etype
            integer :: gtype
            integer :: stm
            character(*) :: pname
            integer :: swm
            integer :: cs
            real(kind=8) :: val(*)
            integer :: cret
          end subroutine mfdrpr
        end interface
