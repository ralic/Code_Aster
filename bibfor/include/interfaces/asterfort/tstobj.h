        interface
          subroutine tstobj(ob,perm,resume,sommi,sommr,lonuti,lonmax,&
     &type,iret,ni)
            character(*) :: ob
            character(*) :: perm
            integer :: resume
            integer :: sommi
            real(kind=8) :: sommr
            integer :: lonuti
            integer :: lonmax
            character(len=3) :: type
            integer :: iret
            integer :: ni
          end subroutine tstobj
        end interface
