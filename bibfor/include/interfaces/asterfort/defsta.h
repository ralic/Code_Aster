        interface
          subroutine defsta(nmresz,numrfz,raildz,lddl,nocmp,nbfor,&
     &nbdef,tydef,inord)
            integer :: nbdef
            integer :: nbfor
            character(*) :: nmresz
            character(*) :: numrfz
            character(*) :: raildz
            integer :: lddl(nbfor,nbdef)
            character(len=16) :: nocmp(nbdef)
            character(len=16) :: tydef
            integer :: inord
          end subroutine defsta
        end interface
