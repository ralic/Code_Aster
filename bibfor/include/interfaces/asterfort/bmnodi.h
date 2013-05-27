        interface
          subroutine bmnodi(basmdz,intfz,nmintz,numint,nbdef,ivcord,&
     &nbdif)
            integer :: nbdef
            character(*) :: basmdz
            character(*) :: intfz
            character(*) :: nmintz
            integer :: numint
            integer :: ivcord(nbdef)
            integer :: nbdif
          end subroutine bmnodi
        end interface
