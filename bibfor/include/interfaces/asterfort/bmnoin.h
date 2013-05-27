        interface
          subroutine bmnoin(basmdz,intfz,nmintz,numint,nbnoi,numnoe,&
     &nbdif)
            integer :: nbnoi
            character(*) :: basmdz
            character(*) :: intfz
            character(*) :: nmintz
            integer :: numint
            integer :: numnoe(nbnoi)
            integer :: nbdif
          end subroutine bmnoin
        end interface
