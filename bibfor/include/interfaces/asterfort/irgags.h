        interface
          subroutine irgags(ncmpmx,nomcmp,nomsym,nbchs,nomchs,nbcmps,&
     &nomgds,ipcmps)
            integer :: ncmpmx
            character(*) :: nomcmp(*)
            character(*) :: nomsym
            integer :: nbchs
            character(*) :: nomchs(*)
            integer :: nbcmps(*)
            character(*) :: nomgds(*)
            integer :: ipcmps(*)
          end subroutine irgags
        end interface
