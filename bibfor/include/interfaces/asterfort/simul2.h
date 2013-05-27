        interface
          subroutine simul2(resu,nomcmd,masse,modsta,nbdir,dir,nomnoe,&
     &nbno)
            character(*) :: resu
            character(*) :: nomcmd
            character(*) :: masse
            character(*) :: modsta
            integer :: nbdir
            real(kind=8) :: dir(*)
            character(*) :: nomnoe(*)
            integer :: nbno
          end subroutine simul2
        end interface
