        interface
          subroutine noligr(ligrz,igrel,numel,nb,li,lk,code,irepe,&
     &inema,nbno,typlaz)
            character(*) :: ligrz
            integer :: igrel
            integer :: numel
            integer :: nb
            integer :: li(*)
            character(*) :: lk(*)
            integer :: code
            integer :: irepe
            integer :: inema
            integer :: nbno(*)
            character(*) :: typlaz
          end subroutine noligr
        end interface
