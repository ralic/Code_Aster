        interface
          subroutine getcon(nomres,iob,ishf,ilng,ctype,lcon,iadvar,&
     &nomob)
            character(*) :: nomres
            integer :: iob
            integer :: ishf
            integer :: ilng
            integer :: ctype
            integer :: lcon
            integer :: iadvar
            character(len=24) :: nomob
          end subroutine getcon
        end interface
