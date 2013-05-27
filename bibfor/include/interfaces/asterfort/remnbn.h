        interface
          subroutine remnbn(basmod,nbmod,nbddr,nbdax,flexdr,flexga,&
     &flexax,tetgd,tetax,cmode,vecmod,neq,beta)
            integer :: neq
            integer :: nbdax
            integer :: nbddr
            integer :: nbmod
            character(len=8) :: basmod
            character(len=24) :: flexdr
            character(len=24) :: flexga
            character(len=24) :: flexax
            character(len=24) :: tetgd
            character(len=24) :: tetax
            complex(kind=8) :: cmode(nbmod+nbddr+nbdax)
            complex(kind=8) :: vecmod(neq)
            real(kind=8) :: beta
          end subroutine remnbn
        end interface
