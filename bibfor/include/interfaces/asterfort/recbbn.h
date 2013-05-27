        interface
          subroutine recbbn(basmod,nbmod,nbddr,nbdax,tetgd,iord,iorg,&
     &iora,cmode,vecmod,neq,beta)
            integer :: neq
            integer :: nbdax
            integer :: nbddr
            integer :: nbmod
            character(len=8) :: basmod
            character(len=24) :: tetgd
            integer :: iord(nbddr)
            integer :: iorg(nbddr)
            integer :: iora(nbdax)
            complex(kind=8) :: cmode(nbmod+nbddr+nbdax)
            complex(kind=8) :: vecmod(neq)
            real(kind=8) :: beta
          end subroutine recbbn
        end interface
