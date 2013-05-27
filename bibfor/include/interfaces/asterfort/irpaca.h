        interface
          subroutine irpaca(nomcom,ifi,nbordr,iocc,ordr,nbacc,chacc,&
     &nbchca,chamca,nbk16,nive)
            character(*) :: nomcom
            integer :: ifi
            integer :: nbordr
            integer :: iocc
            integer :: ordr(*)
            integer :: nbacc
            character(*) :: chacc(*)
            integer :: nbchca
            character(*) :: chamca(*)
            integer :: nbk16
            integer :: nive
          end subroutine irpaca
        end interface
