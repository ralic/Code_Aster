        interface
          subroutine mfesar(ifichi,nomail,typgeo,nomatt,nbrval,tabval,&
     &codret)
            integer :: ifichi
            character(*) :: nomail
            integer :: typgeo
            character(*) :: nomatt
            integer :: nbrval
            real(kind=8) :: tabval(*)
            integer :: codret
          end subroutine mfesar
        end interface
