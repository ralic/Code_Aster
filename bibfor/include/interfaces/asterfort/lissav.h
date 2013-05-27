        interface
          subroutine lissav(lischa,ichar,charge,typech,codcha,prefob,&
     &typapp,nomfct,typfct,phase,npuis)
            character(len=19) :: lischa
            integer :: ichar
            character(len=8) :: charge
            character(len=8) :: typech
            integer :: codcha
            character(len=13) :: prefob
            character(len=16) :: typapp
            character(len=8) :: nomfct
            character(len=16) :: typfct
            real(kind=8) :: phase
            integer :: npuis
          end subroutine lissav
        end interface
