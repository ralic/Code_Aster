        interface
          subroutine cfnord(noma,typent,nument,itype,vector,tau1,tau2,&
     &lnfixe)
            character(len=8) :: noma
            character(len=4) :: typent
            integer :: nument
            integer :: itype
            real(kind=8) :: vector(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            logical :: lnfixe
          end subroutine cfnord
        end interface
