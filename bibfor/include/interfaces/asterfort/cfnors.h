        interface
          subroutine cfnors(noma,defico,resoco,posmai,typent,nument,&
     &lpoutr,lpoint,ksi1,ksi2,lliss,itype,vector,tau1,tau2,lnfixe)
            character(len=8) :: noma
            character(len=24) :: defico
            character(len=24) :: resoco
            integer :: posmai
            character(len=4) :: typent
            integer :: nument
            logical :: lpoutr
            logical :: lpoint
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            logical :: lliss
            integer :: itype
            real(kind=8) :: vector(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            logical :: lnfixe
          end subroutine cfnors
        end interface
