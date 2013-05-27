        interface
          subroutine cfchno(noma,defico,ndimg,posnoe,typenm,numenm,&
     &lmait,lescl,lmfixe,lefixe,tau1m,tau2m,tau1e,tau2e,tau1,tau2)
            character(len=8) :: noma
            character(len=24) :: defico
            integer :: ndimg
            integer :: posnoe
            character(len=4) :: typenm
            integer :: numenm
            logical :: lmait
            logical :: lescl
            logical :: lmfixe
            logical :: lefixe
            real(kind=8) :: tau1m(3)
            real(kind=8) :: tau2m(3)
            real(kind=8) :: tau1e(3)
            real(kind=8) :: tau2e(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
          end subroutine cfchno
        end interface
