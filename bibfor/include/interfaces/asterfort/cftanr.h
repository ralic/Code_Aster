        interface
          subroutine cftanr(noma,ndimg,defico,resoco,izone,posnoe,&
     &typenm,posenm,numenm,ksipr1,ksipr2,tau1m,tau2m,tau1,tau2)
            character(len=8) :: noma
            integer :: ndimg
            character(len=24) :: defico
            character(len=24) :: resoco
            integer :: izone
            integer :: posnoe
            character(len=4) :: typenm
            integer :: posenm
            integer :: numenm
            real(kind=8) :: ksipr1
            real(kind=8) :: ksipr2
            real(kind=8) :: tau1m(3)
            real(kind=8) :: tau2m(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
          end subroutine cftanr
        end interface
