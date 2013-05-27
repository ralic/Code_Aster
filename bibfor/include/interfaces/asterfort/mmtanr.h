        interface
          subroutine mmtanr(noma,ndimg,defico,resoco,izone,lexfro,&
     &posnoe,ksi1,ksi2,posmam,nummam,tau1m,tau2m,tau1,tau2)
            character(len=8) :: noma
            integer :: ndimg
            character(len=24) :: defico
            character(len=24) :: resoco
            integer :: izone
            logical :: lexfro
            integer :: posnoe
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            integer :: posmam
            integer :: nummam
            real(kind=8) :: tau1m(3)
            real(kind=8) :: tau2m(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
          end subroutine mmtanr
        end interface
