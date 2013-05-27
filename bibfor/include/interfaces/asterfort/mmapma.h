        interface
          subroutine mmapma(noma,defico,resoco,ndimg,izone,lexfro,&
     &typint,aliase,posmae,nummae,nnomae,posmam,nummam,ksipr1,ksipr2,&
     &tau1m,tau2m,iptm,iptc,norm,nommam)
            character(len=8) :: noma
            character(len=24) :: defico
            character(len=24) :: resoco
            integer :: ndimg
            integer :: izone
            logical :: lexfro
            integer :: typint
            character(len=8) :: aliase
            integer :: posmae
            integer :: nummae
            integer :: nnomae
            integer :: posmam
            integer :: nummam
            real(kind=8) :: ksipr1
            real(kind=8) :: ksipr2
            real(kind=8) :: tau1m(3)
            real(kind=8) :: tau2m(3)
            integer :: iptm
            integer :: iptc
            real(kind=8) :: norm(3)
            character(len=8) :: nommam
          end subroutine mmapma
        end interface
