        interface
          subroutine mmsauv(resoco,izone,iptc,nummam,ksipr1,ksipr2,&
     &tau1,tau2,nummae,numnoe,ksipc1,ksipc2,wpc)
            character(len=24) :: resoco
            integer :: izone
            integer :: iptc
            integer :: nummam
            real(kind=8) :: ksipr1
            real(kind=8) :: ksipr2
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            integer :: nummae
            integer :: numnoe
            real(kind=8) :: ksipc1
            real(kind=8) :: ksipc2
            real(kind=8) :: wpc
          end subroutine mmsauv
        end interface
