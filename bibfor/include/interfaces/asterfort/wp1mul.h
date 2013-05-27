        interface
          subroutine wp1mul(lmasse,lamor,lraide,ptorig,tolf,nitf,&
     &nbfreq,mxresf,nprec,resufi,resufr)
            integer :: mxresf
            integer :: lmasse
            integer :: lamor
            integer :: lraide
            complex(kind=8) :: ptorig(3,*)
            real(kind=8) :: tolf
            integer :: nitf
            integer :: nbfreq
            integer :: nprec
            integer :: resufi(mxresf,*)
            real(kind=8) :: resufr(mxresf,*)
          end subroutine wp1mul
        end interface
