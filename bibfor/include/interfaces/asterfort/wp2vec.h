        interface
          subroutine wp2vec(appr,opt,nbfreq,nbvect,neq,shift,yh,yb,vr,&
     &nlivr,vpr,vpi,vecp,mxresf,resufi,resufr,lagr,omecor)
            integer :: mxresf
            integer :: nlivr
            integer :: neq
            character(len=1) :: appr
            character(*) :: opt
            integer :: nbfreq
            integer :: nbvect
            complex(kind=8) :: shift
            real(kind=8) :: yh(neq,*)
            real(kind=8) :: yb(neq,*)
            real(kind=8) :: vr(nlivr,*)
            real(kind=8) :: vpr(*)
            real(kind=8) :: vpi(*)
            complex(kind=8) :: vecp(neq,*)
            integer :: resufi(mxresf,*)
            real(kind=8) :: resufr(mxresf,*)
            integer :: lagr(*)
            real(kind=8) :: omecor
          end subroutine wp2vec
        end interface
