        interface
          subroutine wp3vec(appr,opt,nbfreq,nbvect,neq,shift,vpr,vpi,&
     &vecp,mxresf,resufi,resufr,lagr,vauc,omecor)
            integer :: mxresf
            integer :: neq
            character(len=1) :: appr
            character(*) :: opt
            integer :: nbfreq
            integer :: nbvect
            complex(kind=8) :: shift
            real(kind=8) :: vpr(*)
            real(kind=8) :: vpi(*)
            complex(kind=8) :: vecp(neq,*)
            integer :: resufi(mxresf,*)
            real(kind=8) :: resufr(mxresf,*)
            integer :: lagr(*)
            complex(kind=8) :: vauc(2*neq,*)
            real(kind=8) :: omecor
          end subroutine wp3vec
        end interface
