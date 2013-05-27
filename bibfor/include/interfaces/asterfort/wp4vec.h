        interface
          subroutine wp4vec(nbfreq,nbvect,neq,shift,vp,vecp,mxresf,&
     &resufi,resufr,lagr,vauc,omecor)
            integer :: mxresf
            integer :: neq
            integer :: nbfreq
            integer :: nbvect
            complex(kind=8) :: shift
            complex(kind=8) :: vp(*)
            complex(kind=8) :: vecp(neq,*)
            integer :: resufi(mxresf,*)
            real(kind=8) :: resufr(mxresf,*)
            integer :: lagr(*)
            complex(kind=8) :: vauc(2*neq,*)
            real(kind=8) :: omecor
          end subroutine wp4vec
        end interface
