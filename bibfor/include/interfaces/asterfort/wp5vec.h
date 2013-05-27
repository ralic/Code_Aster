        interface
          subroutine wp5vec(opt,nbfreq,nbvect,neq,vp,vecp,mxresf,&
     &resufi,resufr,vauc)
            integer :: mxresf
            integer :: neq
            character(*) :: opt
            integer :: nbfreq
            integer :: nbvect
            complex(kind=8) :: vp(*)
            complex(kind=8) :: vecp(neq,*)
            integer :: resufi(mxresf,*)
            real(kind=8) :: resufr(mxresf,*)
            complex(kind=8) :: vauc(2*neq,*)
          end subroutine wp5vec
        end interface
