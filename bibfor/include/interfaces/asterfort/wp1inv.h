        interface
          subroutine wp1inv(lmasse,lamor,lraide,tolf,nitf,mxresf,&
     &nbfreq,neq,resufi,resufr,resufk,vecpro,solveu)
            integer :: neq
            integer :: mxresf
            integer :: lmasse
            integer :: lamor
            integer :: lraide
            real(kind=8) :: tolf
            integer :: nitf
            integer :: nbfreq
            integer :: resufi(mxresf,*)
            real(kind=8) :: resufr(mxresf,*)
            character(*) :: resufk(mxresf,*)
            complex(kind=8) :: vecpro(neq,*)
            character(len=19) :: solveu
          end subroutine wp1inv
        end interface
