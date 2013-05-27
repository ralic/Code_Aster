        interface
          subroutine dxbsig(nomte,xyzl,pgl,sigma,bsigma)
            character(len=16) :: nomte
            real(kind=8) :: xyzl(3,1)
            real(kind=8) :: pgl(3,1)
            real(kind=8) :: sigma(1)
            real(kind=8) :: bsigma(1)
          end subroutine dxbsig
        end interface
