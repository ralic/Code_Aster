        interface
          subroutine dxefgm(nomte,option,xyzl,pgl,depl,effg)
            character(len=16) :: nomte
            character(len=16) :: option
            real(kind=8) :: xyzl(3,1)
            real(kind=8) :: pgl(3,1)
            real(kind=8) :: depl(1)
            real(kind=8) :: effg(1)
          end subroutine dxefgm
        end interface
