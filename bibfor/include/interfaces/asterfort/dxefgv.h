        interface
          subroutine dxefgv(nomte,option,xyzl,pgl,depl,effgt)
            character(len=16) :: nomte
            character(*) :: option
            real(kind=8) :: xyzl(3,1)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: depl(1)
            real(kind=8) :: effgt(1)
          end subroutine dxefgv
        end interface
