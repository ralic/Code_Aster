        interface
          subroutine dkqmas(xyzl,option,pgl,mas,ener)
            real(kind=8) :: xyzl(3,*)
            character(len=16) :: option
            real(kind=8) :: pgl(*)
            real(kind=8) :: mas(*)
            real(kind=8) :: ener(*)
          end subroutine dkqmas
        end interface
