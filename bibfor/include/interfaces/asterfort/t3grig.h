        interface
          subroutine t3grig(nomte,xyzl,option,pgl,rig,ener)
            character(len=16) :: nomte
            real(kind=8) :: xyzl(3,*)
            character(len=16) :: option
            real(kind=8) :: pgl(*)
            real(kind=8) :: rig(*)
            real(kind=8) :: ener(*)
          end subroutine t3grig
        end interface
