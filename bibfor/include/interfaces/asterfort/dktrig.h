        interface
          subroutine dktrig(nomte,xyzl,option,pgl,rig,ener,multic)
            character(len=16) :: nomte
            real(kind=8) :: xyzl(3,*)
            character(len=16) :: option
            real(kind=8) :: pgl(*)
            real(kind=8) :: rig(*)
            real(kind=8) :: ener(*)
            integer :: multic
          end subroutine dktrig
        end interface
