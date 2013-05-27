        interface
          subroutine kpouli(e,a,nx,l0,l1,l2,norml1,norml2,amat)
            real(kind=8) :: e
            real(kind=8) :: a
            real(kind=8) :: nx
            real(kind=8) :: l0
            real(kind=8) :: l1(3)
            real(kind=8) :: l2(3)
            real(kind=8) :: norml1
            real(kind=8) :: norml2
            real(kind=8) :: amat(*)
          end subroutine kpouli
        end interface
