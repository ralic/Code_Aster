        interface
          subroutine newvit(neq,c1,c2,v0,a0,v1,a1)
            integer :: neq
            real(kind=8) :: c1
            real(kind=8) :: c2
            real(kind=8) :: v0(*)
            real(kind=8) :: a0(*)
            real(kind=8) :: v1(*)
            real(kind=8) :: a1(*)
          end subroutine newvit
        end interface
