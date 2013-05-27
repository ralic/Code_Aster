        interface
          subroutine newacc(neq,c1,c2,c3,d0,v0,a0,d1,a1)
            integer :: neq
            real(kind=8) :: c1
            real(kind=8) :: c2
            real(kind=8) :: c3
            real(kind=8) :: d0(*)
            real(kind=8) :: v0(*)
            real(kind=8) :: a0(*)
            real(kind=8) :: d1(*)
            real(kind=8) :: a1(*)
          end subroutine newacc
        end interface
