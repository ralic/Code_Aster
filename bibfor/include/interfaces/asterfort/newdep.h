        interface
          subroutine newdep(neq,c,dt,d0,v0,a0,d1,a1)
            integer :: neq
            real(kind=8) :: c
            real(kind=8) :: dt
            real(kind=8) :: d0(*)
            real(kind=8) :: v0(*)
            real(kind=8) :: a0(*)
            real(kind=8) :: d1(*)
            real(kind=8) :: a1(*)
          end subroutine newdep
        end interface
