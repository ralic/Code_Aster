        interface
          subroutine calcdq(proj,nub,nu,d,pqx,pqy,pqz,dq)
            integer :: proj
            real(kind=8) :: nub
            real(kind=8) :: nu
            real(kind=8) :: d(6,6)
            real(kind=8) :: pqx(4)
            real(kind=8) :: pqy(4)
            real(kind=8) :: pqz(4)
            real(kind=8) :: dq(72)
          end subroutine calcdq
        end interface
