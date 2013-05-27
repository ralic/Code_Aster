        interface
          subroutine gdjrg0(kp,nno,enprim,x00,y0,ajacob,rot0)
            integer :: kp
            integer :: nno
            real(kind=8) :: enprim(3,2)
            real(kind=8) :: x00(3,3)
            real(kind=8) :: y0(3)
            real(kind=8) :: ajacob
            real(kind=8) :: rot0(3,3)
          end subroutine gdjrg0
        end interface
