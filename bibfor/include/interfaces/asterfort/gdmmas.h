        interface
          subroutine gdmmas(kp,nno,pjacob,en,grani,rot0,mass)
            integer :: kp
            integer :: nno
            real(kind=8) :: pjacob
            real(kind=8) :: en(3,2)
            real(kind=8) :: grani(4)
            real(kind=8) :: rot0(3,3)
            real(kind=8) :: mass(18,18)
          end subroutine gdmmas
        end interface
