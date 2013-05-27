        interface
          subroutine gdfine(kp,nno,pjacob,en,grani,rot0,rotk,omgk,&
     &ompgk,fint)
            integer :: kp
            integer :: nno
            real(kind=8) :: pjacob
            real(kind=8) :: en(3,2)
            real(kind=8) :: grani(4)
            real(kind=8) :: rot0(3,3)
            real(kind=8) :: rotk(3,3)
            real(kind=8) :: omgk(3)
            real(kind=8) :: ompgk(3)
            real(kind=8) :: fint(6,3)
          end subroutine gdfine
        end interface
