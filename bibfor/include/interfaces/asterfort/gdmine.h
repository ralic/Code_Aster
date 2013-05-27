        interface
          subroutine gdmine(kp,nno,pjacob,en,grani,alfnmk,delnmk,pas,&
     &rot0,rotm,rotkm1,rotk,rmkm1,rmk,omgkm,ompgkm,omgk,ompgk,rigi)
            integer :: kp
            integer :: nno
            real(kind=8) :: pjacob
            real(kind=8) :: en(3,2)
            real(kind=8) :: grani(4)
            real(kind=8) :: alfnmk
            real(kind=8) :: delnmk
            real(kind=8) :: pas
            real(kind=8) :: rot0(3,3)
            real(kind=8) :: rotm(3,3)
            real(kind=8) :: rotkm1(3,3)
            real(kind=8) :: rotk(3,3)
            real(kind=8) :: rmkm1(3)
            real(kind=8) :: rmk(3)
            real(kind=8) :: omgkm(3)
            real(kind=8) :: ompgkm(3)
            real(kind=8) :: omgk(3)
            real(kind=8) :: ompgk(3)
            real(kind=8) :: rigi(18,18)
          end subroutine gdmine
        end interface
