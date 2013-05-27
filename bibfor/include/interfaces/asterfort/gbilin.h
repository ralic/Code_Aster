        interface
          subroutine gbilin(fami,kp,imate,dudm,dvdm,dtdm,dfdm,tgdm,&
     &poids,c1,c2,c3,cs,th,coef,rho,puls,axi,g)
            character(*) :: fami
            integer :: kp
            integer :: imate
            real(kind=8) :: dudm(3,4)
            real(kind=8) :: dvdm(3,4)
            real(kind=8) :: dtdm(3,4)
            real(kind=8) :: dfdm(3,4)
            real(kind=8) :: tgdm(2)
            real(kind=8) :: poids
            real(kind=8) :: c1
            real(kind=8) :: c2
            real(kind=8) :: c3
            real(kind=8) :: cs
            real(kind=8) :: th
            real(kind=8) :: coef
            real(kind=8) :: rho
            real(kind=8) :: puls
            logical :: axi
            real(kind=8) :: g
          end subroutine gbilin
        end interface
