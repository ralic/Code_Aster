        interface
          subroutine gbil3d(dudm,dvdm,dtdm,dfudm,dfvdm,tgudm,tgvdm,&
     &ttrgu,ttrgv,poids,c1,c2,c3,k3a,rho,puls,g)
            real(kind=8) :: dudm(3,4)
            real(kind=8) :: dvdm(3,4)
            real(kind=8) :: dtdm(3,4)
            real(kind=8) :: dfudm(3,4)
            real(kind=8) :: dfvdm(3,4)
            real(kind=8) :: tgudm(3)
            real(kind=8) :: tgvdm(3)
            real(kind=8) :: ttrgu
            real(kind=8) :: ttrgv
            real(kind=8) :: poids
            real(kind=8) :: c1
            real(kind=8) :: c2
            real(kind=8) :: c3
            real(kind=8) :: k3a
            real(kind=8) :: rho
            real(kind=8) :: puls
            real(kind=8) :: g
          end subroutine gbil3d
        end interface
