        interface
          subroutine tanmat(alpha,beta,gamma,k1,k2,dmax1,dmax2,dam1,&
     &dam2,curv,dcurv,tanma2)
            real(kind=8) :: alpha
            real(kind=8) :: beta
            real(kind=8) :: gamma
            real(kind=8) :: k1
            real(kind=8) :: k2
            real(kind=8) :: dmax1
            real(kind=8) :: dmax2
            real(kind=8) :: dam1
            real(kind=8) :: dam2
            real(kind=8) :: curv(3)
            real(kind=8) :: dcurv(3)
            real(kind=8) :: tanma2(3,3)
          end subroutine tanmat
        end interface
