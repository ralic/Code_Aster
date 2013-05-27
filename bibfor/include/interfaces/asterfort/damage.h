        interface
          subroutine damage(curvvp,bend,k,dmax,dam,tanmrp,alpha,beta,&
     &gamma)
            real(kind=8) :: curvvp(2)
            integer :: bend
            real(kind=8) :: k
            real(kind=8) :: dmax
            real(kind=8) :: dam
            real(kind=8) :: tanmrp(3,3)
            real(kind=8) :: alpha
            real(kind=8) :: beta
            real(kind=8) :: gamma
          end subroutine damage
        end interface
