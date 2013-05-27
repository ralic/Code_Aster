        interface
          subroutine criteo(epsp,epsd,eta,ba,d,lambda,mu,alpha,ecrob,&
     &ecrod,seuil,crit,critp)
            real(kind=8) :: epsp(6)
            real(kind=8) :: epsd(6)
            real(kind=8) :: eta
            real(kind=8) :: ba(6)
            real(kind=8) :: d
            real(kind=8) :: lambda
            real(kind=8) :: mu
            real(kind=8) :: alpha
            real(kind=8) :: ecrob
            real(kind=8) :: ecrod
            real(kind=8) :: seuil
            real(kind=8) :: crit
            real(kind=8) :: critp
          end subroutine criteo
        end interface
