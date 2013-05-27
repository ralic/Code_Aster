        interface
          subroutine critev(epsp,epsd,eta,lambda,deuxmu,fpd,seuil,rd,&
     &crit,critp)
            real(kind=8) :: epsp(7)
            real(kind=8) :: epsd(7)
            real(kind=8) :: eta
            real(kind=8) :: lambda
            real(kind=8) :: deuxmu
            real(kind=8) :: fpd
            real(kind=8) :: seuil
            real(kind=8) :: rd
            real(kind=8) :: crit
            real(kind=8) :: critp
          end subroutine critev
        end interface
