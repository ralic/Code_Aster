        interface
          subroutine nzcalc(crit,phasp,nz,fmel,seuil,dt,trans,rprim,&
     &deuxmu,eta,unsurn,dp,iret)
            real(kind=8) :: crit(3)
            real(kind=8) :: phasp(5)
            integer :: nz
            real(kind=8) :: fmel
            real(kind=8) :: seuil
            real(kind=8) :: dt
            real(kind=8) :: trans
            real(kind=8) :: rprim
            real(kind=8) :: deuxmu
            real(kind=8) :: eta(5)
            real(kind=8) :: unsurn(5)
            real(kind=8) :: dp
            integer :: iret
          end subroutine nzcalc
        end interface
