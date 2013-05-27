        interface
          subroutine nzfpri(deuxmu,trans,rprim,seuil,phasp,nz,fmel,eta&
     &,unsurn,dt,dp,fplas,fp,fd,fprim,fdevi)
            integer :: nz
            real(kind=8) :: deuxmu
            real(kind=8) :: trans
            real(kind=8) :: rprim
            real(kind=8) :: seuil
            real(kind=8) :: phasp(5)
            real(kind=8) :: fmel
            real(kind=8) :: eta(nz)
            real(kind=8) :: unsurn(nz)
            real(kind=8) :: dt
            real(kind=8) :: dp
            real(kind=8) :: fplas
            real(kind=8) :: fp(nz)
            real(kind=8) :: fd(5)
            real(kind=8) :: fprim
            real(kind=8) :: fdevi
          end subroutine nzfpri
        end interface
