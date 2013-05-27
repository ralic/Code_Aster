        interface
          subroutine poslog(resi,rigi,tn,tp,fm,lgpg,vip,ndim,fp,g,dtde&
     &,sigm,cplan,fami,mate,instp,angmas,gn,lamb,logl,sigp,dsidep,pk2m,&
     &pk2,codret)
            integer :: ndim
            integer :: lgpg
            logical :: resi
            logical :: rigi
            real(kind=8) :: tn(6)
            real(kind=8) :: tp(6)
            real(kind=8) :: fm(3,3)
            real(kind=8) :: vip(lgpg)
            real(kind=8) :: fp(3,3)
            integer :: g
            real(kind=8) :: dtde(6,6)
            real(kind=8) :: sigm(2*ndim)
            logical :: cplan
            character(*) :: fami
            integer :: mate
            real(kind=8) :: instp
            real(kind=8) :: angmas(*)
            real(kind=8) :: gn(3,3)
            real(kind=8) :: lamb(3)
            real(kind=8) :: logl(3)
            real(kind=8) :: sigp(2*ndim)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: pk2m(6)
            real(kind=8) :: pk2(6)
            integer :: codret
          end subroutine poslog
        end interface
