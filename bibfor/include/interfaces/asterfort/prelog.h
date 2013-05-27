        interface
          subroutine prelog(ndim,lgpg,vim,gn,lamb,logl,fm,fp,epsml,&
     &deps,tn,resi,iret)
            integer :: lgpg
            integer :: ndim
            real(kind=8) :: vim(lgpg)
            real(kind=8) :: gn(3,3)
            real(kind=8) :: lamb(3)
            real(kind=8) :: logl(3)
            real(kind=8) :: fm(3,3)
            real(kind=8) :: fp(3,3)
            real(kind=8) :: epsml(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: tn(6)
            logical :: resi
            integer :: iret
          end subroutine prelog
        end interface
