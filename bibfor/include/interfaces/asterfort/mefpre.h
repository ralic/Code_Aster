        interface
          subroutine mefpre(ndim,alpha,z,cf,dh,vit,rho,pstat,dpstat,&
     &dvit,itypg,zg,hg,axg,pm,xig,afluid,cdg,cfg,vitg,rhog)
            integer :: ndim(14)
            real(kind=8) :: alpha
            real(kind=8) :: z(*)
            real(kind=8) :: cf(*)
            real(kind=8) :: dh
            real(kind=8) :: vit(*)
            real(kind=8) :: rho(*)
            real(kind=8) :: pstat(*)
            real(kind=8) :: dpstat(*)
            real(kind=8) :: dvit(*)
            integer :: itypg(*)
            real(kind=8) :: zg(*)
            real(kind=8) :: hg(*)
            real(kind=8) :: axg(*)
            real(kind=8) :: pm
            real(kind=8) :: xig(*)
            real(kind=8) :: afluid
            real(kind=8) :: cdg(*)
            real(kind=8) :: cfg(*)
            real(kind=8) :: vitg(*)
            real(kind=8) :: rhog(*)
          end subroutine mefpre
        end interface
