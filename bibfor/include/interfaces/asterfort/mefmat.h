        interface
          subroutine mefmat(ndim,numgrp,nbz,nbgrp,nbmod,matma,dcent,cp&
     &,cf,vit,rho,pstat,dpstat,rint,phix,phiy,z,matm,matr,mata,itypg,axg&
     &,zg,rhog,vitg,cdg,cpg)
            integer :: nbmod
            integer :: nbgrp
            integer :: nbz
            integer :: ndim(14)
            integer :: numgrp(*)
            real(kind=8) :: matma(*)
            real(kind=8) :: dcent(*)
            real(kind=8) :: cp(*)
            real(kind=8) :: cf(*)
            real(kind=8) :: vit(0:*)
            real(kind=8) :: rho(0:*)
            real(kind=8) :: pstat(*)
            real(kind=8) :: dpstat(*)
            real(kind=8) :: rint(*)
            real(kind=8) :: phix(nbz*nbgrp,nbmod)
            real(kind=8) :: phiy(nbz*nbgrp,nbmod)
            real(kind=8) :: z(*)
            real(kind=8) :: matm(nbmod,nbmod)
            real(kind=8) :: matr(nbmod,nbmod)
            real(kind=8) :: mata(nbmod,nbmod)
            integer :: itypg(*)
            real(kind=8) :: axg(*)
            real(kind=8) :: zg(*)
            real(kind=8) :: rhog(*)
            real(kind=8) :: vitg(*)
            real(kind=8) :: cdg(*)
            real(kind=8) :: cpg(*)
          end subroutine mefmat
        end interface
