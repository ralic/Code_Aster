        interface
          subroutine xmvef1(ndim,jnne,jnnm,ndeple,nnc,nfaes,cface,hpg,&
     &ffc,ffe,ffm,jacobi,jpcai,dlagrc,dlagrf,coeffr,coeffp,lpenaf,coefff&
     &,tau1,tau2,rese,mproj,coefcr,coefcp,jeu,typmai,nsinge,nsingm,rre,&
     &rrm,nvit,nconta,jddle,jddlm,nfhe,vtmp)
            integer :: ndim
            integer :: jnne(3)
            integer :: jnnm(3)
            integer :: ndeple
            integer :: nnc
            integer :: nfaes
            integer :: cface(5,3)
            real(kind=8) :: hpg
            real(kind=8) :: ffc(9)
            real(kind=8) :: ffe(20)
            real(kind=8) :: ffm(20)
            real(kind=8) :: jacobi
            integer :: jpcai
            real(kind=8) :: dlagrc
            real(kind=8) :: dlagrf(2)
            real(kind=8) :: coeffr
            real(kind=8) :: coeffp
            logical :: lpenaf
            real(kind=8) :: coefff
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: rese(3)
            real(kind=8) :: mproj(3,3)
            real(kind=8) :: coefcr
            real(kind=8) :: coefcp
            real(kind=8) :: jeu
            character(len=8) :: typmai
            integer :: nsinge
            integer :: nsingm
            real(kind=8) :: rre
            real(kind=8) :: rrm
            integer :: nvit
            integer :: nconta
            integer :: jddle(2)
            integer :: jddlm(2)
            integer :: nfhe
            real(kind=8) :: vtmp(336)
          end subroutine xmvef1
        end interface
