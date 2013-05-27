        interface
          subroutine xmmab1(ndim,jnne,ndeple,nnc,jnnm,nfaes,cface,hpg,&
     &ffc,ffe,ffm,jacobi,jpcai,lambda,coefcr,coefcp,dvitet,coeffr,dlagrf&
     &,jeu,coeffp,coefff,lpenaf,tau1,tau2,rese,mproj,norm,typmai,nsinge,&
     &nsingm,rre,rrm,nvit,nconta,jddle,jddlm,nfhe,mmat)
            integer :: ndim
            integer :: jnne(3)
            integer :: ndeple
            integer :: nnc
            integer :: jnnm(3)
            integer :: nfaes
            integer :: cface(5,3)
            real(kind=8) :: hpg
            real(kind=8) :: ffc(8)
            real(kind=8) :: ffe(20)
            real(kind=8) :: ffm(20)
            real(kind=8) :: jacobi
            integer :: jpcai
            real(kind=8) :: lambda
            real(kind=8) :: coefcr
            real(kind=8) :: coefcp
            real(kind=8) :: dvitet(3)
            real(kind=8) :: coeffr
            real(kind=8) :: dlagrf(2)
            real(kind=8) :: jeu
            real(kind=8) :: coeffp
            real(kind=8) :: coefff
            logical :: lpenaf
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: rese(3)
            real(kind=8) :: mproj(3,3)
            real(kind=8) :: norm(3)
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
            real(kind=8) :: mmat(336,336)
          end subroutine xmmab1
        end interface
