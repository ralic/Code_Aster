        interface
          subroutine xmmaa1(ndim,jnne,ndeple,nnc,jnnm,nfaes,cface,hpg,&
     &ffc,ffe,ffm,jacobi,jpcai,coefcr,coefcp,lpenac,norm,typmai,nsinge,&
     &nsingm,rre,rrm,nconta,jddle,jddlm,nfhe,nfhm,lmulti,heavno,heavfa,&
     &mmat)
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
            real(kind=8) :: coefcr
            real(kind=8) :: coefcp
            logical :: lpenac
            real(kind=8) :: norm(3)
            character(len=8) :: typmai
            integer :: nsinge
            integer :: nsingm
            real(kind=8) :: rre
            real(kind=8) :: rrm
            integer :: nconta
            integer :: jddle(2)
            integer :: jddlm(2)
            integer :: nfhe
            integer :: nfhm
            logical :: lmulti
            integer :: heavno(8)
            integer :: heavfa(*)
            real(kind=8) :: mmat(336,336)
          end subroutine xmmaa1
        end interface
