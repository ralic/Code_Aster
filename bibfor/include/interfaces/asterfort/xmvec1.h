        interface
          subroutine xmvec1(ndim,jnne,ndeple,nnc,jnnm,hpg,nfaes,ffc,&
     &ffe,ffm,jacobi,dlagrc,jpcai,cface,coefcr,coefcp,lpenac,jeu,norm,&
     &typmai,nsinge,nsingm,rre,rrm,nconta,jddle,jddlm,nfhe,nfhm,lmulti,&
     &heavno,heavfa,vtmp)
            integer :: ndim
            integer :: jnne(3)
            integer :: ndeple
            integer :: nnc
            integer :: jnnm(3)
            real(kind=8) :: hpg
            integer :: nfaes
            real(kind=8) :: ffc(9)
            real(kind=8) :: ffe(20)
            real(kind=8) :: ffm(20)
            real(kind=8) :: jacobi
            real(kind=8) :: dlagrc
            integer :: jpcai
            integer :: cface(3,5)
            real(kind=8) :: coefcr
            real(kind=8) :: coefcp
            logical :: lpenac
            real(kind=8) :: jeu
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
            real(kind=8) :: vtmp(336)
          end subroutine xmvec1
        end interface
