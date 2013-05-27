        interface
          subroutine xmvef0(ndim,jnne,nnc,nfaes,jpcai,hpg,ffc,jacobi,&
     &coefcr,lpenac,dlagrf,cface,typmai,tau1,tau2,jddle,nconta,nfhe,&
     &lmulti,heavno,vtmp)
            integer :: ndim
            integer :: jnne(3)
            integer :: nnc
            integer :: nfaes
            integer :: jpcai
            real(kind=8) :: hpg
            real(kind=8) :: ffc(9)
            real(kind=8) :: jacobi
            real(kind=8) :: coefcr
            logical :: lpenac
            real(kind=8) :: dlagrf(2)
            integer :: cface(5,3)
            character(len=8) :: typmai
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            integer :: jddle(2)
            integer :: nconta
            integer :: nfhe
            logical :: lmulti
            integer :: heavno(8)
            real(kind=8) :: vtmp(336)
          end subroutine xmvef0
        end interface
