        interface
          subroutine xmmab0(ndim,nnc,jnne,nfaes,jpcai,hpg,ffc,jacobi,&
     &coefcr,lpenac,typmai,cface,tau1,tau2,jddle,nconta,nfhe,lmulti,&
     &heavno,mmat)
            integer :: ndim
            integer :: nnc
            integer :: jnne(3)
            integer :: nfaes
            integer :: jpcai
            real(kind=8) :: hpg
            real(kind=8) :: ffc(8)
            real(kind=8) :: jacobi
            real(kind=8) :: coefcr
            logical :: lpenac
            character(len=8) :: typmai
            integer :: cface(5,3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            integer :: jddle(2)
            integer :: nconta
            integer :: nfhe
            logical :: lmulti
            integer :: heavno(8)
            real(kind=8) :: mmat(336,336)
          end subroutine xmmab0
        end interface
