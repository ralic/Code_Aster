        interface
          subroutine xmmaa0(ndim,nnc,jnne,hpg,nfaes,cface,ffc,jacobi,&
     &jpcai,coefcr,coefcp,lpenac,typmai,jddle,nconta,nfhe,lmulti,heavno,&
     &mmat)
            integer :: ndim
            integer :: nnc
            integer :: jnne(3)
            real(kind=8) :: hpg
            integer :: nfaes
            integer :: cface(5,3)
            real(kind=8) :: ffc(8)
            real(kind=8) :: jacobi
            integer :: jpcai
            real(kind=8) :: coefcr
            real(kind=8) :: coefcp
            logical :: lpenac
            character(len=8) :: typmai
            integer :: jddle(2)
            integer :: nconta
            integer :: nfhe
            logical :: lmulti
            integer :: heavno(8)
            real(kind=8) :: mmat(336,336)
          end subroutine xmmaa0
        end interface
