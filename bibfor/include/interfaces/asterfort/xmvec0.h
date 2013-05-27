        interface
          subroutine xmvec0(ndim,jnne,nnc,nfaes,dlagrc,hpg,ffc,jacobi,&
     &cface,jpcai,coefcr,coefcp,lpenac,typmai,jddle,nconta,nfhe,lmulti,&
     &heavno,vtmp)
            integer :: ndim
            integer :: jnne(3)
            integer :: nnc
            integer :: nfaes
            real(kind=8) :: dlagrc
            real(kind=8) :: hpg
            real(kind=8) :: ffc(9)
            real(kind=8) :: jacobi
            integer :: cface(3,5)
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
            real(kind=8) :: vtmp(336)
          end subroutine xmvec0
        end interface
