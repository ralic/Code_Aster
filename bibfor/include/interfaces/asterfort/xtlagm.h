        interface
          subroutine xtlagm(typmai,ndim,nnc,jnn,nddls,nface,cface,&
     &jdepde,jpcai,ffc,lfrott,nconta,nfhe,lmulti,heavno,dlagrc,dlagrf)
            character(len=8) :: typmai
            integer :: ndim
            integer :: nnc
            integer :: jnn(3)
            integer :: nddls
            integer :: nface
            integer :: cface(5,3)
            integer :: jdepde
            integer :: jpcai
            real(kind=8) :: ffc(9)
            logical :: lfrott
            integer :: nconta
            integer :: nfhe
            logical :: lmulti
            integer :: heavno(8)
            real(kind=8) :: dlagrc
            real(kind=8) :: dlagrf(2)
          end subroutine xtlagm
        end interface
