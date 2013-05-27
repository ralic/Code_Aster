        interface
          subroutine xtlagc(typmai,ndim,nnc,jnn,nddls,nface,cface,&
     &jdepde,jpcai,ffc,nconta,nfhe,lmulti,heavno,dlagrc)
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
            integer :: nconta
            integer :: nfhe
            logical :: lmulti
            integer :: heavno(8)
            real(kind=8) :: dlagrc
          end subroutine xtlagc
        end interface
