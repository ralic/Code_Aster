        interface
          subroutine xtlagf(typmai,ndim,nnc,jnn,nddls,nface,cface,&
     &jdepde,jpcai,ffc,nconta,nfhe,dlagrf)
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
            real(kind=8) :: dlagrf(2)
          end subroutine xtlagf
        end interface
