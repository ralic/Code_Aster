        interface
          subroutine mefeig(ndim,nbmod,matm,matr,mata,fre,ksi,mavr,&
     &alfr,alfi,mat1,mavi,w,z,ind)
            integer :: nbmod
            integer :: ndim(14)
            real(kind=8) :: matm(nbmod,nbmod)
            real(kind=8) :: matr(nbmod,nbmod)
            real(kind=8) :: mata(nbmod,nbmod)
            real(kind=8) :: fre(nbmod)
            real(kind=8) :: ksi(nbmod)
            real(kind=8) :: mavr(2*nbmod,2*nbmod)
            real(kind=8) :: alfr(2*nbmod)
            real(kind=8) :: alfi(2*nbmod)
            real(kind=8) :: mat1(2*nbmod,2*nbmod)
            real(kind=8) :: mavi(2*nbmod,2*nbmod)
            real(kind=8) :: w(4*nbmod)
            real(kind=8) :: z(4*nbmod,2*nbmod)
            integer :: ind(2*nbmod)
          end subroutine mefeig
        end interface
