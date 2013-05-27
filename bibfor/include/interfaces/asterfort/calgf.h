        interface
          subroutine calgf(ndim,nno,axi,npg,geomi,g,iw,vff,idff,deplm,&
     &deplt,grand,alpha,r,w,dff,fm,fmp,fma)
            integer :: npg
            integer :: nno
            integer :: ndim
            logical :: axi
            real(kind=8) :: geomi(ndim,nno)
            integer :: g
            integer :: iw
            real(kind=8) :: vff(nno,npg)
            integer :: idff
            real(kind=8) :: deplm(nno*ndim)
            real(kind=8) :: deplt(nno*ndim)
            logical :: grand
            real(kind=8) :: alpha
            real(kind=8) :: r
            real(kind=8) :: w
            real(kind=8) :: dff(nno,ndim)
            real(kind=8) :: fm(3,3)
            real(kind=8) :: fmp(3,3)
            real(kind=8) :: fma(3,3)
          end subroutine calgf
        end interface
