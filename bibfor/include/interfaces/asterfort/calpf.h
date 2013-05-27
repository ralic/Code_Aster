        interface
          subroutine calpf(ndim,nno,axi,npg,geomm,g,iw,vff,idff,depld,&
     &grand,alpha,r,dff,fd,deplda,fda)
            integer :: npg
            integer :: nno
            integer :: ndim
            logical :: axi
            real(kind=8) :: geomm(ndim,nno)
            integer :: g
            integer :: iw
            real(kind=8) :: vff(nno,npg)
            integer :: idff
            real(kind=8) :: depld(81)
            logical :: grand
            real(kind=8) :: alpha
            real(kind=8) :: r
            real(kind=8) :: dff(nno,ndim)
            real(kind=8) :: fd(3,3)
            real(kind=8) :: deplda(81)
            real(kind=8) :: fda(3,3)
          end subroutine calpf
        end interface
