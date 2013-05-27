        interface
          subroutine nmepsi(ndim,nno,axi,grand,vff,r,dfdi,depl,f,eps)
            integer :: nno
            integer :: ndim
            logical :: axi
            logical :: grand
            real(kind=8) :: vff(nno)
            real(kind=8) :: r
            real(kind=8) :: dfdi(nno,ndim)
            real(kind=8) :: depl(ndim,nno)
            real(kind=8) :: f(3,3)
            real(kind=8) :: eps(6)
          end subroutine nmepsi
        end interface
