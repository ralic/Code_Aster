        interface
          subroutine nmfdff(ndim,nno,axi,g,r,rigi,matsym,fr,vff,dff,&
     &def,pff)
            integer :: nno
            integer :: ndim
            logical :: axi
            integer :: g
            real(kind=8) :: r
            logical :: rigi
            logical :: matsym
            real(kind=8) :: fr(3,3)
            real(kind=8) :: vff(nno,*)
            real(kind=8) :: dff(nno,*)
            real(kind=8) :: def(2*ndim,nno,ndim)
            real(kind=8) :: pff(2*ndim,nno,nno)
          end subroutine nmfdff
        end interface
