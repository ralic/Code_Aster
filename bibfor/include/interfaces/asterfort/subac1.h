        interface
          subroutine subac1(laxi,nno,vff,dff,geom,cova)
            integer :: nno
            logical :: laxi
            real(kind=8) :: vff(nno)
            real(kind=8) :: dff(nno)
            real(kind=8) :: geom(2,nno)
            real(kind=8) :: cova(3,3)
          end subroutine subac1
        end interface
