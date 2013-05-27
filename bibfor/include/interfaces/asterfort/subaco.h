        interface
          subroutine subaco(nno,dff,geom,cova)
            integer :: nno
            real(kind=8) :: dff(2,nno)
            real(kind=8) :: geom(3,nno)
            real(kind=8) :: cova(3,3)
          end subroutine subaco
        end interface
