        interface
          subroutine nmmabu(ndim,nno,axi,grand,dfdi,b)
            integer :: nno
            integer :: ndim
            logical :: axi
            logical :: grand
            real(kind=8) :: dfdi(nno,ndim)
            real(kind=8) :: b(6,3,nno)
          end subroutine nmmabu
        end interface
