        interface
          subroutine cale(ndim,fd,id,fdm,fdmt,prodf,edpn1)
            integer :: ndim
            real(kind=8) :: fd(3,3)
            real(kind=8) :: id(3,3)
            real(kind=8) :: fdm(3,3)
            real(kind=8) :: fdmt(3,3)
            real(kind=8) :: prodf(3,3)
            real(kind=8) :: edpn1(3,3)
          end subroutine cale
        end interface
