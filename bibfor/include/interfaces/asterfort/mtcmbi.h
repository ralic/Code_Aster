        interface
          subroutine mtcmbi(typmat,lmat,coef,ccoef,lres)
            character(*) :: typmat
            integer :: lmat
            real(kind=8) :: coef
            complex(kind=8) :: ccoef
            integer :: lres
          end subroutine mtcmbi
        end interface
