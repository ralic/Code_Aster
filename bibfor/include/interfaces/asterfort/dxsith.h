        interface
          subroutine dxsith(nomte,mater,sigma)
            character(len=16) :: nomte
            integer :: mater
            real(kind=8) :: sigma(*)
          end subroutine dxsith
        end interface
