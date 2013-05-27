        interface
          subroutine rcfodi(ifon,beta,f,df)
            integer :: ifon
            real(kind=8) :: beta
            real(kind=8) :: f
            real(kind=8) :: df
          end subroutine rcfodi
        end interface
