        interface
          subroutine rcfode(ifon,temp,f,df)
            integer :: ifon
            real(kind=8) :: temp
            real(kind=8) :: f
            real(kind=8) :: df
          end subroutine rcfode
        end interface
