        interface
          subroutine dsxhft(df,jacob,hft2)
            real(kind=8) :: df(3,3)
            real(kind=8) :: jacob(*)
            real(kind=8) :: hft2(2,6)
          end subroutine dsxhft
        end interface
