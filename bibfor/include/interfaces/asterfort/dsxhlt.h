        interface
          subroutine dsxhlt(df,jacob,hlt2)
            real(kind=8) :: df(3,3)
            real(kind=8) :: jacob(*)
            real(kind=8) :: hlt2(4,6)
          end subroutine dsxhlt
        end interface
