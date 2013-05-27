        interface
          subroutine drfdrg(parame,derpar,h0,sigc,rgdev,dudg,df)
            real(kind=8) :: parame(5)
            real(kind=8) :: derpar(4)
            real(kind=8) :: h0
            real(kind=8) :: sigc
            real(kind=8) :: rgdev
            real(kind=8) :: dudg
            real(kind=8) :: df
          end subroutine drfdrg
        end interface
