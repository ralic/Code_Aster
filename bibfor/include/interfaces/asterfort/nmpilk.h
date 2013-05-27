        interface
          subroutine nmpilk(incpr1,incpr2,ddincc,neq,eta,rho,offset)
            character(len=19) :: incpr1
            character(len=19) :: incpr2
            character(len=19) :: ddincc
            integer :: neq
            real(kind=8) :: eta
            real(kind=8) :: rho
            real(kind=8) :: offset
          end subroutine nmpilk
        end interface
