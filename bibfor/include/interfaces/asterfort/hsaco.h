        interface
          subroutine hsaco(vectt,dudxnc,hsc)
            real(kind=8) :: vectt(3,3)
            real(kind=8) :: dudxnc(9)
            real(kind=8) :: hsc(5,9)
          end subroutine hsaco
        end interface
