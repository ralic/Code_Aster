        interface
          subroutine hbvaec(gamma,nbmat,materf,parame)
            integer :: nbmat
            real(kind=8) :: gamma
            real(kind=8) :: materf(nbmat,2)
            real(kind=8) :: parame(4)
          end subroutine hbvaec
        end interface
