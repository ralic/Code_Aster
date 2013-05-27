        interface
          subroutine cripoi(nbm,b,crit)
            integer :: nbm
            complex(kind=8) :: b(nbm,nbm)
            real(kind=8) :: crit
          end subroutine cripoi
        end interface
