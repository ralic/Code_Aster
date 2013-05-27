        interface
          subroutine dpassa(xyzgau,repere,irep,passag)
            real(kind=8) :: xyzgau(3)
            real(kind=8) :: repere(7)
            integer :: irep
            real(kind=8) :: passag(6,6)
          end subroutine dpassa
        end interface
