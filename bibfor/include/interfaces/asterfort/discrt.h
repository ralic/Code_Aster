        interface
          subroutine discrt(ff0,ff1,ff2,nbpt,amor,f)
            real(kind=8) :: ff0
            real(kind=8) :: ff1
            real(kind=8) :: ff2
            integer :: nbpt
            real(kind=8) :: amor
            real(kind=8) :: f(*)
          end subroutine discrt
        end interface
