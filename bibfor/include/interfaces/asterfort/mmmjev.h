        interface
          subroutine mmmjev(ndim,norm,vitpe,vitpm,jeuvit)
            integer :: ndim
            real(kind=8) :: norm(3)
            real(kind=8) :: vitpe(3)
            real(kind=8) :: vitpm(3)
            real(kind=8) :: jeuvit
          end subroutine mmmjev
        end interface
