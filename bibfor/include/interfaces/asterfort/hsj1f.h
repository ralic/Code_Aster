        interface
          subroutine hsj1f(intsn,xr,epais,vectg,vectt,hsf,kwgt,hsj1fx,&
     &wgt)
            integer :: intsn
            real(kind=8) :: xr(*)
            real(kind=8) :: epais
            real(kind=8) :: vectg(2,3)
            real(kind=8) :: vectt(3,3)
            real(kind=8) :: hsf(3,9)
            integer :: kwgt
            real(kind=8) :: hsj1fx(3,9)
            real(kind=8) :: wgt
          end subroutine hsj1f
        end interface
