        interface
          subroutine zzcala(npg,nno,wi,x,y,xmin,xmax,ymin,ymax,a)
            integer :: npg
            integer :: nno
            real(kind=8) :: wi(1)
            real(kind=8) :: x(1)
            real(kind=8) :: y(1)
            real(kind=8) :: xmin
            real(kind=8) :: xmax
            real(kind=8) :: ymin
            real(kind=8) :: ymax
            real(kind=8) :: a(9,9)
          end subroutine zzcala
        end interface
