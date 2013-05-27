        interface
          subroutine histog(nbpt,v,vmin,vmax,x,y,ndec)
            integer :: nbpt
            real(kind=8) :: v(*)
            real(kind=8) :: vmin
            real(kind=8) :: vmax
            real(kind=8) :: x(*)
            real(kind=8) :: y(*)
            integer :: ndec
          end subroutine histog
        end interface
