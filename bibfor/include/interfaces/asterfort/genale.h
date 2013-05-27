        interface
          subroutine genale(vec1,vec2,r,v,x,dim,long,lonv,ln)
            integer :: lonv
            integer :: long
            integer :: dim
            real(kind=8) :: vec1(long)
            real(kind=8) :: vec2(lonv)
            complex(kind=8) :: r(dim,dim)
            complex(kind=8) :: v(dim)
            complex(kind=8) :: x(dim)
            integer :: ln
          end subroutine genale
        end interface
