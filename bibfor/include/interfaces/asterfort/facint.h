        interface
          subroutine facint(nbpas,dim,longh,vec1,vec2,long,s,r,d,u,v,w&
     &)
            integer :: long
            integer :: longh
            integer :: dim
            integer :: nbpas
            real(kind=8) :: vec1(long)
            real(kind=8) :: vec2(longh)
            complex(kind=8) :: s(dim,dim)
            complex(kind=8) :: r(dim,dim)
            real(kind=8) :: d(dim)
            complex(kind=8) :: u(*)
            real(kind=8) :: v(*)
            complex(kind=8) :: w(*)
          end subroutine facint
        end interface
