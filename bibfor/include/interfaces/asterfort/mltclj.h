        interface
          subroutine mltclj(nb,n,ll,m,it,p,front,frn,adper,trav,c)
            integer :: p
            integer :: nb
            integer :: n
            integer :: ll
            integer :: m
            integer :: it
            complex(kind=8) :: front(*)
            complex(kind=8) :: frn(*)
            integer :: adper(*)
            complex(kind=8) :: trav(p,nb,*)
            complex(kind=8) :: c(nb,nb,*)
          end subroutine mltclj
        end interface
