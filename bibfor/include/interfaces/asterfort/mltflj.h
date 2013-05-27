        interface
          subroutine mltflj(nb,n,ll,m,it,p,front,frn,adper,trav,c)
            integer :: p
            integer :: nb
            integer :: n
            integer :: ll
            integer :: m
            integer :: it
            real(kind=8) :: front(*)
            real(kind=8) :: frn(*)
            integer :: adper(*)
            real(kind=8) :: trav(p,nb,*)
            real(kind=8) :: c(nb,nb,*)
          end subroutine mltflj
        end interface
