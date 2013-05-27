        interface
          subroutine mlfmlt(b,f,y,ldb,n,p,l,opta,optb,nb)
            integer :: l
            integer :: p
            integer :: n
            integer :: ldb
            real(kind=8) :: b(ldb,l)
            real(kind=8) :: f(n,p)
            real(kind=8) :: y(ldb,l)
            integer :: opta
            integer :: optb
            integer :: nb
          end subroutine mlfmlt
        end interface
