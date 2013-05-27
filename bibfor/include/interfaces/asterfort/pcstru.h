        interface
          subroutine pcstru(n,in,ip,icpl,icpc,icpd,icpcx,icplx,niveau,&
     &complt,lca,imp,ier)
            integer :: n
            integer :: in(n)
            integer(kind=4) :: ip(*)
            integer :: icpl(0:n)
            integer(kind=4) :: icpc(*)
            integer :: icpd(n)
            integer :: icpcx(*)
            integer :: icplx(0:n)
            integer :: niveau
            logical :: complt
            integer :: lca
            integer :: imp
            integer :: ier
          end subroutine pcstru
        end interface
