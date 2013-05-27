        interface
          subroutine pcinfe(n,icpl,icpc,icpd,icplp,icpcp,ind,lca,ier)
            integer :: n
            integer :: icpl(0:n)
            integer(kind=4) :: icpc(*)
            integer :: icpd(n)
            integer :: icplp(0:n)
            integer :: icpcp(*)
            integer :: ind(n)
            integer :: lca
            integer :: ier
          end subroutine pcinfe
        end interface
