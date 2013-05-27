        interface
          subroutine amdapt(neq,nbnd,nbsn,pe,nv,invp,parent,supnd,&
     &adress,lgind,fctnzs,fctops,llist,nnv)
            integer :: neq
            integer :: nbnd
            integer :: nbsn
            integer :: pe(neq+1)
            integer :: nv(neq)
            integer :: invp(neq)
            integer :: parent(*)
            integer :: supnd(neq)
            integer :: adress(*)
            integer :: lgind
            integer :: fctnzs
            real(kind=8) :: fctops
            integer :: llist(neq)
            integer :: nnv(neq)
          end subroutine amdapt
        end interface
