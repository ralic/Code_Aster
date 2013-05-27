        interface
          subroutine onmetl(nbnd,nadj,xadjd,adjncy,invpnd,permnd,supnd&
     &,parent,nbsn,nbops,fctnzs,lgind,niv)
            integer(kind=4) :: nbnd
            integer(kind=4) :: nadj
            integer(kind=4) :: xadjd
            integer(kind=4) :: adjncy
            integer(kind=4) :: invpnd
            integer(kind=4) :: permnd
            integer(kind=4) :: supnd
            integer(kind=4) :: parent
            integer(kind=4) :: nbsn
            real(kind=8) :: nbops
            integer(kind=4) :: fctnzs
            integer(kind=4) :: lgind
            integer :: niv
          end subroutine onmetl
        end interface
