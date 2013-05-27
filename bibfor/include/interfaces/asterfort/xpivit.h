        interface
          subroutine xpivit(jcesd,jcesv,jcesl,ifiss,cncte,ndim,nummae,&
     &iface,xpc,ypc,nvit,group,naret)
            integer :: jcesd(10)
            integer :: jcesv(10)
            integer :: jcesl(10)
            integer :: ifiss
            character(len=24) :: cncte
            integer :: ndim
            integer :: nummae
            integer :: iface
            real(kind=8) :: xpc
            real(kind=8) :: ypc
            integer :: nvit
            integer :: group
            integer :: naret
          end subroutine xpivit
        end interface
