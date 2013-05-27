        interface
          subroutine xcfacf(ptint,ptmax,ipt,ainter,lsn,lst,igeom,nno,&
     &ndim,typma,noma,nmaabs)
            integer :: nno
            real(kind=8) :: ptint(*)
            integer :: ptmax
            integer :: ipt
            real(kind=8) :: ainter(*)
            real(kind=8) :: lsn(nno)
            real(kind=8) :: lst(nno)
            integer :: igeom
            integer :: ndim
            character(len=8) :: typma
            character(len=8) :: noma
            integer :: nmaabs
          end subroutine xcfacf
        end interface
