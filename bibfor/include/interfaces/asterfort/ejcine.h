        interface
          subroutine ejcine(ndim,axi,nno1,nno2,vff1,vff2,wref,dffr2,&
     &geom,wg,kpg,ipg,idf2,rot,b)
            integer :: nno2
            integer :: nno1
            integer :: ndim
            logical :: axi
            real(kind=8) :: vff1(nno1)
            real(kind=8) :: vff2(nno2)
            real(kind=8) :: wref
            real(kind=8) :: dffr2(ndim-1,nno2)
            real(kind=8) :: geom(ndim,nno2)
            real(kind=8) :: wg
            integer :: kpg
            integer :: ipg
            integer :: idf2
            real(kind=8) :: rot(ndim,ndim)
            real(kind=8) :: b(2*ndim-1,ndim+1,2*nno1+nno2)
          end subroutine ejcine
        end interface
