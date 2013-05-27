        interface
          subroutine eicine(ndim,axi,nno1,nno2,vff1,vff2,wref,dffr2,&
     &geom,ang,wg,b)
            integer :: nno2
            integer :: nno1
            integer :: ndim
            logical :: axi
            real(kind=8) :: vff1(nno1)
            real(kind=8) :: vff2(nno2)
            real(kind=8) :: wref
            real(kind=8) :: dffr2(ndim-1,nno2)
            real(kind=8) :: geom(ndim,nno2)
            real(kind=8) :: ang(*)
            real(kind=8) :: wg
            real(kind=8) :: b(3,3,2*nno1)
          end subroutine eicine
        end interface
