        interface
          subroutine eifore(ndim,axi,nno1,nno2,npg,wref,vff1,vff2,&
     &dffr2,geom,ang,iu,im,sigref,depref,vect)
            integer :: npg
            integer :: nno2
            integer :: nno1
            integer :: ndim
            logical :: axi
            real(kind=8) :: wref(npg)
            real(kind=8) :: vff1(nno1,npg)
            real(kind=8) :: vff2(nno2,npg)
            real(kind=8) :: dffr2(ndim-1,nno2,npg)
            real(kind=8) :: geom(ndim,nno2)
            real(kind=8) :: ang(*)
            integer :: iu(3,18)
            integer :: im(3,9)
            real(kind=8) :: sigref
            real(kind=8) :: depref
            real(kind=8) :: vect(2*nno1*ndim+nno2*ndim)
          end subroutine eifore
        end interface
