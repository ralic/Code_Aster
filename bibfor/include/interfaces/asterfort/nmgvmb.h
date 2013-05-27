        interface
          subroutine nmgvmb(ndim,nno1,nno2,npg,axi,geom,vff1,vff2,&
     &idfde1,idfde2,iw,nddl,neps,b,w,ni2ldc)
            integer :: npg
            integer :: nno2
            integer :: nno1
            integer :: ndim
            logical :: axi
            real(kind=8) :: geom(ndim,nno1)
            real(kind=8) :: vff1(nno1,npg)
            real(kind=8) :: vff2(nno2,npg)
            integer :: idfde1
            integer :: idfde2
            integer :: iw
            integer :: nddl
            integer :: neps
            real(kind=8) :: b(3*ndim+2,npg,*)
            real(kind=8) :: w(npg)
            real(kind=8) :: ni2ldc(3*ndim+2)
          end subroutine nmgvmb
        end interface
