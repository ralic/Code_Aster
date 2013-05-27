        interface
          subroutine matthm(ndim,axi,nno1,nno2,dimuel,dimdef,iu,ip,ipf&
     &,iq,yap1,yap2,yate,addep1,addep2,addlh1,vff1,vff2,dffr2,wref,geom,&
     &ang,wi,q)
            integer :: dimdef
            integer :: dimuel
            integer :: nno2
            integer :: nno1
            integer :: ndim
            logical :: axi
            integer :: iu(3,18)
            integer :: ip(2,9)
            integer :: ipf(2,2,9)
            integer :: iq(2,2,9)
            integer :: yap1
            integer :: yap2
            integer :: yate
            integer :: addep1
            integer :: addep2
            integer :: addlh1
            real(kind=8) :: vff1(nno1)
            real(kind=8) :: vff2(nno2)
            real(kind=8) :: dffr2(ndim-1,nno2)
            real(kind=8) :: wref
            real(kind=8) :: geom(ndim,nno2)
            real(kind=8) :: ang(24)
            real(kind=8) :: wi
            real(kind=8) :: q(dimdef,dimuel)
          end subroutine matthm
        end interface
