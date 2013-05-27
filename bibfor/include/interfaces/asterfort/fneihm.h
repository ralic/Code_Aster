        interface
          subroutine fneihm(fnoevo,deltat,perman,nno1,nno2,npi,npg,&
     &wref,iu,ip,ipf,iq,vff1,vff2,dffr2,geom,ang,congem,r,vectu,mecani,&
     &press1,press2,tempe,dimdef,dimcon,dimuel,ndim,axi)
            integer :: ndim
            integer :: dimuel
            integer :: dimcon
            integer :: dimdef
            integer :: npg
            integer :: npi
            integer :: nno2
            integer :: nno1
            logical :: fnoevo
            real(kind=8) :: deltat
            logical :: perman
            real(kind=8) :: wref(npg)
            integer :: iu(3,18)
            integer :: ip(2,9)
            integer :: ipf(2,2,9)
            integer :: iq(2,2,9)
            real(kind=8) :: vff1(nno1,npi)
            real(kind=8) :: vff2(nno2,npi)
            real(kind=8) :: dffr2(ndim-1,nno2,npi)
            real(kind=8) :: geom(ndim,nno2)
            real(kind=8) :: ang(24)
            real(kind=8) :: congem(dimcon,npi)
            real(kind=8) :: r(dimdef)
            real(kind=8) :: vectu(dimuel)
            integer :: mecani(8)
            integer :: press1(9)
            integer :: press2(9)
            integer :: tempe(5)
            logical :: axi
          end subroutine fneihm
        end interface
