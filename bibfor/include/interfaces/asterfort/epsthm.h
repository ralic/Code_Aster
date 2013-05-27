        interface
          subroutine epsthm(nddls,nddlm,nno,nnos,nnom,nmec,dimdef,&
     &dimuel,ndim,npi,ipoids,ipoid2,ivf,ivf2,idfde,idfde2,dfdi,dfdi2,b,&
     &geom,depla,mecani,press1,press2,tempe,np1,np2,axi,epsm)
            integer :: npi
            integer :: ndim
            integer :: dimuel
            integer :: dimdef
            integer :: nnos
            integer :: nno
            integer :: nddls
            integer :: nddlm
            integer :: nnom
            integer :: nmec
            integer :: ipoids
            integer :: ipoid2
            integer :: ivf
            integer :: ivf2
            integer :: idfde
            integer :: idfde2
            real(kind=8) :: dfdi(nno,3)
            real(kind=8) :: dfdi2(nnos,3)
            real(kind=8) :: b(dimdef,dimuel)
            real(kind=8) :: geom(ndim,nno)
            real(kind=8) :: depla(dimuel)
            integer :: mecani(5)
            integer :: press1(7)
            integer :: press2(7)
            integer :: tempe(5)
            integer :: np1
            integer :: np2
            logical :: axi
            real(kind=8) :: epsm(6,npi)
          end subroutine epsthm
        end interface
