        interface
          subroutine fnothm(fnoevo,deltat,perman,nno,nnos,nnom,npi,npg&
     &,ipoids,ipoid2,ivf,ivf2,idfde,idfde2,geom,congem,b,dfdi,dfdi2,r,&
     &vectu,imate,mecani,press1,press2,tempe,dimdef,dimcon,nddls,nddlm,&
     &dimuel,nmec,np1,np2,ndim,axi)
            integer :: ndim
            integer :: dimuel
            integer :: dimcon
            integer :: dimdef
            integer :: npi
            integer :: nnos
            integer :: nno
            logical :: fnoevo
            real(kind=8) :: deltat
            logical :: perman
            integer :: nnom
            integer :: npg
            integer :: ipoids
            integer :: ipoid2
            integer :: ivf
            integer :: ivf2
            integer :: idfde
            integer :: idfde2
            real(kind=8) :: geom(ndim,nno)
            real(kind=8) :: congem(1:npi*dimcon)
            real(kind=8) :: b(dimdef,dimuel)
            real(kind=8) :: dfdi(nno,3)
            real(kind=8) :: dfdi2(nnos,3)
            real(kind=8) :: r(1:dimdef+1)
            real(kind=8) :: vectu(dimuel)
            integer :: imate
            integer :: mecani(5)
            integer :: press1(7)
            integer :: press2(7)
            integer :: tempe(5)
            integer :: nddls
            integer :: nddlm
            integer :: nmec
            integer :: np1
            integer :: np2
            logical :: axi
          end subroutine fnothm
        end interface
