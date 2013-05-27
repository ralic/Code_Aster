        interface
          subroutine refthm(fnoevo,dt,perman,nno,nnos,nnom,npi,npg,&
     &ipoids,ipoid2,ivf,ivf2,idfde,idfde2,geom,b,dfdi,dfdi2,r,vectu,&
     &imate,mecani,press1,press2,tempe,dimdef,dimcon,dimuel,nddls,nddlm,&
     &nmec,np1,np2,ndim,axi)
            integer :: ndim
            integer :: dimuel
            integer :: dimdef
            integer :: nnos
            integer :: nno
            logical :: fnoevo
            real(kind=8) :: dt
            logical :: perman
            integer :: nnom
            integer :: npi
            integer :: npg
            integer :: ipoids
            integer :: ipoid2
            integer :: ivf
            integer :: ivf2
            integer :: idfde
            integer :: idfde2
            real(kind=8) :: geom(ndim,nno)
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
            integer :: dimcon
            integer :: nddls
            integer :: nddlm
            integer :: nmec
            integer :: np1
            integer :: np2
            logical :: axi
          end subroutine refthm
        end interface
