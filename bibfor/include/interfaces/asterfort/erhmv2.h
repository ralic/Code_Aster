        interface
          subroutine erhmv2(axi,perman,deltat,dimdep,dimdef,nmec,np1,&
     &np2,ndim,nno,nnos,nnom,npg,nddls,nddlm,dimuel,ipoids,ivf,idfde,&
     &ipoid2,ivf2,idfde2,geom,fovo,deplp,deplm,sielnp,sielnm,nbcmp,biot,&
     &unsurm,fpx,fpy,frx,fry,yamec,addeme,yap1,addep1,yap2,addep2,yate,&
     &addete,tm2h1v)
            integer :: dimuel
            integer :: nnos
            integer :: nno
            integer :: ndim
            integer :: dimdef
            integer :: dimdep
            logical :: axi
            logical :: perman
            real(kind=8) :: deltat
            integer :: nmec
            integer :: np1
            integer :: np2
            integer :: nnom
            integer :: npg
            integer :: nddls
            integer :: nddlm
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            integer :: ipoid2
            integer :: ivf2
            integer :: idfde2
            real(kind=8) :: geom(ndim,nno)
            real(kind=8) :: fovo(ndim)
            real(kind=8) :: deplp(nno*dimdep)
            real(kind=8) :: deplm(nno*dimdep)
            real(kind=8) :: sielnp(90)
            real(kind=8) :: sielnm(90)
            integer :: nbcmp
            real(kind=8) :: biot
            real(kind=8) :: unsurm
            real(kind=8) :: fpx
            real(kind=8) :: fpy
            real(kind=8) :: frx(9)
            real(kind=8) :: fry(9)
            integer :: yamec
            integer :: addeme
            integer :: yap1
            integer :: addep1
            integer :: yap2
            integer :: addep2
            integer :: yate
            integer :: addete
            real(kind=8) :: tm2h1v(3)
          end subroutine erhmv2
        end interface
