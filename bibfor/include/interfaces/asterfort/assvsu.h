        interface
          subroutine assvsu(nno,nnos,nface,geom,crit,deplm,ddepl,&
     &congem,congep,vintm,vintp,defgem,defgep,dsde,matuu,vectu,rinstm,&
     &rinstp,option,imate,mecani,press1,press2,tempe,dimdef,dimcon,&
     &dimuel,nbvari,ndim,compor,typmod,typvf,axi,perman,nvoima,nscoma,&
     &nbvois,livois,nbnovo,nbsoco,lisoco)
            integer :: nbvois
            integer :: nscoma
            integer :: nvoima
            integer :: ndim
            integer :: nbvari
            integer :: dimuel
            integer :: dimcon
            integer :: dimdef
            integer :: nno
            integer :: nnos
            integer :: nface
            real(kind=8) :: geom(ndim,nno)
            real(kind=8) :: crit(*)
            real(kind=8) :: deplm(dimuel)
            real(kind=8) :: ddepl(dimuel)
            real(kind=8) :: congem(dimcon,7)
            real(kind=8) :: congep(dimcon,7)
            real(kind=8) :: vintm(nbvari,7)
            real(kind=8) :: vintp(nbvari,7)
            real(kind=8) :: defgem(dimdef)
            real(kind=8) :: defgep(dimdef)
            real(kind=8) :: dsde(dimcon,dimdef)
            real(kind=8) :: matuu((nbvois+1)*dimuel*dimuel)
            real(kind=8) :: vectu(dimuel)
            real(kind=8) :: rinstm
            real(kind=8) :: rinstp
            character(len=16) :: option
            integer :: imate
            integer :: mecani(5)
            integer :: press1(7)
            integer :: press2(7)
            integer :: tempe(5)
            character(len=16) :: compor(*)
            character(len=8) :: typmod(2)
            integer :: typvf
            logical :: axi
            logical :: perman
            integer :: livois(nvoima)
            integer :: nbnovo(nvoima)
            integer :: nbsoco(nvoima)
            integer :: lisoco(nvoima,nscoma,2)
          end subroutine assvsu
        end interface
