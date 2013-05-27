        interface
          subroutine assesu(nno,nnos,nface,geom,crit,deplm,deplp,&
     &congem,congep,vintm,vintp,defgem,defgep,dsde,matuu,vectu,rinstm,&
     &rinstp,option,imate,mecani,press1,press2,tempe,dimdef,dimcon,&
     &dimuel,nbvari,ndim,compor,typmod,typvf,axi,perman)
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
            real(kind=8) :: deplp(dimuel)
            real(kind=8) :: congem(dimcon,7)
            real(kind=8) :: congep(dimcon,7)
            real(kind=8) :: vintm(nbvari,7)
            real(kind=8) :: vintp(nbvari,7)
            real(kind=8) :: defgem(dimdef)
            real(kind=8) :: defgep(dimdef)
            real(kind=8) :: dsde(dimcon,dimdef)
            real(kind=8) :: matuu(dimuel*dimuel)
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
          end subroutine assesu
        end interface
