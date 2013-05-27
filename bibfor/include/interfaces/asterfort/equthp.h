        interface
          subroutine equthp(imate,option,ndim,compor,typmod,kpi,npg,&
     &dimdef,dimcon,nbvari,defgem,congem,vintm,defgep,congep,vintp,&
     &mecani,press1,press2,tempe,crit,rinstm,rinstp,r,drds,dsde,retcom)
            integer :: nbvari
            integer :: dimcon
            integer :: dimdef
            integer :: imate
            character(len=16) :: option
            integer :: ndim
            character(len=16) :: compor(*)
            character(len=8) :: typmod(2)
            integer :: kpi
            integer :: npg
            real(kind=8) :: defgem(1:dimdef)
            real(kind=8) :: congem(1:dimcon)
            real(kind=8) :: vintm(1:nbvari)
            real(kind=8) :: defgep(1:dimdef)
            real(kind=8) :: congep(1:dimcon)
            real(kind=8) :: vintp(1:nbvari)
            integer :: mecani(5)
            integer :: press1(7)
            integer :: press2(7)
            integer :: tempe(5)
            real(kind=8) :: crit(*)
            real(kind=8) :: rinstm
            real(kind=8) :: rinstp
            real(kind=8) :: r(1:dimdef+1)
            real(kind=8) :: drds(1:dimdef+1,1:dimcon)
            real(kind=8) :: dsde(1:dimcon,1:dimdef)
            integer :: retcom
          end subroutine equthp
        end interface
