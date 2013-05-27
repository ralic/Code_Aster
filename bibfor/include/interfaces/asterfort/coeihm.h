        interface
          subroutine coeihm(option,perman,resi,rigi,imate,compor,crit,&
     &instam,instap,nomail,ndim,dimdef,dimcon,nbvari,yamec,yap1,yap2,&
     &yate,nbpha1,nbpha2,addeme,adcome,addep1,adcp11,adcp12,addlh1,&
     &adcop1,addep2,adcp21,adcp22,adcop2,addete,adcote,defgem,defgep,kpi&
     &,npg,npi,sigm,sigp,varim,varip,res,drde,retcom)
            integer :: nbvari
            integer :: dimcon
            integer :: dimdef
            character(len=16) :: option
            logical :: perman
            logical :: resi
            logical :: rigi
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: crit(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            character(len=8) :: nomail
            integer :: ndim
            integer :: yamec
            integer :: yap1
            integer :: yap2
            integer :: yate
            integer :: nbpha1
            integer :: nbpha2
            integer :: addeme
            integer :: adcome
            integer :: addep1
            integer :: adcp11
            integer :: adcp12
            integer :: addlh1
            integer :: adcop1
            integer :: addep2
            integer :: adcp21
            integer :: adcp22
            integer :: adcop2
            integer :: addete
            integer :: adcote
            real(kind=8) :: defgem(1:dimdef)
            real(kind=8) :: defgep(1:dimdef)
            integer :: kpi
            integer :: npg
            integer :: npi
            real(kind=8) :: sigm(dimcon)
            real(kind=8) :: sigp(dimcon)
            real(kind=8) :: varim(nbvari)
            real(kind=8) :: varip(nbvari)
            real(kind=8) :: res(dimdef)
            real(kind=8) :: drde(dimdef,dimdef)
            integer :: retcom
          end subroutine coeihm
        end interface
