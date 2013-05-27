        interface
          subroutine coeime(meca,imate,nomail,option,resi,rigi,ndim,&
     &dimdef,dimcon,yap1,yap2,yate,addeme,addep1,addep2,nbvari,advime,&
     &advico,npg,npi,defgep,defgem,sigm,sigp,varim,varip,ouvh,tlint,drde&
     &,kpi,vicphi,unsurn,retcom)
            integer :: nbvari
            integer :: dimcon
            integer :: dimdef
            integer :: ndim
            character(len=16) :: meca
            integer :: imate
            character(len=8) :: nomail
            character(len=16) :: option
            logical :: resi
            logical :: rigi
            integer :: yap1
            integer :: yap2
            integer :: yate
            integer :: addeme
            integer :: addep1
            integer :: addep2
            integer :: advime
            integer :: advico
            integer :: npg
            integer :: npi
            real(kind=8) :: defgep(dimdef)
            real(kind=8) :: defgem(dimdef)
            real(kind=8) :: sigm(dimcon)
            real(kind=8) :: sigp(dimcon)
            real(kind=8) :: varim(nbvari)
            real(kind=8) :: varip(nbvari)
            real(kind=8) :: ouvh
            real(kind=8) :: tlint
            real(kind=8) :: drde(dimdef,dimdef)
            integer :: kpi
            integer :: vicphi
            real(kind=8) :: unsurn
            integer :: retcom
          end subroutine coeime
        end interface
