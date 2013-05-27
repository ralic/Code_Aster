        interface
          subroutine lcjohm(imate,resi,rigi,kpi,npg,nomail,addeme,&
     &advico,ndim,dimdef,dimcon,nbvari,defgem,defgep,varim,varip,sigm,&
     &sigp,drde,ouvh,retcom)
            integer :: nbvari
            integer :: dimcon
            integer :: dimdef
            integer :: imate
            logical :: resi
            logical :: rigi
            integer :: kpi
            integer :: npg
            character(len=8) :: nomail
            integer :: addeme
            integer :: advico
            integer :: ndim
            real(kind=8) :: defgem(dimdef)
            real(kind=8) :: defgep(dimdef)
            real(kind=8) :: varim(nbvari)
            real(kind=8) :: varip(nbvari)
            real(kind=8) :: sigm(dimcon)
            real(kind=8) :: sigp(dimcon)
            real(kind=8) :: drde(dimdef,dimdef)
            real(kind=8) :: ouvh
            integer :: retcom
          end subroutine lcjohm
        end interface
