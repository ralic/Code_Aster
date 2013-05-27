        interface
          subroutine mdnewm(nbpas,dt,nbmode,pulsat,pulsa2,masgen,&
     &riggen,rgygen,lamor,amogen,gyogen,foncv,fonca,typbas,basemo,tinit,&
     &iparch,depsto,vitsto,accsto,iorsto,temsto,nomres,nbexci,idescf,&
     &nomfon,coefm,liad,inumor,passto)
            integer :: nbpas
            real(kind=8) :: dt
            integer :: nbmode
            real(kind=8) :: pulsat(*)
            real(kind=8) :: pulsa2(*)
            real(kind=8) :: masgen(*)
            real(kind=8) :: riggen(*)
            real(kind=8) :: rgygen(*)
            logical :: lamor
            real(kind=8) :: amogen(*)
            real(kind=8) :: gyogen(*)
            character(len=8) :: foncv
            character(len=8) :: fonca
            character(len=16) :: typbas
            character(len=8) :: basemo
            real(kind=8) :: tinit
            integer :: iparch(*)
            real(kind=8) :: depsto(*)
            real(kind=8) :: vitsto(*)
            real(kind=8) :: accsto(*)
            integer :: iorsto(*)
            real(kind=8) :: temsto(*)
            character(len=8) :: nomres
            integer :: nbexci
            integer :: idescf(*)
            character(len=8) :: nomfon(*)
            real(kind=8) :: coefm(*)
            integer :: liad(*)
            integer :: inumor(*)
            real(kind=8) :: passto(*)
          end subroutine mdnewm
        end interface
