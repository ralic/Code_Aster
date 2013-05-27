subroutine nmnoli(result, sddisc, sderro, carcri, sdimpr,&
                  sdcrit, fonact, sddyna, sdpost, modele,&
                  mate, carele, lisch2, sdpilo, sdtime,&
                  sdener, sdieto, sdcriq)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include      'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/gnomsd.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/isfonc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/nmarch.h'
    include 'asterfort/nmetpl.h'
    include 'asterfort/rscrsd.h'
    include 'asterfort/rsrusd.h'
    include 'asterfort/u2mess.h'
    character(len=19) :: sddisc, sdcrit, lisch2, sddyna, sdpost, sdpilo, sdener
    character(len=24) :: sderro, carcri
    character(len=24) :: modele, mate, carele
    character(len=24) :: sdieto, sdtime, sdcriq, sdimpr
    character(len=8) :: result
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (SD EVOL_NOLI)
!
! PREPARATION DE LA SD EVOL_NOLI
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM DE LA SD RESULTAT
! IN  NOMA   : NOM DU MAILLAGE
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  SDIMPR : SD AFFICHAGE
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDIETO : SD GESTION IN ET OUT
! IN  SDCRIT : INFORMATIONS RELATIVES A LA CONVERGENCE
! IN  SDPILO : SD PILOTAGE
! IN  SDTIME : SD TIMER
! IN  SDERRO : SD ERREUR
! IN  SDENER : SD ENERGIE
! IN  SDCRIQ : SD CRITERE QUALITE
! IN  MODELE : NOM DU MODELE
! IN  MATE   : CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  LISCH2 : NOM DE LA SD INFO CHARGE POUR STOCKAGE DANS LA SD
!              RESULTAT
!
! ----------------------------------------------------------------------
!
    character(len=24) :: arcinf
    integer :: jarinf
    character(len=19) :: sdarch
    integer :: numarc, numins
    integer :: ifm, niv
    character(len=24) :: noobj
    logical :: lreuse
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> PREPARATION DE LA SD EVOL_NOLI'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    lreuse = isfonc(fonact,'REUSE')
!
! --- INSTANT INITIAL
!
    numins = 0
!
! --- DETERMINATION DU NOM DE LA SD INFO_CHARGE STOCKEE
! --- DANS LA SD RESULTAT
!
    noobj = '12345678'//'.1234'//'.EXCIT'
    call gnomsd(' ', noobj, 10, 13)
    lisch2 = noobj(1:19)
!
! --- ACCES SD ARCHIVAGE
!
    sdarch = sddisc(1:14)//'.ARCH'
    arcinf = sdarch(1:19)//'.AINF'
!
! --- NUMERO ARCHIVAGE COURANT
!
    call jeveuo(arcinf, 'L', jarinf)
    numarc = zi(jarinf+1-1)
!
! --- CREATION DE LA SD EVOL_NOLI OU NETTOYAGE DES ANCIENS NUMEROS
!
    if (lreuse) then
        call assert(numarc.ne.0)
        call rsrusd(result, numarc)
    else
        call assert(numarc.eq.0)
        call rscrsd('G', result, 'EVOL_NOLI', 100)
    endif
!
! --- ARCHIVAGE ETAT INITIAL
!
    if (.not.lreuse) then
        call u2mess('I', 'ARCHIVAGE_4')
        call nmarch(result, numins, modele, mate, carele,&
                    fonact, carcri, sdimpr, sddisc, sdpost,&
                    sdcrit, sdtime, sderro, sddyna, sdpilo,&
                    sdener, sdieto, sdcriq, lisch2)
    endif
!
! --- AU PROCHAIN ARCHIVAGE, SAUVEGARDE DES CHAMPS AU TEMPS T+
!
    call nmetpl(sdieto)
!
    call jedema()
!
end subroutine
