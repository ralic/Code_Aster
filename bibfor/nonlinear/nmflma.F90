subroutine nmflma(typmat, mod45, defo, parmet, modelz,&
                  mate, carele, sddisc, sddyna, fonact,&
                  numins, valinc, solalg, lischa, comref,&
                  defico, resoco, solveu, numedd, numfix,&
                  compor, carcri, sdstat, sdtime, meelem,&
                  measse, veelem, nddle, ddlexc, modrig,&
                  ldccvg, matass, matgeo)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
! aslint: disable=W1504
    implicit none
    include 'jeveux.h'
    include 'asterfort/ascoma.h'
    include 'asterfort/asmama.h'
    include 'asterfort/asmari.h'
    include 'asterfort/asmatr.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/isfonc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/matide.h'
    include 'asterfort/ndynlo.h'
    include 'asterfort/nmcha0.h'
    include 'asterfort/nmchai.h'
    include 'asterfort/nmchcp.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/nmcmat.h'
    include 'asterfort/nmxmat.h'
    include 'asterfort/u2mess.h'
    character(len=16) :: typmat, modrig
    character(len=4) :: mod45
    integer :: defo
    real(kind=8) :: parmet(*)
    integer :: fonact(*)
    character(len=*) :: modelz
    character(len=24) :: mate, carele
    character(len=24) :: compor, carcri
    character(len=24) :: sdtime, sdstat
    integer :: numins, ldccvg, nddle
    character(len=19) :: sddisc, sddyna, lischa, solveu
    character(len=24) :: defico, resoco
    character(len=24) :: comref, numedd, numfix, ddlexc
    character(len=19) :: meelem(*), measse(*), veelem(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=19) :: matass, matgeo
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! CALCUL DE LA MATRICE GLOBALE POUR FLAMBEMENT/MODES VIBRATOIRES
!
! ----------------------------------------------------------------------
!
!
! IN  TYPMAT : TYPE DE MATRICE DE RIGIDITE A UTILISER
!                'ELASTIQUE/TANGENTE/SECANTE'
! IN  MOD45  : TYPE DE CALCUL DE MODES PROPRES
!              'VIBR'     MODES VIBRATOIRES
!              'FLAM'     MODES DE FLAMBEMENT
! IN  DEFO   : TYPE DE DEFORMATIONS
!                0        PETITES DEFORMATIONS (MATR. GEOM.)
!                1        GRANDES DEFORMATIONS (PAS DE MATR. GEOM.)
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  DEFICO : SD DEFINITION CONTACT
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION (VOIR NMLECT)
! IN  PARMET : PARAMETRES DES METHODES DE RESOLUTION (VOIR NMLECT)
! IN  SOLVEU : SOLVEUR
! IN  CARCRI : PARAMETRES METHODES D'INTEGRATION LOCALES (VOIR NMLECT)
! IN  SDDISC : SD DISC_INST
! IN  PREMIE : SI PREMIER INSTANT DE CALCUL
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  MEELEM : MATRICES ELEMENTAIRES
! IN  MEASSE : MATRICE ASSEMBLEE
! IN  VEELEM : VECTEUR ELEMENTAIRE
! IN  NDDLE  : NOMBRE DE DDL A EXCLURE
! IN  DDLEXC : LISTE DES NOMS DES DDL A EXCLURE
! IN  MODRIG : MODIFICATION OU NON DE LA RAIDEUR
! OUT LDCCVG : CODE RETOUR INTEGRATION DU COMPORTEMENT
!                0 - OK
!                1 - ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
!                2 - ERREUR DANS LES LDC SUR LA NON VERIFICATION DE
!                    CRITERES PHYSIQUES
!                3 - SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
! OUT MATASS : MATRICE DE RAIDEUR ASSEMBLEE GLOBALE
! OUT MATGEO : DEUXIEME MATRICE ASSEMBLEE POUR LE PROBLEME AUX VP :
!              - MATRICE GEOMETRIQUE GLOBALE (CAS FLAMBEMENT)
!              - MATRICE DE RAIDEUR ASSEMBLEE GLOBALE (CAS FLAMBEMENT)
!              - MATRICE DE MASSE (CAS MODES DYNAMIQUES)
!
! ----------------------------------------------------------------------
!
    integer :: zvalin
    parameter    (zvalin=28)
!
    logical :: reasma
    logical :: lcrigi, lcfint, lmacr
    logical :: lamor
    logical :: lsuiv
    character(len=16) :: optrig, optamo
    integer :: reincr, iterat
    character(len=8) :: tdiag, syme
    character(len=24) :: codere
    character(len=19) :: rigi2, masse, amort, memass, megeom
    character(len=19) :: depplu, vitplu, accplu, sigplu, varplu, valin2(zvalin)
    integer :: nmax
    integer :: nbmatr, jexx
    character(len=6) :: ltypma(20)
    character(len=16) :: loptme(20), loptma(20), modlag
    logical :: lassme(20), lcalme(20)
    integer :: ifm, niv, ibid, iret
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ...... CALCUL MATRICE'
    endif
!
! --- INITIALISATIONS
!
    call nmcmat('INIT', ' ', ' ', ' ', .true.,&
                .true., nbmatr, ltypma, loptme, loptma,&
                lcalme, lassme)
    codere = '&&NMFLMA.CODERE'
    iterat = 0
!
! --- RECOPIE DU VECTEUR CHAPEAU
!
    call nmchai('VALINC', 'LONMAX', nmax)
    call assert(nmax.eq.zvalin)
    call nmcha0('VALINC', 'ALLINI', ' ', valin2)
    call nmchcp('VALINC', valinc, valin2)
!
! --- FONCTIONNALITES ACTIVEES
!
    lamor = ndynlo(sddyna,'MAT_AMORT')
    lsuiv = isfonc(fonact,'FORCE_SUIVEUSE')
    lmacr = isfonc(fonact,'MACR_ELEM_STAT')
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(measse, 'MEASSE', 'MEMASS', masse)
    call nmchex(measse, 'MEASSE', 'MEAMOR', amort)
    call nmchex(meelem, 'MEELEM', 'MEMASS', memass)
    call nmchex(meelem, 'MEELEM', 'MEGEOM', megeom)
!
! --- ON UTILISE CHAMP EN T+ PAS T- CAR DEPDEL=0 ET
! --- MATRICE = RIGI_MECA_TANG (VOIR FICHE 18779)
! --- SAUF VARI. COMM.
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'SIGPLU', sigplu)
    call nmchex(valinc, 'VALINC', 'VARPLU', varplu)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(valinc, 'VALINC', 'ACCPLU', accplu)
!
    call nmcha0('VALINC', 'DEPMOI', depplu, valin2)
    call nmcha0('VALINC', 'VITMOI', vitplu, valin2)
    call nmcha0('VALINC', 'VARMOI', varplu, valin2)
    call nmcha0('VALINC', 'ACCMOI', accplu, valin2)
    call nmcha0('VALINC', 'SIGMOI', sigplu, valin2)
!
! --- OBJET POUR RECONSTRUCTION RIGIDITE TOUJOURS SYMETRIQUE
!
    rigi2 = '&&NMFLMA.RIGISYME'
!
! --- PARAMETRES
!
    reincr = nint(parmet(1))
!
! --- REASSEMBLAGE DE LA MATRICE GLOBALE
!
    if ((reincr.eq.0) .and. (numins.ne.1)) then
        reasma = .false.
    endif
    if (numins .eq. 1) then
        reasma = .true.
    endif
    if ((reincr.ne.0) .and. (numins.ne.1)) then
        reasma = mod(numins-1,reincr) .eq. 0
    endif
!
! --- OPTION DE CALCUL DE MERIMO
!
    if (typmat .eq. 'TANGENTE') then
        optrig = 'RIGI_MECA_TANG'
    else if (typmat.eq.'SECANTE') then
        optrig = 'RIGI_MECA_ELAS'
    else if (typmat.eq.'ELASTIQUE') then
        optrig = 'RIGI_MECA'
    else
        optrig = 'RIGI_MECA_TANG'
!        CALL ASSERT(.FALSE.)
    endif
!
! --- A RECALCULER
!
    lcrigi = reasma
    lcfint = .false.
!
! --- CALCUL DES MATR-ELEM DE RIGIDITE
!
    if (lcrigi) then
        call nmcmat('AJOU', 'MERIGI', optrig, ' ', .true.,&
                    reasma, nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
! --- CALCUL DES MATR-ELEM D'AMORTISSEMENT DE RAYLEIGH A CALCULER
! --- NECESSAIRE SI MATR_ELEM RIGIDITE CHANGE !
!
    if (lcrigi .and. lamor) then
        optamo = 'AMOR_MECA'
        call nmcmat('AJOU', 'MEAMOR', optamo, ' ', .true.,&
                    .true., nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
! --- CALCUL DES MATR-ELEM DES CHARGEMENTS SUIVEURS
!
    if (lsuiv) then
        call nmcmat('AJOU', 'MESUIV', ' ', ' ', .true.,&
                    .false., nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
! --- CALCUL DE LA RIGIDITE GEOMETRIQUE DANS LE CAS HPP
!
    if (mod45 .eq. 'FLAM') then
        if (defo .eq. 0) then
            call nmcmat('AJOU', 'MEGEOM', ' ', ' ', .true.,&
                        .false., nbmatr, ltypma, loptme, loptma,&
                        lcalme, lassme)
        endif
    endif
!
! --- CALCUL DES MATR-ELEM DES SOUS-STRUCTURES
!
    if (lmacr) then
        call nmcmat('AJOU', 'MESSTR', ' ', ' ', .true.,&
                    .false., nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
! --- CALCUL ET ASSEMBLAGE DES MATR_ELEM DE LA LISTE
!
    if (nbmatr .gt. 0) then
        call nmxmat(modelz, mate, carele, compor, carcri,&
                    sddisc, sddyna, fonact, numins, iterat,&
                    valin2, solalg, lischa, comref, defico,&
                    resoco, solveu, numedd, numfix, sdstat,&
                    sdtime, nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme, lcfint, meelem, measse,&
                    veelem, ldccvg, codere)
    endif
!
! --- ON RECONSTRUIT RIGI2 TOUJOURS SYMETRIQUE
!
    call asmari(fonact, meelem, numedd, solveu, lischa,&
                rigi2)
    matass = rigi2
!
! --- PRISE EN COMPTE DE LA MATRICE TANGENTE DES FORCES SUIVEUSES
!
    if (reasma) then
        if (lsuiv) then
            call ascoma(meelem, numedd, solveu, lischa, matass)
        endif
    endif
!
!  --- MODIFICATION EVENTUELLE DE LA MATRICE DE RAIDEUR
!
    modlag = 'MODI_LAGR_OUI'
    tdiag = 'MAX_ABS'
    if ((nddle.ne.0) .and. (modrig(1:13).eq.'MODI_RIGI_OUI')) then
        call jeveuo(ddlexc, 'L', jexx)
        call matide(matass, nddle, zk8(jexx), modlag, tdiag,&
                    10.d0)
    endif
!
! --- CALCUL DE LA RIGIDITE GEOMETRIQUE DANS LE CAS HPP
!
    if (mod45 .eq. 'FLAM') then
        if (defo .eq. 0) then
            call asmatr(1, megeom, ' ', numedd, solveu,&
                        lischa, 'ZERO', 'V', 1, matgeo)
            if ((nddle.ne.0) .and. (modrig(1:13).eq.'MODI_RIGI_OUI')) then
                call matide(matgeo, nddle, zk8(jexx), modlag, tdiag,&
                            10.d0)
            endif
        else
            matgeo = matass
        endif
    else if (mod45 .eq. 'VIBR') then
        call asmama(memass, ' ', numedd, solveu, lischa,&
                    matgeo)
    endif
!
! --- VERIFICATION POUR MODE_VIBR QUE LES DEUX MATRICES SONT SYMETRIQUES
!
    if (mod45 .eq. 'VIBR') then
        call dismoi('F', 'TYPE_MATRICE', matass, 'MATR_ASSE', ibid,&
                    syme, iret)
        if (syme .eq. 'NON_SYM') then
            call u2mess('F', 'MECANONLINE5_56')
        else
            call dismoi('F', 'TYPE_MATRICE', matgeo, 'MATR_ASSE', ibid,&
                        syme, iret)
            if (syme .eq. 'NON_SYM') call u2mess('F', 'MECANONLINE5_56')
        endif
    endif
!
    call jedema()
!
end subroutine
