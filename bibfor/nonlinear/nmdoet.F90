subroutine nmdoet(modele, compor, fonact, numedd, sdpilo,&
                  sddyna, sdcriq, sdieto, solalg, lacc0,&
                  instin)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/r8vide.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/isfonc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/ndloam.h'
    include 'asterfort/ndynlo.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/nmdoin.h'
    include 'asterfort/nmetl1.h'
    include 'asterfort/nmetl2.h'
    include 'asterfort/nmetl3.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vtcopy.h'
    real(kind=8) :: instin
    character(len=24) :: modele, compor, sdcriq
    character(len=24) :: numedd
    character(len=24) :: sdieto
    character(len=19) :: sddyna, sdpilo
    character(len=19) :: solalg(*)
    integer :: fonact(*)
    logical :: lacc0, lener
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! SAISIE DES CHAMPS A L'ETAT INITIAL
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  COMPOR : CARTE COMPORTEMENT
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  NUMEDD : NUME_DDL
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  SDPILO : SD DE PILOTAGE
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDCRIQ : SD CRITERE QUALITE
! IN  SDIETO : SD GESTION IN ET OUT
! OUT LACC0  : .TRUE. SI ACCEL. INITIALE A CALCULER
! OUT INSTIN : INSTANT INITIAL
!                R8VIDE SI NON DEFINI
!
! ----------------------------------------------------------------------
!
    character(len=24) :: ioinfo, iolcha
    integer :: jioinf, jiolch
    integer :: nbcham, zioch
    character(len=24) :: nomchs
    logical :: evonol, leinit
    integer :: neq, nocc, numein, iret, ibid, i
    integer :: jpltk, icham
    character(len=8) :: k8bid
    character(len=8) :: calcri, result
    character(len=16) :: motfac
    character(len=24) :: evol
    character(len=24) :: typpil, typsel
    character(len=19) :: depold
    character(len=24) :: champ1, champ2, dep2, dep1, errthm
    integer :: jdep1, jdep2, jdepol, jplir, jinst, jerrt
    logical :: lpilo, lpiarc, lctcc
    logical :: lexge, lreuse, lerrt
    logical :: lzero
    real(kind=8) :: coefav
    integer :: ifm, niv
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
!
! --- INITIALISATIONS
!
    dep1 = '&&CNPART.CHP1'
    dep2 = '&&CNPART.CHP2'
    lacc0 = .false.
    lpiarc = .false.
    motfac = 'ETAT_INIT'
    call dismoi('F', 'NB_EQUA', numedd, 'NUME_DDL', neq,&
                k8bid, iret)
!
! --- ON VERIFIE QUE LE MODELE SAIT CALCULER UNE RIGIDITE
!
    call dismoi('F', 'CALC_RIGI', modele, 'MODELE', ibid,&
                calcri, iret)
    if (calcri .ne. 'OUI') call u2mesk('F', 'CALCULEL2_65', 1, modele)
!
! --- ACCES SD IN ET OUT
!
    ioinfo = sdieto(1:19)//'.INFO'
    iolcha = sdieto(1:19)//'.LCHA'
    call jeveuo(ioinfo, 'L', jioinf)
    call jeveuo(iolcha, 'L', jiolch)
    zioch = zi(jioinf+4-1)
    nbcham = zi(jioinf+1-1)
!
! --- FONCTIONNALITES ACTIVEES
!
    lpilo = isfonc(fonact,'PILOTAGE')
    lctcc = isfonc(fonact,'CONT_CONTINU')
    lexge = ndynlo(sddyna,'EXPL_GENE')
    lreuse = isfonc(fonact,'REUSE')
    lerrt = isfonc(fonact,'ERRE_TEMPS_THM')
    lener = isfonc(fonact,'ENERGIE')
!
! --- EXTRACTION VARIABLES CHAPEAUX
!
    call nmchex(solalg, 'SOLALG', 'DEPOLD', depold)
!
! --- PILOTAGE LONGUEUR D'ARC AVEC ANGL_INCR_DEPL: IL FAUT LES DEUX
! --- DERNIERS DEPLACEMENTS POUR QUE CA MARCHE (CHAMP DEPOLD)
!
    if (lpilo) then
        call jeveuo(sdpilo(1:19)//'.PLTK', 'L', jpltk)
        typpil = zk24(jpltk)
        typsel = zk24(jpltk+5)
        lpiarc = .false.
        if (typpil .eq. 'LONG_ARC' .or. typpil .eq. 'SAUT_LONG_ARC') then
            if (typsel(1:14) .eq. 'ANGL_INCR_DEPL') then
                lpiarc = .true.
            endif
        endif
    endif
!
! --- PAS D'ETAT INITIAL EN PRESENCE D'UN CONCEPT REENTRANT
!
    call getfac(motfac, nocc)
    call assert(nocc.le.1)
    leinit = nocc.gt.0
    if (leinit) then
        call u2mess('I', 'ETATINIT_10')
        if (lener) then
            call u2mess('I', 'ETATINIT_5')
        endif
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> LECTURE ETAT INITIAL'
        endif
    else
        if (lreuse) then
            call u2mess('A', 'ETATINIT_1')
        else
            call u2mess('I', 'ETATINIT_20')
        endif
    endif
!
! --- CONCEPT EVOL_NOLI DONNE DANS ETAT_INIT
!
    call getvid(motfac, 'EVOL_NOLI', 1, iarg, 1,&
                evol, nocc)
    call assert(nocc.le.1)
    evonol = nocc .gt. 0
!
! --- ALARME SI CONTACT CONTINU AVEC UN CONCEPT REENTRANT
!
    if (lctcc) then
        if (lreuse) then
            if (.not.isfonc(fonact,'CONTACT_INIT')) then
                call u2mess('A', 'MECANONLINE4_14')
            endif
        else if (evonol) then
            if (.not.isfonc(fonact,'CONTACT_INIT')) then
                call u2mess('A', 'MECANONLINE4_15')
            endif
        endif
    endif
!
! --- INSTANT INITIAL
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... INSTANT INITIAL'
    endif
    call nmdoin(evol, evonol, instin, numein)
    if (niv .ge. 2) then
        if (instin .eq. r8vide()) then
            write (ifm,*) '<MECANONLINE> ...... NON DEFINI PAR ETAT_INIT'
        else
            write (ifm,*) '<MECANONLINE> ...... VALEUR    : ',instin
        endif
    endif
!
! --- BOUCLE SUR LES CHAMPS A LIRE
!
    do 10 icham = 1, nbcham
        result = evol(1:8)
!
! ----- LECTURE DU CHAMP - ETAT_INIT/SDRESU
!
        if (evonol) call nmetl1(result, numein, sdieto, icham)
!
! ----- LECTURE DU CHAMP - CHAMP PAR CHAMP
!
        call nmetl2(motfac, sdieto, icham)
!
! ----- VERIFICATIONS SUR LE CHAMP
!
        call nmetl3(modele, compor, evonol, result, numein,&
                    sdieto, leinit, icham)
!
10  end do
!
! --- VERIFICATION COMPATIBILITE PILOTAGE
!
    if (evonol .and. lpiarc) then
        call rsexch(' ', result, 'DEPL', numein, champ1,&
                    iret)
        call rsexch(' ', result, 'DEPL', numein-1, champ2,&
                    iret)
        if (iret .ne. 0) then
            call u2mesk('F', 'MECANONLINE4_47', 1, evol)
        endif
        call vtcopy(champ1, dep1, 'F', iret)
        call vtcopy(champ2, dep2, 'F', iret)
        call jeveuo(dep1(1:19)//'.VALE', 'L', jdep1)
        call jeveuo(dep2(1:19)//'.VALE', 'L', jdep2)
        call jeveuo(depold(1:19)//'.VALE', 'E', jdepol)
        do 156 i = 1, neq
            zr(jdepol-1+i) = zr(jdep1-1+i) - zr(jdep2-1+i)
156      continue
        call jeveuo(sdpilo(1:19)//'.PLIR', 'E', jplir)
        call rsadpa(result, 'L', 1, 'COEF_MULT', numein,&
                    0, jinst, k8bid)
        coefav = zr(jinst)
        if (coefav .ne. 0.d0 .and. coefav .ne. r8vide()) then
            zr(jplir+5) = coefav
        endif
    endif
!
! --- LECTURE DES INDICATEURS D'ERREUR EN TEMPS EN THM
!
    if (evonol .and. lerrt) then
        errthm = sdcriq(1:19)//'.ERRT'
        call jeveuo(errthm, 'E', jerrt)
        call rsadpa(result, 'L', 1, 'ERRE_TPS_LOC', numein,&
                    0, jinst, k8bid)
        zr(jerrt-1+1) = zr(jinst)
        call rsadpa(result, 'L', 1, 'ERRE_TPS_GLOB', numein,&
                    0, jinst, k8bid)
        zr(jerrt-1+2) = zr(jinst)
!
    endif
!
! --- CAS DE LA DYNAMIQUE: VITESSE ET ACCELERATION INITIALES
!
    do 30 icham = 1, nbcham
        nomchs = zk24(jiolch+zioch*(icham-1)+1-1)
        lzero = zk24(jiolch+zioch*(icham-1)+4-1).eq.'ZERO'
        if (nomchs .eq. 'VITE') then
            if (lzero) then
                call u2mess('I', 'MECANONLINE4_22')
            endif
        endif
        if (nomchs .eq. 'ACCE') then
            if (lzero) then
                call u2mess('I', 'MECANONLINE4_23')
                lacc0 = .true.
            endif
        endif
30  end do
!
! --- PROJECTION MODALE EN EXPLICITE
!
    if (lexge) then
        result = evol(1:8)
        call ndloam(sddyna, result, evonol, numein)
    endif
!
    call jedema()
end subroutine
