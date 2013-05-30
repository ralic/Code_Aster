subroutine op0151()
    implicit none
!       ----------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! TOLE CRP_20
!       ----------------------------------------------------------------
!       C A L C _ F A T I G U E
!       ----------------------------------------------------------------
!       CREATION D UN CHAM_ELEM D ISODOMMAGE A LA FATIGUE
!       D UN MATERIAU SOUMIS A UN CYCLAGE EN CONTRAINTES
!       A PARTIR D'UN CHAM_ELEM DE GRANDEUR 1D EQUIVALENTE
!       ----------------------------------------------------------------
!       ATTENTION : LE CHAM_ELEM EN SORTIE EST CONSTRUIT ARTIFICIELEMENT
!                   SANS PASSER PAR LA ROUTINE CALCUL
!                   A PARTIR DU CHAM_ELEM DANS LA SD RESULTAT
!                   DE TYPE EVOL_NOLI , EVOL_ELAS , DYNA_TRANS, CREE
!                   PAR CALC_CHAMP (OPTIONS SIEQ(EPEQ)_ELNO(ELGA))
!                   LA COHERENCE DU CHAMP EST DONC LIEE A CE DERNIER
!                   (MEMES ELEMENTS/GREL POUR LE CALCUL DE L OPTION ...)
!
!       IMPLANTE  : ACTUELLEMENT  :
!                  - CALCUL DU DOMMAGE A PARTIR DE   = /CONTRAINTE
!                                                      /DEFORMATION
!                  - POINTS DE CALCUL DU DOMMAGE     = /NOEUDS
!                                                      /POINTS DE GAUSS
!                  - COMPOSANTES GRANDEUR EQUIVALENTE= VMIS_SG....
!                  - METHODE  D'EXTRACTION DES PICS  = RAINFLOW
!                  - METHODE  DE COMPTAGE DES CYCLES = RAINFLOW
!                  - METHODE  DE CALCUL   DU DOMMAGE = /WOHLER
!                                                      /MANSON_COFFIN
!                                                      /TAHERI_MANSON
!                                                      /TAHERI_MIXTE
!                  - OPTIONS OUT                     = /DOMA_ELNO_SIGM
!                                                      /DOMA_ELGA_SIGM
!                                                      /DOMA_ELGA_EPSI
!                                                      /DOMA_ELNO_EPSI
!                                                      /DOMA_ELGA_EPME
!                                                      /DOMA_ELNO_EPME
!       ----------------------------------------------------------------
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/alchml.h'
    include 'asterfort/anacri.h'
    include 'asterfort/assert.h'
    include 'asterfort/dmgmod.h'
    include 'asterfort/fgvdmg.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeimpo.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/paqmai.h'
    include 'asterfort/paqnoe.h'
    include 'asterfort/rccome.h'
    include 'asterfort/rcpare.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
!       ---------------------------------------------------------------
    integer :: icodre, icodwo, icodba, icodhs, icodma
    character(len=8) :: nomu, nomres, nommai, k8b, nommat
    character(len=8) :: nomfon, nomnap, cara, nommod, grdvie, inscri
    character(len=16) :: concep, cmd, pheno, phenom, typcal, nomcri, nommet
    character(len=16) :: proaxe, nomsym, typcha, nomopt, nomgde, nomfor
    character(len=16) :: forvie
    character(len=16) :: mexpic, mcompt, mdomag, typeq, typoi, typdg, option
    character(len=19) :: nomsd, chelem, chelrs, ligrel, nomsd2
    character(len=24) :: valk(6)
    logical :: fordef, crsigm, crepst, crepse, crepsp
    real(kind=8) :: rbid
    real(kind=8) :: prec, instic
!
    integer :: nval, impr, ifm, jordr, jcoef, jcelk, jcelv
    integer :: nbpt, nbord, nbcmp, numcmp(6), ntcmp, ibid
    integer :: ivdmg, numsym, nbpt2, nbord2, iret, ivch
    integer :: vali(2), paract(30), iarg
!
! ------------
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    nomfon = ' '
    nomnap = ' '
!
! ----- DONNEES UTILISATEUR
!
    call getres(nomu, concep, cmd)
!
! ---   TYPE DE CALCUL
    call getvtx(' ', 'TYPE_CALCUL', 1, iarg, 1,&
                typcal, nval)
!
! ---------------------------------------------------------------------
! ---- FATIGUE MULTIAXIALE
! ---------------------------------------------------------------------
!
    if (typcal(1:13) .eq. 'FATIGUE_MULTI') then
!
! ---   TYPE DU CHARGEMENT APPLIQUE (PERIODIQUE OU NON_PERIODIQUE)
!
        call getvtx(' ', 'TYPE_CHARGE', 1, iarg, 1,&
                    typcha, nval)
!
! ---   NOM DE L'OPTION (CALCUL AUX POINTS DE GAUSS OU AUX NOEUDS
!                        CHAM_NO)
        call getvtx(' ', 'OPTION', 1, iarg, 1,&
                    nomopt, nval)
!
! ---   STRUCTURE RESULTAT CONTENANT LES CHAM_ELEMS DE SIGMA
!       OU LES CHAM_NOS DE SIGMA
        call getvid(' ', 'RESULTAT', 1, iarg, 1,&
                    nomres, nval)
        nomsd = nomres
!
! ---   NOM DU CRITERE
        call getvtx(' ', 'CRITERE', 1, iarg, 1,&
                    nomcri, nval)
!
        call getvid(' ', 'FORMULE_GRDEQ', 1, iarg, 1,&
                    nomfor, nval)
        if (nval .eq. 0) then
            nomfor = '        '
        endif
!
        call getvtx(' ', 'COURBE_GRD_VIE', 1, iarg, 1,&
                    grdvie, nval)
        if (nval .eq. 0) then
            grdvie = '        '
        endif
!
        call getvid(' ', 'FORMULE_VIE', 1, iarg, 1,&
                    forvie, nval)
        if (nval .eq. 0) then
            forvie = '        '
        endif
!
! ---   NOM DE LA METHODE PERMETTANT DE DETERMINER LE CERCLE CIRCONSCRIT
        call getvtx(' ', 'METHODE', 1, iarg, 1,&
                    nommet, nval)
        if (nval .eq. 0) then
            nommet = '        '
        endif
!
! ---   PROJECTION SUR UN AXE OU SUR DEUX AXES
!       (CHARGEMENT NON_PERIODIQUE UNIQUEMENT)
        call getvtx(' ', 'PROJECTION', 1, iarg, 1,&
                    proaxe, nval)
        if (nval .eq. 0) then
            proaxe = '        '
        endif
!
! ---   NOM DU MAILLAGE
        call getvid(' ', 'MAILLAGE', 1, iarg, 1,&
                    nommai, nval)
        if (nval .eq. 0) then
            nommai = '        '
        endif
!
! ----- INSTANT INITIAL DU PARTIE DE CHARGEMENT CYCLIQUE
!
        prec = 1.d-6
        inscri = '        '
        instic = 0.d0
!
        call getvr8(' ', 'INST_INIT_CYCL', 1, iarg, 1,&
                    instic, nval)
        call getvtx(' ', 'INST_CRIT', 1, iarg, 1,&
                    inscri, nval)
        call getvr8(' ', 'PRECISION', 1, iarg, 1,&
                    prec, nval)
!
!
!---    ANALYSER LE CRITERE
! C  INITIALISER
        crsigm = .false.
        crepst = .false.
        crepse = .false.
        crepsp = .false.
        call anacri(nomcri, nomfor, typcha, 'OUI', paract,&
                    fordef, crsigm, crepst, crepse, crepsp)
!
!   FORDEF EST UNE BOOLEAN QUI INDIQUE S'IL EXISTE LE PARAMETRE
!   DE DEFORMATION DAS LA FORMULE (COMME DANS FATEMISOCIE)
!
        if (nomopt .eq. 'DOMA_ELGA') then
!
! ---   CONSTRUCTION DES PAQUETS DE MAILLES
            call paqmai(nomres, nomu, nommai, nommet, nomcri,&
                        nomfor, grdvie, forvie, fordef, typcha,&
                        proaxe, instic, inscri, prec)
!
        else if (nomopt .eq. 'DOMA_NOEUD') then
!
! ---   CONSTRUCTION DES PAQUETS DE NOEUDS
            call paqnoe(nomres, nomu, nommai, nommet, nomcri,&
                        nomfor, grdvie, forvie, fordef, typcha,&
                        proaxe, instic, inscri, prec)
        endif
!
!
        goto 7777
    endif
!
! ---------------------------------------------------------------------
! ---- CAS GENERAL (CUMUL DE DOMMAGE OU FATIGUE MODALE)
! ---------------------------------------------------------------------
!
! ---   NOM DE LA GRANDEUR EQUIVALENTE
    call getvtx('HISTOIRE', 'EQUI_GD', 1, iarg, 1,&
                nomgde, nval)
!
! ---   IMPRESSIONS
    call getvis(' ', 'INFO', 1, iarg, 1,&
                impr, nval)
!
! ---   CHAMP : NOM DE L'OPTION RESULTANTE
!   'DOMA_ELNO_SIGM'/'DOMA_ELGA_SIGM'/'DOMA_ELNO_EPSI'/'DOMA_ELGA_EPSI
!   'DOMA_ELNO_EPME'/'DOMA_ELGA_EPME'
!
    call getvtx(' ', 'OPTION', 1, iarg, 1,&
                nomopt, nval)
!
! ---   NOM DE LA METHODE DE CALCUL DU DOMMAGE
    call getvtx(' ', 'DOMMAGE', 1, iarg, 1,&
                mdomag, nval)
    call getvid(' ', 'MATER', 1, iarg, 1,&
                nommat, nval)
!
! ---   NOMBRE DE NUMEROS D ORDRE DE POINTS (NOEUDS/PG) DE CMPS ...
!       ---------------------------------------------------------------
!       NOM SYMBOLIQUE DE L OPTION IN UTILISEE (GRANDEUR EQUIVALENTE)
!       (SIEQ_ELNO  SIEQ_ELGA  EPEQ_ELGA EPEQ_ELNO
!        EPMQ_ELNO  EPMQ_ELGA  )
!       ET NOMBRE TOTAL DE COMPOSANTES DE CETTE OPTION
!
    if (nomopt(11:14) .eq. 'SIGM') then
        nomsym='SIEQ_'//nomopt(6:9)
    else if (nomopt(11:14).eq.'EPSI') then
        nomsym='EPEQ_'//nomopt(6:9)
    else if (nomopt(11:14).eq.'EPME') then
        nomsym='EPMQ_'//nomopt(6:9)
    else
        call assert(.false.)
    endif
!
!
    if (nomopt(6:14) .eq. 'ELGA_SIGM') then
        ntcmp = 17
    else if (nomopt(6:14) .eq. 'ELNO_SIGM') then
        ntcmp = 17
    else if (nomopt(6:14) .eq. 'ELGA_EPSI') then
        ntcmp = 14
    else if (nomopt(6:14) .eq. 'ELNO_EPSI') then
        ntcmp = 14
    else if (nomopt(6:14) .eq. 'ELGA_EPME') then
        ntcmp = 14
    else if (nomopt(6:14) .eq. 'ELNO_EPME') then
        ntcmp = 14
    else
        call assert(.false.)
    endif
!
!       TYPE DE GRANDEUR EQUIVALENTE UTILISEE LE POUR CALCUL DU DOMMAGE
!       ET NOMBRE DE COMPOSANTES DE CETTE GRANDEUR
!       VMIS_SIG   = CMP NUMERO 6       DE  L OPTION EQUI_...._SIGM
!       INVA_2_SG  = CMP NUMERO 5       DE  L OPTION EQUI_...._EPSI
!       INVA_2_SG  = CMP NUMERO 5       DE  L OPTION EQUI_...._EPME
!
    if (nomgde(1:7) .eq. 'VMIS_SG') then
        numcmp(1) = 6
        nbcmp = 1
    else if (nomgde(1:9).eq.'INVA_2_SG') then
        numcmp(1) = 5
        nbcmp = 1
    endif
!
! ---------------------------------------------------------------------
! ---- CUMUL DE DOMMAGE
! ---------------------------------------------------------------------
!
    if (typcal(1:13) .eq. 'CUMUL_DOMMAGE') then
!
! ---   STRUCTURE RESULTAT CONTENANT LES CHAM_ELEMS DE SIGMA EQUIVALENT
        call getvid('HISTOIRE', 'RESULTAT', 1, iarg, 1,&
                    nomres, nval)
        nomsd = nomres
!
        if (mdomag .eq. 'WOHLER') then
            if (nomopt(11:14) .ne. 'SIGM') then
                call u2mesk('F', 'FATIGUE1_29', 1, nomopt)
            endif
            pheno = 'FATIGUE'
            call rccome(nommat, pheno, phenom, icodre)
            if (icodre .eq. 1) call u2mess('F', 'FATIGUE1_24')
            cara = 'WOHLER'
            call rcpare(nommat, pheno, cara, icodwo)
            cara = 'A_BASQUI'
            call rcpare(nommat, pheno, cara, icodba)
            cara = 'A0'
            call rcpare(nommat, pheno, cara, icodhs)
            if (icodwo .ne. 0 .and. icodba .ne. 0 .and. icodhs .ne. 0) call u2mess('F',&
                                                                                   'FATIGUE1_30')
!
        else if (mdomag.eq.'MANSON_COFFIN') then
            if (nomopt(11:14) .ne. 'EPSI' .and. nomopt(11:14) .ne. 'EPME') then
                call u2mesk('F', 'FATIGUE1_31', 1, nomopt)
            endif
            pheno = 'FATIGUE'
            call rccome(nommat, pheno, phenom, icodre)
            if (icodre .eq. 1) call u2mess('F', 'FATIGUE1_24')
            cara = 'MANSON_C'
            call rcpare(nommat, pheno, cara, icodma)
            if (icodma .ne. 0) call u2mess('F', 'FATIGUE1_32')
!
        else if (mdomag.eq.'TAHERI_MANSON') then
            if (nomopt(11:14) .ne. 'EPSI' .and. nomopt(11:14) .ne. 'EPME') then
                call u2mesk('F', 'FATIGUE1_25', 1, nomopt)
            endif
            pheno = 'FATIGUE'
            call rccome(nommat, pheno, phenom, icodre)
            if (icodre .eq. 1) call u2mess('F', 'FATIGUE1_24')
            cara = 'MANSON_C'
            call rcpare(nommat, pheno, cara, icodma)
            if (icodma .ne. 0) call u2mess('F', 'FATIGUE1_32')
            call getvid(' ', 'TAHERI_NAPPE', 1, iarg, 1,&
                        nomnap, nval)
            if (nval .eq. 0) then
                call u2mess('F', 'FATIGUE1_26')
            endif
            call getvid(' ', 'TAHERI_FONC', 1, iarg, 1,&
                        nomfon, nval)
            if (nval .eq. 0) then
                call u2mess('F', 'FATIGUE1_27')
            endif
!
        else if (mdomag.eq.'TAHERI_MIXTE') then
            if (nomopt(11:14) .ne. 'EPSI' .and. nomopt(11:14) .ne. 'EPME') then
                call u2mesk('F', 'FATIGUE1_28', 1, nomopt)
            endif
            pheno = 'FATIGUE'
            call rccome(nommat, pheno, phenom, icodre)
            if (icodre .eq. 1) call u2mess('F', 'FATIGUE1_24')
            cara = 'MANSON_C'
            call rcpare(nommat, pheno, cara, icodma)
            if (icodma .ne. 0) call u2mess('F', 'FATIGUE1_32')
            cara = 'WOHLER'
            call rcpare(nommat, pheno, cara, icodwo)
            cara = 'A_BASQUI'
            call rcpare(nommat, pheno, cara, icodba)
            cara = 'A0'
            call rcpare(nommat, pheno, cara, icodhs)
            if (icodwo .ne. 0 .and. icodba .ne. 0 .and. icodhs .ne. 0) call u2mess('F',&
                                                                                   'FATIGUE1_30')
            call getvid(' ', 'TAHERI_NAPPE', 1, iarg, 1,&
                        nomnap, nval)
            if (nval .eq. 0) then
                call u2mess('F', 'FATIGUE1_26')
            endif
!
        endif
!
! --- VERIFICATION DU NOMBRE DE PAS DE TEMPS
        call jelira(nomsd//'.ORDR', 'LONUTI', nbord, k8b)
        if (nbord .lt. 2) then
            call u2mesi('F', 'FATIGUE1_76', 1, nbord)
        endif
!
        call jenonu(jexnom(nomsd//'.DESC', nomsym), numsym)
        if (numsym .eq. 0) then
            valk(1) = nomsym
            valk(2) = nomsd
            call u2mesk('F', 'PREPOST4_5', 2, valk)
        endif
        call jeveuo(jexnum(nomsd//'.TACH', numsym), 'L', ivch)
        chelrs = zk24(ivch)(1:19)
        if (chelrs .eq. ' ') then
            valk(1) = chelrs
            valk(2) = nomsym
            valk(3) = nomsd
            call u2mesk('F', 'PREPOST4_6', 3, valk)
        endif
        call jeveuo(chelrs//'.CELK', 'L', jcelk)
        ligrel=zk24(jcelk-1+1)
        call jelira(chelrs//'.CELV', 'LONMAX', nval, k8b)
!
!  -      IL Y A NTCMP COMPOSANTES DANS L OPTION XXXX_EQUI_YYYY
        nbpt = nval / ntcmp
!
        if (impr .ge. 2) then
            vali (1) = nbord
            vali (2) = nbpt
            call u2mesg('I', 'PREPOST6_27', 0, ' ', 2,&
                        vali, 0, 0.d0)
        endif
!
! ----- CALCUL DU VECTEUR DOMMAGE EN CHAQUE NOEUD/PG
!       ----------------------------------------------------------------
!
        call wkvect('&&OP0151.DOMMAGE', 'V V R', nbpt, ivdmg)
!
        mexpic = 'RAINFLOW'
        mcompt = 'RAINFLOW'
        if (mdomag(1:6) .eq. 'TAHERI') mcompt = 'TAHERI'
!
        if (impr .ge. 2) then
            typeq = nomgde
            if (nomopt(11:14) .eq. 'SIGM') typdg = 'CONTRAINTE'
            if (nomopt(11:14) .eq. 'EPSI') typdg = 'DEFORMATION'
            if (nomopt(11:14) .eq. 'EPME') typdg = 'DEFORMATION'
            if (nomopt(6:9) .eq. 'ELNO') typoi = 'NOEUDS'
            if (nomopt(6:9) .eq. 'ELGA') typoi = 'POINTS DE GAUSS'
            valk (1) = typdg
            valk (2) = typoi
            valk (3) = typeq
            valk (4) = mexpic
            valk (5) = mcompt
            valk (6) = mdomag
            call u2mesg('I', 'PREPOST6_28', 6, valk, 0,&
                        0, 0, 0.d0)
        endif
!
        call fgvdmg(nomsym, nomsd, nommat, nomnap, nomfon,&
                    mexpic, mcompt, mdomag, nbord, nbpt,&
                    ntcmp, nbcmp, numcmp, impr, zr(ivdmg))
!
        if (impr .ge. 2) then
            ifm = iunifi('MESSAGE')
            call jeimpo(ifm, '&&OP0151.DOMMAGE', 'DOMMAGE')
        endif
!
! ---------------------------------------------------------------------
! ---- FATIGUE VIBRATOIRE
! ---------------------------------------------------------------------
!
    else if (typcal(1:13) .eq. 'FATIGUE_VIBR') then
! ---   STRUCTURE RESULTAT CONTENANT LES CHAM_ELEMS DE SIGMA EQUIVALENT
        call getvid('HISTOIRE', 'RESULTAT', 1, iarg, 1,&
                    nomres, nval)
        nomsd = nomres
        call getvid('HISTOIRE', 'MODE_MECA', 1, iarg, 1,&
                    nommod, nval)
        nomsd2 = nommod
!
        if (mdomag .eq. 'WOHLER') then
            if (nomopt(11:14) .ne. 'SIGM') then
                call u2mesk('F', 'FATIGUE1_29', 1, nomopt)
            endif
            pheno = 'FATIGUE'
            call rccome(nommat, pheno, phenom, icodre)
            if (icodre .eq. 1) call u2mesk('F', 'FATIGUE1_88', 1, 'WOHLER')
            call rcpare(nommat, pheno, 'WOHLER', icodre)
            if (icodre .eq. 1) call u2mesk('F', 'FATIGUE1_88', 1, 'WOHLER')
        endif
!
! --- CONTRAINTE STATIQUE
!
        call jelira(nomsd//'.ORDR', 'LONUTI', nbord, k8b)
        if (nbord .gt. 1) then
            call u2mesi('F', 'FATIGUE1_84', 1, nbord)
        endif
!
        call jenonu(jexnom(nomsd//'.DESC', nomsym), numsym)
        if (numsym .eq. 0) then
            valk(1) = nomsym
            valk(2) = nomsd
            call u2mesk('F', 'PREPOST4_5', 2, valk)
        endif
        call jeveuo(jexnum(nomsd//'.TACH', numsym), 'L', ivch)
        chelrs = zk24(ivch)(1:19)
        if (chelrs .eq. ' ') then
            valk(1) = chelrs
            valk(2) = nomsym
            valk(3) = nomsd
            call u2mesk('F', 'PREPOST4_6', 3, valk)
        endif
        call jeveuo(chelrs//'.CELK', 'L', jcelk)
        ligrel=zk24(jcelk-1+1)
        call jelira(chelrs//'.CELV', 'LONMAX', nval, k8b)
!
!  -      IL Y A NTCMP COMPOSANTES DANS L OPTION XXXX_EQUI_YYYY
        nbpt = nval / ntcmp
!
! --- CONTRAINTE MODALE
!
        call jenonu(jexnom(nomsd2//'.DESC', nomsym), numsym)
        if (numsym .eq. 0) then
            valk(1) = nomsym
            valk(2) = nomsd2
            call u2mesk('F', 'PREPOST4_5', 2, valk)
        endif
        call jeveuo(jexnum(nomsd2//'.TACH', numsym), 'L', ivch)
        chelrs = zk24(ivch)(1:19)
        if (chelrs .eq. ' ') then
            valk(1) = chelrs
            valk(2) = nomsym
            valk(3) = nomsd2
            call u2mesk('F', 'PREPOST4_6', 3, valk)
        endif
        call jeveuo(chelrs//'.CELK', 'L', jcelk)
        ligrel=zk24(jcelk-1+1)
        call jelira(chelrs//'.CELV', 'LONMAX', nval, k8b)
!
!  -      IL Y A NTCMP COMPOSANTES DANS L OPTION XXXX_EQUI_YYYY
        nbpt2 = nval / ntcmp
!
        if (nbpt .ne. nbpt2) then
            vali (1) = nbpt
            vali (2) = nbpt2
            call u2mesi('F', 'FATIGUE1_85', 2, vali)
        endif
!
!-- NOMBRE ET NUMERO D ORDRE
        call getvis('HISTOIRE', 'NUME_MODE', 1, iarg, 0,&
                    ibid, nbord)
        call getvr8('HISTOIRE', 'FACT_PARTICI', 1, iarg, 0,&
                    rbid, nbord2)
!
        if (nbord .ne. nbord2) then
            call u2mess('F', 'FATIGUE1_86')
        endif
!
        nbord = -nbord
        call wkvect('&&OP0151.LMODE', 'V V I', nbord, jordr)
        call getvis('HISTOIRE', 'NUME_MODE', 1, iarg, nbord,&
                    zi(jordr), ibid)
        call wkvect('&&OP0151.CMODE', 'V V R', nbord, jcoef)
        call getvr8('HISTOIRE', 'FACT_PARTICI', 1, iarg, nbord,&
                    zr(jcoef), ibid)
!
        if (impr .ge. 1) then
            typeq = nomgde
            typdg = 'CONTRAINTE'
            if (nomopt(6:9) .eq. 'ELNO') typoi = 'NOEUDS'
            if (nomopt(6:9) .eq. 'ELGA') typoi = 'POINTS DE GAUSS'
            valk (1) = typdg
            valk (2) = typoi
            valk (3) = typeq
            vali (1) = nbpt
            vali (2) = nbord
            call u2mesg('I', 'FATIGUE1_81', 3, valk, 2,&
                        vali, 0, 0.d0)
        endif
!
        call wkvect('&&OP0151.DOMMAGE', 'V V R', nbpt, ivdmg)
        call dmgmod(nomsym, nomsd, nomsd2, nommat, nbord,&
                    jordr, jcoef, nbpt, ntcmp, numcmp,&
                    impr, zr(ivdmg))
!
        call jedetr('&&OP0151.LMODE')
        call jedetr('&&OP0151.CMODE')
!
    endif
!
! ----- TRANSFORMATION DU VECTEUR DOMMAGE EN UN VRAI CHAM_ELEM
!       ----------------------------------------------------------------
!       ON ALLOUE LE CHAM_ELEM AVEC LA ROUTINE ALCHML
!       PUIS ON RECOPIE DANS .CELV LES VALEURS CALCULEES
!
    chelem = nomu
!
    option='TOU_INI_'//nomopt(6:9)
    call alchml(ligrel, option, 'PDOMMAG', 'G', chelem,&
                iret, ' ')
    call assert(iret.eq.0)
!
!
    call jeveuo(chelem//'.CELV', 'E', jcelv)
    call jelira(chelem//'.CELV', 'LONMAX', ibid, k8b)
    call assert(ibid.eq.nbpt)
    do 222 i = 1, nbpt
        zr(jcelv+i-1) = zr(ivdmg+i-1)
222  end do
    call jedetr('&&OP0151.DOMMAGE')
!
7777  continue
!
    call jedema()
end subroutine
