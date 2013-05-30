subroutine nmstat(phase, fonact, sdstat, sdtime, sdimpr,&
                  defico)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/impfot.h'
    include 'asterfort/impmem.h'
    include 'asterfort/isfonc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmrini.h'
    include 'asterfort/nmrvai.h'
    include 'asterfort/nmtimr.h'
    include 'asterfort/obgetb.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    character(len=1) :: phase
    character(len=24) :: sdtime, sdstat, sdimpr
    character(len=24) :: defico
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! AFFICHAGE DE STATISTIQUES DIVERSES
!
! ----------------------------------------------------------------------
!
!
! IN  PHASE  : TYPE DE STATISTIQUES
!               'N' SUR L'ITERATION DE NEWTON COURANTE
!               'P' SUR LE PAS COURANT
!               'T' SUR TOUT LE TRANSITOIRE
! IN  SDSTAT : SD STATISTIQUES
! IN  SDTIME : SD TIME
! IN  SDIMPR : SD AFFICHAGE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  FONACT : FONCTIONNALITES ACTIVES
!
! ----------------------------------------------------------------------
!
    logical :: lctcd, lallv, lcont, lctcc
    logical :: lctcg, lboucc, lboucf, lfrot
    logical :: lmvib, lflam, lpost, lnewtg
    logical :: lprint
    real(kind=8) :: tpsint, tpsasm, tpsfac, tps2mb, tpssol
    real(kind=8) :: tps, tpsmoy, tpspst, tpslst, tpsrst
    real(kind=8) :: effica
    integer :: nbrint, nbrfac, nbrsol, nbrfet, nbrrel
    integer :: nbiter, nbinst
    real(kind=8) :: tpscog
    real(kind=8) :: tpscda
    real(kind=8) :: tpsccp, tpsccm, tpsccv, tpsccc, tpsccf
    integer :: ctgeom
    integer :: nbliac, nbliaf
    integer :: ctccpr, ctcmat, ctcvec, ctiter, ctcfro
    integer :: ibid
    integer :: vali(5)
    character(len=24) :: tpscvt
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- FONCTIONNALITES ACTIVES
!
    lmvib = isfonc(fonact,'MODE_VIBR')
    lflam = isfonc(fonact,'CRIT_STAB')
    lcont = isfonc(fonact,'CONTACT' )
    lpost = lmvib.or.lflam.or.lcont
    if (lcont) then
        lctcg = cfdisl(defico,'GEOM_BOUCLE')
        lallv = cfdisl(defico,'ALL_VERIF')
        lctcd = isfonc(fonact,'CONT_DISCRET')
        lctcc = isfonc(fonact,'CONT_CONTINU')
        lboucc = isfonc(fonact,'BOUCLE_EXT_CONT')
        lfrot = cfdisl(defico,'FROTTEMENT')
        lboucf = isfonc(fonact,'BOUCLE_EXT_FROT')
        lnewtg = cfdisl(defico,'GEOM_NEWTON')
    else
        lctcg = .false.
        lallv = .false.
        lctcd = .false.
        lctcc = .false.
        lboucc = .false.
        lboucf = .false.
        lfrot = .false.
        lnewtg = .false.
    endif
!
! --- INITIALISATIONS
!
    tps = 0.d0
    tpsint = 0.d0
    tpsasm = 0.d0
    tpsfac = 0.d0
    tps2mb = 0.d0
    tpssol = 0.d0
    tpscog = 0.d0
    tpscda = 0.d0
    tpsccp = 0.d0
    tpsccm = 0.d0
    tpsccv = 0.d0
    tpsccc = 0.d0
    tpsccf = 0.d0
    tpspst = 0.d0
    tpslst = 0.d0
    tpsrst = 0.d0
!
    nbiter = 0
    nbinst = 0
    nbrint = 0
    nbrfac = 0
    nbrsol = 0
    ctgeom = 0
    nbliac = 0
    nbliaf = 0
    ctccpr = 0
    ctcmat = 0
    ctcvec = 0
    ctiter = 0
    ctcfro = 0
    nbrfet = 0
    nbrrel = 0
!
! --- AFFICHAGE POUR CE PAS ?
!
    call obgetb(sdimpr, 'PRINT', lprint)
!
! --- TEMPS TOTAL DANS LA PHASE
!
    call nmtimr(sdtime, 'TEMPS_PHASE', phase, tps)
!
! --- TEMPS PERDU
!
    if (phase .eq. 'T') then
        call nmtimr(sdtime, 'PAS_LOST', phase, tpslst)
    endif
!
! --- TEMPS DES OPERATIONS DANS LA PHASE
!
    call nmtimr(sdtime, 'INTEGRATION', phase, tpsint)
    call nmtimr(sdtime, 'ASSE_MATR', phase, tpsasm)
    call nmtimr(sdtime, 'FACTOR', phase, tpsfac)
    call nmtimr(sdtime, 'SECO_MEMB', phase, tps2mb)
    call nmtimr(sdtime, 'SOLVE', phase, tpssol)
!
! --- COMPTEURS D'OPERATIONS
!
    call nmrvai(sdstat, 'INTEGRATION', phase, nbrint)
    call nmrvai(sdstat, 'FACTOR', phase, nbrfac)
    call nmrvai(sdstat, 'SOLVE', phase, nbrsol)
    call nmrvai(sdstat, 'FETI_ITER', phase, nbrfet)
    call nmrvai(sdstat, 'RECH_LINE_ITER', phase, nbrrel)
!
    if ((phase.eq.'P') .or. (phase.eq.'T')) then
        call nmrvai(sdstat, 'ITE', phase, nbiter)
    endif
!
    if (phase .eq. 'T') then
        call nmrvai(sdstat, 'PAS', phase, nbinst)
    endif
!
! --- BOUCLE GEOMETRIQUE DE CONTACT
!
    if (lctcg .or. lnewtg) then
        call nmtimr(sdtime, 'CONT_GEOM', phase, tpscog)
        call nmrvai(sdstat, 'CONT_GEOM', phase, ctgeom)
    endif
!
! --- AUTRES INFOS SUR LE CONTACT
!
    if (lcont .and. (.not.lallv)) then
        call nmrvai(sdstat, 'CONT_NBLIAC', phase, nbliac)
        if (lfrot) then
            call nmrvai(sdstat, 'CONT_NBLIAF', phase, nbliaf)
        endif
        if (lctcd) then
            call nmrvai(sdstat, 'CTCD_ALGO_ITER', phase, ctiter)
            call nmtimr(sdtime, 'CTCD_ALGO', phase, tpscda)
        else if (lctcc) then
            call nmrvai(sdstat, 'CTCC_PREP', phase, ctccpr)
            call nmrvai(sdstat, 'CTCC_MATR', phase, ctcmat)
            call nmrvai(sdstat, 'CTCC_VECT', phase, ctcvec)
            call nmrvai(sdstat, 'CTCC_CONT', phase, ctiter)
            call nmrvai(sdstat, 'CTCC_FROT', phase, ctcfro)
            call nmtimr(sdtime, 'CTCC_PREP', phase, tpsccp)
            call nmtimr(sdtime, 'CTCC_MATR', phase, tpsccm)
            call nmtimr(sdtime, 'CTCC_VECT', phase, tpsccv)
            call nmtimr(sdtime, 'CTCC_CONT', phase, tpsccc)
            call nmtimr(sdtime, 'CTCC_FROT', phase, tpsccf)
        endif
    endif
!
! --- POST-TRAITEMENT (FLAMBEMENT OU STABILITE)
!
    if (lpost) then
        call nmtimr(sdtime, 'POST_TRAITEMENT', phase, tpspst)
    endif
!
! --- TEMPS RESTANT (NON MESURE)
!
    tpsrst = tps - tpsint - tpsasm - tpsfac - tps2mb - tpssol - tpscog - tpscda - tpsccp - tpsccm&
             & - tpsccv - tpsccc - tpsccf
!
    if (tpsrst .le. 0.d0) tpsrst = 0.d0
!
! --- AFFICHAGE FIN DU PAS
!
    if ((phase.eq.'P') .and. lprint) then
        tpsmoy = tps/nbiter
        call impfot(tps, tpscvt)
        call u2mesk('I', 'MECANONLINE7_1', 1, tpscvt)
        call impfot(tpsmoy, tpscvt)
        call u2mesg('I', 'MECANONLINE7_2', 1, tpscvt, 1,&
                    nbiter, 0, 0.d0)
        call impfot(tpsint, tpscvt)
        call u2mesg('I', 'MECANONLINE7_4', 1, tpscvt, 1,&
                    nbrint, 0, 0.d0)
        call impfot(tpsasm, tpscvt)
        call u2mesk('I', 'MECANONLINE7_13', 1, tpscvt)
        call impfot(tpsfac, tpscvt)
        call u2mesg('I', 'MECANONLINE7_3', 1, tpscvt, 1,&
                    nbrfac, 0, 0.d0)
        call impfot(tps2mb, tpscvt)
        call u2mesk('I', 'MECANONLINE7_12', 1, tpscvt)
        call impfot(tpssol, tpscvt)
        call u2mesg('I', 'MECANONLINE7_5', 1, tpscvt, 1,&
                    nbrsol, 0, 0.d0)
!
! ----- CONTACT - BOUCLE GEOMETRIQUE
!
        if ((lctcg.and.(.not.lallv)) .or. lnewtg) then
            call impfot(tpscog, tpscvt)
            call u2mesg('I', 'MECANONLINE7_11', 1, tpscvt, 1,&
                        ctgeom, 0, 0.d0)
        endif
!
! ----- CONTACT DISCRET
!
        if (lctcd .and. (.not.lallv)) then
            call impfot(tpscda, tpscvt)
            call u2mesg('I', 'MECANONLINE7_10', 1, tpscvt, 1,&
                        ctiter, 0, 0.d0)
        endif
!
! ----- CONTACT CONTINU
!
        if (lctcc .and. (.not.lallv)) then
            call impfot(tpsccp, tpscvt)
            call u2mesg('I', 'MECANONLINE7_24', 1, tpscvt, 1,&
                        ctccpr, 0, 0.d0)
            call impfot(tpsccm, tpscvt)
            call u2mesg('I', 'MECANONLINE7_20', 1, tpscvt, 1,&
                        ctcmat, 0, 0.d0)
            call impfot(tpsccv, tpscvt)
            call u2mesg('I', 'MECANONLINE7_25', 1, tpscvt, 1,&
                        ctcvec, 0, 0.d0)
            if (lboucc) then
                call impfot(tpsccc, tpscvt)
                call u2mesg('I', 'MECANONLINE7_23', 1, tpscvt, 1,&
                            ctiter, 0, 0.d0)
            endif
            if (lboucf) then
                call impfot(tpsccf, tpscvt)
                call u2mesg('I', 'MECANONLINE7_22', 1, tpscvt, 1,&
                            ctcfro, 0, 0.d0)
            endif
        endif
!
! ----- TEMPS POST-TRAITEMENT
!
        if (lpost) then
            call impfot(tpspst, tpscvt)
            call u2mesk('I', 'MECANONLINE7_7', 1, tpscvt)
        endif
!
! ----- TEMPS RESTANT
!
        call impfot(tpsrst, tpscvt)
        call u2mesk('I', 'MECANONLINE7_6', 1, tpscvt)
!
! ----- STATISTIQUES RECHERCHE LINEAIRE + FETI
!
        if (nbrrel .ne. 0) then
            call u2mesi('I', 'MECANONLINE7_8', 1, nbrrel)
        endif
        if (nbrfet .ne. 0) then
            call u2mesi('I', 'MECANONLINE7_9', 1, nbrfet)
        endif
!
! ----- STATISTIQUES SUR LE CONTACT
!
        if (lcont .and. (.not.lallv)) then
            call u2mess('I', 'MECANONLINE7_30')
            call u2mesi('I', 'MECANONLINE7_31', 1, nbliac)
            if (lfrot) then
                call u2mesi('I', 'MECANONLINE7_32', 1, nbliaf)
            endif
        endif
!
! --- AFFICHAGE DE LA CONSOMMATION MEMOIRE
!
        call impmem()
    endif
!
! --- AFFICHAGE FIN DU TRANSITOIRE
!
    if (phase .eq. 'T') then
        vali(1) = nbinst
        vali(2) = nbiter
        vali(3) = nbrint
        vali(4) = nbrfac
        vali(5) = nbrsol
        call u2mesi('I', 'MECANONLINE8_1', 5, vali)
!
! ----- STATISTIQUES RECHERCHE LINEAIRE + FETI
!
        if (nbrrel .ne. 0) then
            call u2mesi('I', 'MECANONLINE8_2', 1, nbrrel)
        endif
        if (nbrfet .ne. 0) then
            call u2mesi('I', 'MECANONLINE8_3', 1, nbrfet)
        endif
!
! ----- STATISTIQUES SUR LE CONTACT
!
        if (lcont .and. (.not.lallv)) then
            if (lctcd .or. lboucc .or. lboucf .or. lctcg .or. lnewtg) then
                call u2mess('I', 'MECANONLINE8_30')
            endif
        endif
!
! ----- CONTACT - BOUCLE GEOMETRIQUE
!
        if ((lctcg.and.(.not.lallv)) .or. lnewtg) then
            call u2mesi('I', 'MECANONLINE8_11', 1, ctgeom)
        endif
!
! ----- CONTACT DISCRET
!
        if (lctcd .and. (.not.lallv)) then
            call u2mesi('I', 'MECANONLINE8_10', 1, ctiter)
        endif
!
! ----- CONTACT CONTINU
!
        if (lctcc .and. (.not.lallv)) then
            if (lboucf) then
                call u2mesi('I', 'MECANONLINE8_22', 1, ctcfro)
            endif
            if (lboucc) then
                call u2mesi('I', 'MECANONLINE8_23', 1, ctiter)
            endif
        endif
!
! ----- TEMPS PASSE
!
        call impfot(tps, tpscvt)
        call u2mesk('I', 'MECANONLINE8_50', 1, tpscvt)
        if (tpslst .ne. 0.d0) then
            call impfot(tpslst, tpscvt)
            effica = 100.d0*(tps-tpslst)/tps
            call u2mesg('I', 'MECANONLINE8_70', 1, tpscvt, 0,&
                        ibid, 1, effica)
        endif
        call impfot(tpsint, tpscvt)
        call u2mesk('I', 'MECANONLINE8_54', 1, tpscvt)
        call impfot(tpsasm, tpscvt)
        call u2mesk('I', 'MECANONLINE8_51', 1, tpscvt)
        call impfot(tpsfac, tpscvt)
        call u2mesk('I', 'MECANONLINE8_53', 1, tpscvt)
        call impfot(tps2mb, tpscvt)
        call u2mesk('I', 'MECANONLINE8_52', 1, tpscvt)
        call impfot(tpssol, tpscvt)
        call u2mesk('I', 'MECANONLINE8_55', 1, tpscvt)
!
! ----- CONTACT - BOUCLE GEOMETRIQUE
!
        if ((lctcg.and.(.not.lallv)) .or. lnewtg) then
            call impfot(tpscog, tpscvt)
            call u2mesk('I', 'MECANONLINE8_61', 1, tpscvt)
        endif
!
! ----- CONTACT DISCRET
!
        if (lctcd .and. (.not.lallv)) then
            call impfot(tpscda, tpscvt)
            call u2mesk('I', 'MECANONLINE8_56', 1, tpscvt)
        endif
!
! ----- CONTACT CONTINU
!
        if (lctcc .and. (.not.lallv)) then
            call impfot(tpsccp, tpscvt)
            call u2mesk('I', 'MECANONLINE8_58', 1, tpscvt)
            call impfot(tpsccm, tpscvt)
            call u2mesk('I', 'MECANONLINE8_57', 1, tpscvt)
            call impfot(tpsccv, tpscvt)
            call u2mesk('I', 'MECANONLINE8_64', 1, tpscvt)
            if (lboucc) then
                call impfot(tpsccc, tpscvt)
                call u2mesk('I', 'MECANONLINE8_60', 1, tpscvt)
            endif
            if (lboucf) then
                call impfot(tpsccf, tpscvt)
                call u2mesk('I', 'MECANONLINE8_59', 1, tpscvt)
            endif
        endif
!
! ----- TEMPS POST-TRAITEMENT
!
        if (lpost) then
            call impfot(tpspst, tpscvt)
            call u2mesk('I', 'MECANONLINE8_62', 1, tpscvt)
        endif
!
! ----- TEMPS RESTANT
!
        call impfot(tpsrst, tpscvt)
        call u2mesk('I', 'MECANONLINE8_63', 1, tpscvt)
!
    endif
!
! --- RE-INIT STAT
!
    call nmrini(sdtime, sdstat, phase)
!
    call jedema()
end subroutine
