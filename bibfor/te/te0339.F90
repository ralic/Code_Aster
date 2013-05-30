subroutine te0339(option, nomte)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/dfdm3d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/tecach.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: option, nomte
!     -----------------------------------------------------------------
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
!     -----------------------------------------------------------------
!     FONCTION REALISEE :
!
!         CALCUL DU TAUX DE CROISSANCE DE CAVITES SELON UNE LOI DE
!         RICE ET TRACEY EN COMPORTEMENT NON-LINEAIRE.
!         ELEMENTS ISOPARAMETRIQUES 3D.
!
!         OPTION : 'RICE_TRACEY'
!
! ENTREE  --->  OPTION : NOM DE L'OPTION DE CALCUL
!         --->  NOMTE  : NOM DU TYPE D'ELEMENT
!
!
!-DEL CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8
!
    character(len=16) :: optcal(2)
!
    real(kind=8) :: sig(6), triax, volu, rsr0, numema, depseq
    real(kind=8) :: poids, dvpg, sigm, sigeq, lrsr0m, lrsr0p
    real(kind=8) :: cong(6), varigp, varigm, crois, vk, dfdbid(30)
    integer :: jgano, nno, npg, i, kp, iritra, ndim, iret
    integer :: issopt, ima, iadzi, iazk24, nbvari, ipopp, icompo
    integer :: ipoids, ivf, idfde, nnos
    integer :: igeom, icong, ivarpg, ivarmg, isdrmr, isdrpr, jtab(7)
!
!======================== CORPS DU PROGRAMME ===========================
!
!     1. RECUPERATION DES INFOS
!     -------------------------
!     1.1 NOMBRE DE NOEUDS ET DE POINTS DE GAUSS
!     ------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
!     1.2 NUMERO DE LA MAILLE
!     -----------------------
    call tecael(iadzi, iazk24)
    ima = zi(iadzi)
    numema = dble(ima)
!
!     1.3 CHAMPS IN
!     -------------
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCONTPR', 'L', icong)
    call jevech('PVARIMR', 'L', ivarmg)
    call jevech('PVARIPR', 'L', ivarpg)
    call jevech('PSDRMR', 'L', isdrmr)
    call jevech('PSOUSOP', 'L', issopt)
    call tecach('OON', 'PVARIPR', 'L', 7, jtab,&
                iret)
    nbvari = max(jtab(6),1)*jtab(7)
    call jevech('PCOMPOR', 'L', icompo)
!     READ (ZK16(ICOMPO+1),'(I16)') NBVARI
!
    if ((zk16(icompo).eq.'VMIS_ISOT_TRAC') .or. (zk16(icompo).eq.'VMIS_ISOT_LINE') .or.&
        (zk16(icompo).eq.'LEMAITRE') .or. (zk16(icompo).eq.'VMIS_ECMI_TRAC') .or.&
        (zk16(icompo).eq.'VMIS_ECMI_LINE') .or. (zk16(icompo).eq.'VISC_CIN1_CHAB') .or.&
        (zk16(icompo).eq.'VISC_CIN2_CHAB')) then
        ipopp = 1
    else
        call u2mesk('F', 'ELEMENTS3_74', 1, zk16(icompo))
    endif
!   /* ========================================================= */
!   /* PVARIMR = DEF PLAST EQ A L'INSTANT PRECEDENT              */
!   /* PVARIPR = DEF PLAST EQ A L'INSTANT COURRANT               */
!   /* PSDRMR  = LOG DU TAUX DE CROISSANCE A L'INSTANT PRECEDENT */
!   /* ========================================================= */
!
!     1.4 CHAMPS OUT
!     --------------
    call jevech('PRICTRA', 'E', iritra)
    call jevech('PSDRPR', 'E', isdrpr)
!   /* ========================================================= */
!   /* PRICTRA = CHAM_ELEM RICE-TRACEY (5 CMP)                   */
!   /* PSDRPR  = LOG DU TAUX DE CROISSANCE A L'INSTANT COURRANT  */
!   /* ========================================================= */
!
!     1.5 OPTIONS DE CALCUL
!     ---------------------
    optcal(1) = zk24(issopt) (1:16)
    optcal(2) = zk24(issopt) (17:19)
!
!     1.6 INITIALISATION
!     ------------------
    poids = 0.d0
    triax = 0.d0
    rsr0 = 0.d0
    volu = 0.d0
    vk = 0.d0
    dvpg = 0.d0
    depseq = 0.d0
    do 10,i = 1,6,1
    cong(i) = 0.d0
    10 end do
    varigm = 0.d0
    varigp = 0.d0
!
!
!     2. BOUCLE SUR POINTS DE GAUSS SUIVANT OPTIONS DE CALCUL
!     -------------------------------------------------------
!   /* ============================================================== */
!   /* CMP ACTIVES DU CHAM_ELEM RICE-TRACEY SUIVANT LES OPTIONS       */
!   /* -------------------------------------------------------------- */
!   /* 1. CALCUL DU TAUX MOYEN : PREPRATION POUR INTEGRATION DE RT    */
!   /*       TAUX DE TRIAXIALITE SUR LA MAILLE               (TRIAX ) */
!   /*       VARIATION DE DEF PLAST EQ                       (DEPSEQ) */
!   /*       VOLUME PRIS EN COMPTE                           (VOLU  ) */
!   /*       NUMERO DE LA MAILLE                             (NUMEMA) */
!   /*    IE. CE QU'IL FAUDRA MOYENNER AVANT D'INTEGRER RT            */
!   /*                                                                */
!   /* 2. CALCUL DU TAUX MAX : INTEGRATION DE RT SUR LA MAILLE        */
!   /*       VOLUME PRIS EN COMPTE                           (VOLU  ) */
!   /*       TAUX DE CROISSANCE SUR LA MAILLE INSTANT COURRANT (RSR0) */
!   /*       NUMERO DE LA MAILLE                             (NUMEMA) */
!   /*    DANS CE CAS, LE PARAM OUT PSDRPR JOUE VRAIMENT SON ROLE     */
!   /* ============================================================== */
!
!     2.1 CHAM_ELEM POUR LE CALCUL DU TAUX MOYEN AVEC CHAMPS IN MOYENNES
!     ------------------------------------------------------------------
    if ((optcal(1).eq.'SIGM_ELMOY') .and. (optcal(2).eq.'NON')) then
!        2.1.1 INTEGRATION PAR QUADRATURE DES CHAMPS IN
!        ----------------------------------------------
        do 30,kp = 1,npg,1
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdbid, dfdbid, dfdbid, poids)
        dvpg = poids
        vk = vk + dvpg
        do 20,i = 1,6,1
        cong(i) = cong(i) + dvpg*zr(icong+6*kp+i-7)
20      continue
        varigm = varigm + dvpg*zr(ivarmg+nbvari* (kp-1)+ipopp-1)
        varigp = varigp + dvpg*zr(ivarpg+nbvari* (kp-1)+ipopp-1)
30      continue
!
!        2.1.2 VALEUR MOYENNE DES CHAMPS IN SUR LA MAILLE
!        ------------------------------------------------
        do 40,i = 1,6,1
        sig(i) = cong(i)/vk
40      continue
        varigm = varigm/vk
        varigp = varigp/vk
!
!        2.1.3 INVARIANTS
!        ----------------
        sigm = (sig(1)+sig(2)+sig(3))/3.d0
        sigeq = sig(4)*sig(4) + sig(5)*sig(5) + sig(6)*sig(6)
        sigeq = sigeq + sigeq
        sigeq = sigeq + (&
                sig(1)-sigm)* (sig(1)-sigm) + (sig(2)-sigm)* (sig(2)-sigm) + (sig(3)-sigm)* (sig(&
                &3)-sigm&
                )
        sigeq = sqrt(1.5d0*sigeq)
!
!        2.1.4 CHAMPS OUT
!        ----------------
        triax = sigm/sigeq
        volu = vk
        depseq = varigp - varigm
        do 50,i = 1,npg,1
        zr(isdrpr+i-1) = zr(isdrmr+i-1)
50      continue
!
!     2.2 CHAM_ELEM POUR CALCUL DU TAUX MOYEN AVEC CHAMPS IN ORIGINAUX
!     ----------------------------------------------------------------
        else if ((optcal(1).eq.'SIGM_ELGA') .and. (optcal(2).eq.'NON'))&
    then
        do 70,kp = 1,npg,1
!           2.2.1 RECUPERATION DES CHAMPS IN
!           --------------------------------
        do 60,i = 1,6,1
        cong(i) = zr(icong+6*kp+i-7)
60      continue
        varigm = zr(ivarmg+nbvari* (kp-1)+ipopp-1)
        varigp = zr(ivarpg+nbvari* (kp-1)+ipopp-1)
!
!           2.2.2 CALCUL DE LA TRIAXIALITE LOCALE
!           -------------------------------------
        sigm = (cong(1)+cong(2)+cong(3))/3.d0
        sigeq = cong(4)*cong(4) + cong(5)*cong(5) + cong(6)*cong( 6)
        sigeq = sigeq + sigeq
        sigeq = sigeq + (&
                cong(1)-sigm)* (cong(1)-sigm) + (cong(2)- sigm)* (cong(2)-sigm) + (cong(3)-sigm)*&
                & (cong(3)-sigm&
                )
        sigeq = sqrt(1.5d0*sigeq)
!
!           2.2.3 INTEGRATION PAR QUADRATURE
!           --------------------------------
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdbid, dfdbid, dfdbid, poids)
        dvpg = poids
        vk = vk + dvpg
        triax = triax + dvpg* (sigm/sigeq)
        depseq = depseq + dvpg* (varigp-varigm)
70      continue
!
!        2.2.4 CHAMPS OUT
!        ----------------
        triax = triax/vk
        volu = vk
        depseq = depseq/vk
        do 80,i = 1,npg,1
        zr(isdrpr+i-1) = zr(isdrmr+i-1)
80      continue
!
!     2.3 CHAM_ELEM POUR LE CALCUL DU TAUX MAX AVEC CHAMPS IN MOYENNES
!     ----------------------------------------------------------------
        else if ((optcal(1).eq.'SIGM_ELMOY') .and. (optcal(2).eq.'OUI'))&
    then
!        2.3.1 INTEGRATION PAR QUADRATURE DES CHAMPS IN
!        ----------------------------------------------
        do 100,kp = 1,npg,1
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdbid, dfdbid, dfdbid, poids)
        dvpg = poids
        vk = vk + dvpg
        do 90,i = 1,6,1
        cong(i) = cong(i) + dvpg*zr(icong+6*kp+i-7)
90      continue
        varigm = varigm + dvpg*zr(ivarmg+nbvari* (kp-1)+ipopp-1)
        varigp = varigp + dvpg*zr(ivarpg+nbvari* (kp-1)+ipopp-1)
100      continue
!
!        2.3.2 VALEUR MOYENNE DES CHAMPS IN SUR LA MAILLE
!        ------------------------------------------------
        do 110,i = 1,6,1
        sig(i) = cong(i)/vk
110      continue
        varigm = varigm/vk
        varigp = varigp/vk
!
!        2.3.3 INVARIANTS
!        ----------------
        sigm = (sig(1)+sig(2)+sig(3))/3.d0
        sigeq = sig(4)*sig(4) + sig(5)*sig(5) + sig(6)*sig(6)
        sigeq = sigeq + sigeq
        sigeq = sigeq + (&
                sig(1)-sigm)* (sig(1)-sigm) + (sig(2)-sigm)* (sig(2)-sigm) + (sig(3)-sigm)* (sig(&
                &3)-sigm&
                )
        sigeq = sqrt(1.5d0*sigeq)
!
!        2.3.4 INTEGRATION DE LA LOI RT
!        ------------------------------
        triax = sigm/sigeq
        volu = vk
        depseq = varigp - varigm
        lrsr0m = zr(isdrmr)
        lrsr0p = lrsr0m + 0.283d0*sign(1.0d0,triax)* exp(1.5d0*abs( triax))*depseq
!
!        2.3.5 CHAMPS OUT
!        ----------------
        rsr0 = exp(lrsr0p)
        do 120,i = 1,npg,1
        zr(isdrpr+i-1) = lrsr0p
120      continue
!
!     2.4 CHAM_ELEM POUR LE CALCUL DU TAUX MAX AVEC CHAMPS IN ORIGINAUX
!     -----------------------------------------------------------------
        else if ((optcal(1).eq.'SIGM_ELGA') .and. (optcal(2).eq.'OUI'))&
    then
        do 140,kp = 1,npg,1
!           2.4.1 RECUPERATION DES CHAMPS IN
!           --------------------------------
        do 130,i = 1,6,1
        cong(i) = zr(icong+6*kp+i-7)
130      continue
        varigm = zr(ivarmg+nbvari* (kp-1)+ipopp-1)
        varigp = zr(ivarpg+nbvari* (kp-1)+ipopp-1)
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdbid, dfdbid, dfdbid, poids)
        dvpg = poids
        volu = volu+dvpg
!
!           2.4.2 CALCUL DE LA TRIAXIALITE LOCALE
!           -------------------------------------
        sigm = (cong(1)+cong(2)+cong(3))/3.d0
        sigeq = cong(4)*cong(4) + cong(5)*cong(5) + cong(6)*cong( 6)
        sigeq = sigeq + sigeq
        sigeq = sigeq + (&
                cong(1)-sigm)* (cong(1)-sigm) + (cong(2)- sigm)* (cong(2)-sigm) + (cong(3)-sigm)*&
                & (cong(3)-sigm&
                )
        sigeq = sqrt(1.5d0*sigeq)
        triax = sigm/sigeq
!
!           2.4.3 INTEGRATION DE LA LOI RT AU PG COURRANT
!           ---------------------------------------------
        depseq = varigp - varigm
        lrsr0m = zr(isdrmr+kp-1)
        lrsr0p = lrsr0m + 0.283d0*sign(1.0d0,triax)* exp(1.5d0* abs(triax))*depseq
        crois = exp(lrsr0p)
!
!           2.4.4 CHAMPS OUT
!           ----------------
        zr(isdrpr+kp-1) = lrsr0p
        if (crois .gt. rsr0) then
            rsr0 = crois
        endif
140      continue
!       ON SORT LE VOLUME ASSOCIE A LA "SOUS-MAILLE" (CF DOC R)
!       PLUTOT QU ECRIRE UN POIDS RELATIF DE PT DE GAUSS,
!       VARIABLE SUIVANT LE PG QUI "ACCROCHE" LE MAX,
!       ON SORT LE SOUS-VOLUME MOYEN PAR PG :
        volu = volu/dvpg
!
!     2.5 TRAITEMENT DES OPTIONS INVALIDES
!     ------------------------------------
    else
!       OPTION DE CALCUL NON VALIDE
        call assert(.false.)
    endif
!
!
!     3. ECRITURE DES CMP DU CHAM_ELEM OUT DE TYPE RICE-TRACEY
!     --------------------------------------------------------
    zr(iritra) = triax
    zr(iritra+1) = rsr0
    zr(iritra+2) = volu
    zr(iritra+3) = numema
    zr(iritra+4) = depseq
!
end subroutine
