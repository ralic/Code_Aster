subroutine acgrdo(jvectn, jvectu, jvectv, nbordr, ordini,&
                  kwork, sompgw, jrwork, tspaq, ipg,&
                  jvecpg, jdtaum, jresun, nommet, nommat,&
                  nomcri, vala, coefpa, nomfor, grdvie,&
                  forvie, valpar, vresu)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getvr8.h'
    include 'asterc/r8maem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/acmata.h'
    include 'asterfort/anacri.h'
    include 'asterfort/fgequi.h'
    include 'asterfort/fmrayo.h'
    include 'asterfort/fointe.h'
    include 'asterfort/fonbpa.h'
    include 'asterfort/jacobi.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/lciv2e.h'
    include 'asterfort/limend.h'
    include 'asterfort/rccome.h'
    include 'asterfort/rcvale.h'
    include 'asterfort/renrfa.h'
    include 'asterfort/teneps.h'
    include 'asterfort/u2mess.h'
    integer :: jvectn, jvectu, jvectv, nbordr, kwork
    integer :: sompgw, jrwork, tspaq, ipg, jvecpg, jdtaum, jresun
    character(len=16) :: nommet, nomcri, nomfor, forvie
    character(len=8) :: nommat, grdvie
    real(kind=8) :: vresu(24), valpar(22), vala, coefpa
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_20 CRP_21 CRS_512 CRS_1404
! ---------------------------------------------------------------------
! BUT: POUR LA FATIGUE A AMPLITUDE CONSTANTE
!      CALCULER DES GRANDEURS SERVANT A EVALUER LES CRITERES D'AMORCAGE
!      ET CALCULER LA GRANDEUR EQUIVALENT
!
! REMARQUE: CETTE SUBROUTINE EST APPLICABLE POUR UN NOEUD OU IPG EGALE
!           A 1 ET SOMPGW = SOMNOW,JVECPG = JVECNO
! ---------------------------------------------------------------------
! ARGUMENTS :
!     JVECTN  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS NORMAUX.
!     JVECTU  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS u DU PLAN DE CISAILLEMENT.
!     JVECTV  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS v DU PLAN DE CISAILLEMENT.
!     NBORDR  : IN  : NOMBRE DE NUMEROS D'ORDRE.
!     ORDINI     IN    I  : ORDRE INITIAL POUR LE CHARGEMENT CYCLIQUE
!     KWORK   : IN  : KWORK = 0 ON TRAITE LA 1ERE MAILLE DU PAQUET DE
!                               MAILLES ;
!                     KWORK = 1 ON TRAITE LA IEME (I>1) MAILLE DU PAQUET
!                               MAILLES.
!     SOMPGW  : IN  : SOMME DES POINTS DE GAUSS DES N MAILLES PRECEDANT
!                     LA MAILLE COURANTE.
!     JRWORK  : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
!                     L'HISTORIQUE DES TENSEURS DES CONTRAINTES
!                     ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
!                     DU <<PAQUET>> DE MAILLES.
!     TSPAQ   : IN  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
!                     COURANT.
!     IPG     : IN  : IEME POINT DE GAUSS.
!     JVECPG  : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
!                     LES COMPOSANTES u ET v DU VECTEUR TAU
!                     (CISAILLEMENT), POUR TOUS LES NUMEROS
!                     D'ORDRE DE CHAQUE VECTEUR NORMAL.
!    JDTAU      IN    ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
!                     LES VALEURS DE DELTA_TAU_MAX POUR CHAQUE VECTEUR.
!    JVECN      IN    ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
!                     LA VALEUR DU POINTEUR PERMETTANT D'ACCEDER AU
!                     VECTEUR NORMAL ASSOCIE A DELTA_TAU_MAX.
!    NOMMET     IN    NOM DE METHOD D'APPROCHEMENT DE CERCLE ("CERCLE
!                     EXACT" ET "CERCLE APPROCHE")
!    VALA       IN    VALEUR DU PARAMETRE a ASSOCIE AU CRITERE.
!    COEFPA     IN    COEFFICIENT DE PASSAGE CISAILLEMENT - UNIAXIAL.
!    VRESU      OUT   TABLEAU DES RESULTATS (GRANDEURS ET DOMMAGE).
!                     POUR L'INSTANT, LA DIMENSION DE VRESU EST 24
! ---------------------------------------------------------------------
!23456
!
    real(kind=8) :: resupc(24), grdeq(2), dtaum(2), nxm(2), nym(2), coepre
    real(kind=8) :: nzm(2), nrupt(2), dom(2), sigeq(2)
    real(kind=8) :: normax(2), normoy(2), epnmax(2), epnmoy(2), valpu(22)
    real(kind=8) :: phydro, phydrm, sigm(nbordr*6)
    real(kind=8) :: sig(6), eps(6), epse(6), epsp(6), vepsp(6)
    real(kind=8) :: epsl(6), epsel(6), epspl(6), eqepsp, jacaux(3)
    real(kind=8) :: vsig(6), sigl(6), eqsig, vsige, equi(17)
    real(kind=8) :: phymin, rbid(6), vepspe
    real(kind=8) :: nm1x, nm1y, nm1z, br(6), vecpro(3, 3), valpro(3)
    real(kind=8) :: eprmax, eprmin, signm1, tol, toldyn, ar(6)
    real(kind=8) :: fxm, fym, fzm, sinm1m, somdef
    real(kind=8) :: devsig(6), dvepse(6), dendis, dsigl(6), raysph
    real(kind=8) :: depsl(6), somden, dendie, etrema, etremi
    real(kind=8) :: sigmax, exm, eym, ezm, epsnm1, epnm1m, sigmin
    real(kind=8) :: strema, stremi, veps(6), eqeps, vepst, epspac
    integer :: nperm, itype, iordre, nvp, nitjac, ordini, ordfin
    integer :: i, j, k, l, ibid, jprof, nparma, np, icodre, adr, iret, ipar
    integer :: decal, paract(30), adrl, iarg, nbf, nbtot
    character(len=24) :: chnom, cbid
    character(len=16) :: phenom, typcha
    character(len=8) :: nompf(22), nompar(22), nomgrd
    logical :: endur, plcicr, lbid
!-----------------------------------------------------------------------
!       DATA  LSIG/ 'SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ', 'SIYZ' /
! C
!       DATA  LEPS/ 'EPXX', 'EPYY', 'EPZZ', 'EPXY', 'EPXZ', 'EPYZ' /
! C
!       DATA  LGRD/  'DTAUM1', 'VNM1X', 'VNM1Y', 'VNM1Z', 'SINMAX1',
!      &             'SINMOY', 'EPNMAX', 'EPNMOY', 'SIGEQ1', 'NBRUP1',
!      &             'ENDO1', 'DTAUM2', 'VNM2X', 'VNM2Y', 'VNM2Z',
!      &        'SINMAX2', 'SINMOY2', 'EPNMAX2', 'EPNMOY2', 'SIGEQ2',
!      &             'NBRUP2', 'ENDO2' ,'VMIS', 'TRESCA' /
!
!     ---------------------------------------------------------------
    data  nompar/   'DTAUMA', 'PHYDRM', 'NORMAX', 'NORMOY',&
     &                  'EPNMAX', 'EPNMOY', 'DEPSPE', 'EPSPR1',&
     &                  'SIGNM1', 'DENDIS', 'DENDIE', 'APHYDR',&
     &                  'MPHYDR', 'DSIGEQ', 'SIGPR1', 'EPSNM1',&
     &                  'INVA2S', 'DSITRE', 'DEPTRE', 'EPSPAC',&
     &                  'RAYSPH', 'AMPCIS'   /
!     ---------------------------------------------------------------
!
! -------------------------------------------------------------------
!
! RECUPERER LA LISTE DE GRANDEURS ACTIVES
!
    typcha = 'PERIODIQUE'
!
!  INITIALISER
!
    call anacri(nomcri, nomfor, typcha, 'NON', paract,&
                lbid, lbid, lbid, lbid, lbid)
!
! ------------------------------------------------------------------
!---  CALCULER DES GRANDEURS ACTIVES
!-------------------------------------------------------------------
! INITIALISATION
    plcicr = .false.
    phydrm = 0.d0
    phymin = 0.d0
    phydro = 0.d0
    vepspe = 0.d0
    vsige = 0.d0
    eprmax = r8prem()
    eprmin = r8maem()
    nm1x = 0.d0
    nm1y = 0.d0
    nm1z = 0.d0
    signm1 = 0.d0
    sinm1m = 0.d0
    dendis = 0.d0
    dendie = 0.d0
    etrema = 0.d0
    etremi = 0.d0
    strema = 0.d0
    stremi = 0.d0
    epnm1m = 0.d0
    sigmax = r8prem()
    sigmin = r8maem()
    vepst = 0.d0
    epspac = 0.d0
    raysph = 0.d0
    dom(1) = 0.d0
    dom(2) = 0.d0
    nrupt(1) = 1.d7
    nrupt(2) = 1.d7
!
    do 20 i = 1, 24
        resupc(i) = 0.0d0
20  end do
!
    do 35 i = 1, 6
        sig(i) = 0.0d0
        eps(i) = 0.0d0
        epse(i)= 0.0d0
        epsp(i)= 0.0d0
        vepsp(i)= 0.0d0
        vsig(i)= 0.0d0
        epsl(i)= 0.0d0
        epsel(i)= 0.0d0
        epspl(i)= 0.0d0
        sigl(i) = 0.0d0
35  end do
!
! FIN D'INITILISATION
!
! ---------------------------------------------------------------
! RECUPER LES CONTRAINTES ET DEFORMATION
!       CALL RCVALE(NOMMAT,'ELAS',0,'        ',R8B,1,'E       ',
!      &            VALE,ICODRE,0)
!       IF (ICODRE .EQ. 1) THEN
!          CALL U2MESS('F','PREPOST_11')
!       ENDIF
!       CALL RCVALE(NOMMAT,'ELAS',0,'        ',R8B,1,'NU      ',
!      &            VALNU,ICODRE,0)
!       IF (ICODRE .EQ. 1) THEN
!          CALL U2MESS('F','PREPOST_12')
!       ENDIF
!       C1 = (1+VALNU)/VALE
!       C2 = VALNU/VALE
!
! ---------------------------------------------------------------
!
! POUR LE POST-FATIGUE, ON CONSIDERE QUE L'HISTOIRE EST
! POUR UN CYCLE ENTIER - COMPLET
!
! C      IF (POST) THEN
!       ORDINI = 1
!       ORDFIN = NBORDR
! C      ENDIF
!       IF ((PARACT(11) .EQ. 1) .OR. (PARACT(10) .EQ. 1) ) THEN
! C ANALYSER LES CHARGEMENTS APLIQUES
!
!          CALL ACANCY(NBORDR, KWORK, SOMPGW, JRWORK, TSPAQ, IPG, C1,
!      &                C2, ORINIE, ORDFIE, NBCYAD,CYFERM, EPSPA)
!          ORDINI = ORINIE
!          ORDFIN = ORDFIE
!
!       ENDIF
!
!
! ---------------------------------------------------------------
! ANALYSER LES CHARGEMENTS APLIQUES
!
!        CALL ACANCY(NBORDR, KWORK, SOMPGW, JRWORK, TSPAQ, IPG, C1,
!      &                C2, ORDINI, ORDFIN, NBCYAD,CYFERM, EPSPA)
! ---------------------------------------------------------------
! CALCULER LES GRANDEURS
!
! POUR LE POST-FATIGUE, ON CONSIDERE QUE L'HISTOIRE EST
! POUR UN CYCLE ENTIER - COMPLET
!
!       IF (POST) THEN
!          ORDINI = 1
!       ENDIF
!
! ---------------------------------------------------------------
! CALCULER LES GRANDEURS
    ordfin = nbordr
!
    do 10 j = ordini, ordfin
        decal = 18
!         ADR = (J-1)*TSPAQ+KWORK*SOMPGW*6+(IPG-1)*6
!
        adr = (j-1)*tspaq+kwork*sompgw*decal+(ipg-1)*decal
!
! RECUPERER LES TENSEURS A CE NUMERO D'ORDRE
! ORDRE DU TENSEUR SIXX, SIYY, SIZZ, SIXY, SIXZ, SIYZ,
! SIMILAIRE POUR
!   EPS  : DEFORMATION TOTALE
!   EPSE : DEFORMATION ELASTIQUE
!   EPSP : DEFORMATION PLASTIQUE
!
!
        call teneps(jrwork, adr, sig, eps, epse,&
                    epsp)
!
! ---------------------------------------------------------------
! ON CALCULE PHYDRM QU'UNE FOIS, LA PRESSION HYDROSTATIQUE
! EST CONSTANTE PAR RAPPORT AU vect_n.
!
! -- CALCULER LES GRANDEURS PRESSION HYDROSTATIQUE
!
!    CALCULER PRESSION HYDROSTATIQUE MAXIMALE = Max_t(1/3 Tr[SIG])
        if ((paract(2) .eq. 1) .or. (paract(12) .eq. 1) .or. (paract( 13) .eq. 1)) then
!
            phydro = (sig(1) + sig(2) + sig(3))/3.0d0
!
            if (phydro .gt. phydrm) then
                phydrm = phydro
            endif
!
            if (phydro .lt. phymin) then
                phymin = phydro
            endif
!
        endif
!
! ---------------------------------------------------------------
! -- CALCULER LA DEMI-AMPLITUDE DE LA DEFORMATION PLASIQUE EQVA
! POUR LE CRIETRE MANSON_COFF
!
        if (paract(7) .eq. 1) then
!
            do 11 l = j, ordfin
                adrl = (l-1)*tspaq+kwork*sompgw*decal+(ipg-1)*decal
!
                call teneps(jrwork, adrl, rbid, rbid, rbid,&
                            epspl)
!
                do 12 k = 1, 6
                    vepsp(k)= epsp(k) - epspl(k)
12              continue
                call fgequi(vepsp, 'EPSI', 3, equi)
                eqepsp = equi(1)
!               EQEPSP = LCIV2E(VEPSP)
!
                if (vepspe .lt. eqepsp) then
                    vepspe = eqepsp
                endif
11          continue
!
        endif
!
! SIG PRIN MAX-----------------------------------------------
        if ((paract(8) .eq. 1) .or. (paract(9) .eq. 1) .or. (paract( 19) .eq. 1)) then
            nvp = 3
            nperm = 12
            tol = 1.d-10
            toldyn = 1.d-2
            itype = 0
            iordre = 1
            ar(1) = eps(1)
            ar(2) = eps(4)
            ar(3) = eps(5)
            ar(4) = eps(2)
            ar(5) = eps(6)
            ar(6) = eps(3)
            br(1) = 1.d0
            br(2) = 0.d0
            br(3) = 0.d0
            br(4) = 1.d0
            br(5) = 0.d0
            br(6) = 1.d0
!
            call jacobi(nvp, nperm, tol, toldyn, ar,&
                        br, vecpro, valpro, jacaux, nitjac,&
                        itype, iordre)
!
            if (eprmax .lt. valpro(1)) then
                eprmax = valpro(1)
                nm1x = vecpro (1,1)
                nm1y = vecpro (2,1)
                nm1z = vecpro (3,1)
! CALCvect_F = [SIG].vect_n
!
                fxm = sig(1)*nm1x + sig(4)*nm1y + sig(5)*nm1z
                fym = sig(4)*nm1x + sig(2)*nm1y + sig(6)*nm1z
                fzm = sig(5)*nm1x + sig(6)*nm1y + sig(3)*nm1z
!
! CALCNORM = vect_F.vect_n
!
                signm1 = fxm*nm1x + fym*nm1y + fzm*nm1z
!
                if (abs(signm1) .gt. sinm1m) then
                    sinm1m = abs(signm1)
                endif
!
!
            endif
!
!
            if (eprmin .gt. valpro(1)) then
                eprmin = valpro(1)
            endif
!
            if (etrema .lt. (valpro(1)-valpro(3))) then
                etrema = (valpro(1)-valpro(3))
            endif
!
            if (etremi .gt. (valpro(1)-valpro(3))) then
                etremi = valpro(1)-valpro(3)
            endif
        endif
!
! ---------------------------------------------------------------
! CALCULER DENSITE D'ENERGIE DISTORSION ELASTIQUE
!
        if (paract(11) .eq. 1) then
!
            call lcdevi(sig, devsig)
            call lcdevi(epse, dvepse)
!
!
            if (j .lt. ordfin) then
                adrl = (j+1-1)*tspaq+kwork*sompgw*decal+(ipg-1)*decal
!
                call teneps(jrwork, adrl, sigl, epsl, epsel,&
                            epspl)
!
                call lcdevi(sigl, dsigl)
                call lcdevi(epsel, depsl)
!
                somden = 0.d0
                do 32 i = 1, 6
!
                    somden= somden + 0.5d0*(devsig(i)+dsigl(i))&
                    *(depsl(i)- dvepse(i))
32              continue
!
                if (somden .gt. 0) then
                    dendie = dendie + somden
                endif
!
            endif
!
        endif
!
! ---------------------------------------------------------------
! CALCULER DENSITE D'ENERGIE DISSIPISE PLASTIQUE
!
        if (paract(10) .eq. 1) then
!
            if (j .lt. ordfin) then
                adrl = (j+1-1)*tspaq+kwork*sompgw*decal+(ipg-1)*decal
!
                call teneps(jrwork, adrl, sigl, epsl, epsel,&
                            epspl)
!
                somden = 0.d0
                do 33 i = 1, 6
                    somden= somden + 0.5d0*(sig(i)+sigl(i)) *(epspl(i)&
                    - epsp(i))
!
33              continue
!
                dendis = dendis + somden
!
            endif
!
        endif
!
! ---------------------------------------------------------------
! -- CALCULER LA DEMI-AMPLITUDE DE LA CONTRAINTE EQVALENTE
! POUR LE CRIETRE MANSON_COFF
!
        if ((paract(14) .eq. 1) .or. (paract(22) .eq. 1)) then
!
            do 21 l = j, ordfin
                adrl = (l-1)*tspaq+kwork*sompgw*decal+(ipg-1)*decal
!
                call teneps(jrwork, adrl, sigl, rbid, rbid,&
                            rbid)
!
                do 22 i = 1, 6
                    vsig(i)= sig(i) - sigl(i)
22              continue
                call fgequi(vsig, 'SIGM', 3, equi)
                eqsig = equi(1)
!
                if (vsige .lt. eqsig) then
                    vsige = eqsig
                endif
21          continue
!
        endif
!
! ---------------------------------------------------------------
! CALCULER CONTRAINTES PRINCIPALES MAX ET LA DEF TRACTION DU PLAN
        if ((paract(15) .eq. 1) .or. (paract(16) .eq. 1) .or. (paract( 18) .eq. 1)) then
            nvp = 3
            nperm = 12
            tol = 1.d-10
            toldyn = 1.d-2
            itype = 0
            iordre = 1
            ar(1) = sig(1)
            ar(2) = sig(4)
            ar(3) = sig(5)
            ar(4) = sig(2)
            ar(5) = sig(6)
            ar(6) = sig(3)
            br(1) = 1.d0
            br(2) = 0.d0
            br(3) = 0.d0
            br(4) = 1.d0
            br(5) = 0.d0
            br(6) = 1.d0
!
            call jacobi(nvp, nperm, tol, toldyn, ar,&
                        br, vecpro, valpro, jacaux, nitjac,&
                        itype, iordre)
!
            if (sigmax .lt. valpro(1)) then
                sigmax = valpro(1)
                nm1x = vecpro (1,1)
                nm1y = vecpro (2,1)
                nm1z = vecpro (3,1)
! CALCvect_F = [SIG].vect_n
!
                exm = eps(1)*nm1x + eps(4)*nm1y + eps(5)*nm1z
                eym = eps(4)*nm1x + eps(2)*nm1y + eps(6)*nm1z
                ezm = eps(5)*nm1x + eps(6)*nm1y + eps(3)*nm1z
!
! CALCNORM = vect_F.vect_n
!
                epsnm1 = exm*nm1x + eym*nm1y + ezm*nm1z
!
                if (abs(epsnm1) .gt. epnm1m) then
                    epnm1m = abs(epsnm1)
                endif
!
            endif
!
            if (sigmin .gt. valpro(1)) then
                sigmin = valpro(1)
            endif
!
            if (strema .lt. (valpro(1)-valpro(3))) then
                strema = (valpro(1)-valpro(3))
            endif
!
            if (stremi .gt. (valpro(1)-valpro(3))) then
                stremi = valpro(1)-valpro(3)
            endif
        endif
!
! ---------------------------------------------------------------
! CALCULER CONTRAINTES PRINCIPALES MAX ET LA TRACTION DE CE PLAN
        if (paract(17) .eq. 1) then
!
            do 41 l = j, ordfin
                adrl = (l-1)*tspaq+kwork*sompgw*decal+(ipg-1)*decal
!
                call teneps(jrwork, adrl, rbid, epsl, epsel,&
                            epspl)
!
                do 42 i = 1, 6
                    veps(i)= eps(i) - epsl(i)
42              continue
                eqeps = lciv2e(veps)
!
                if (vepst .lt. eqeps) then
                    vepst = eqeps
                endif
41          continue
!
        endif
!
! ---------------------------------------------------------------
! CALCULER DEFORMATION PLASTIQUE ACCUMULEE
        if (paract(20) .eq. 1) then
!
            if (j .lt. ordfin) then
                adrl = (j+1-1)*tspaq+kwork*sompgw*decal+(ipg-1)*decal
!
                call teneps(jrwork, adrl, sigl, epsl, epsel,&
                            epspl)
!
                somdef = 0.d0
                do 43 k = 1, 6
                    somdef = somdef + (epspl(k)-epsp(k))* (epspl(k)- epsp(k))
43              continue
                epspac = epspac + somdef**0.5d0
            endif
!
        endif
!
10  end do
!
!
! ---------------------------------------------------------------
! CALCULER LE RAYON DE SPHERE CIRCONCRITE
    if (paract(21) .eq. 1) then
!
        do 16 j = 1, nbordr*6
            sigm(j) = 0.d0
16      continue
!
        do 15 j = ordini, ordfin
            decal = 18
!
            adr = (j-1)*tspaq+kwork*sompgw*decal+(ipg-1)*decal
!
            call teneps(jrwork, adr, sig, eps, epse,&
                        epsp)
!
            do 17 k = 1, 6
                sigm((j-1)*6+ k) = sig(k)
17          continue
15      continue
!
        nbf = 6
        nbtot = ordfin - ordini +1
        call fmrayo(nbf, nbtot, sigm, raysph)
!
    endif
!
! ---------------------------------------------------------------
! POUR LES GRANDEURS DES CRITERERS "CISSAILEMENT PLAN CRITIQUE",
! ACMATA  CALCULE LES 8 PREMIER GRANDEURS ET CEUX DE 13-20
!
    if ((paract(1) .eq. 1) .or. (paract(3) .eq. 1) .or. (paract(4) .eq. 1) .or.&
        (paract(5) .eq. 1) .or. (paract(6) .eq. 1)) then
!
        plcicr = .true.
    endif
!
    do 109 k = 1, 2
        dtaum(k) = 0.d0
        nxm(k) = 0.d0
        nym(k) = 0.d0
        nzm(k) = 0.d0
        normax(k)= 0.d0
        normoy(k)= 0.d0
        epnmax(k)= 0.d0
        epnmoy(k)= 0.d0
109  end do
!
    if (plcicr) then
!
        call acmata(jvectn, jvectu, jvectv, nbordr, kwork,&
                    sompgw, jrwork, tspaq, ipg, jvecpg,&
                    jdtaum, jresun, nommet, resupc)
!
        do 110 k = 1, 2
            dtaum(k) = resupc(1+(k-1)*11)
            nxm(k) = resupc(2+(k-1)*11)
            nym(k) = resupc(3+(k-1)*11)
            nzm(k) = resupc(4+(k-1)*11)
            normax(k)= resupc(5+(k-1)*11)
            normoy(k)= resupc(6+(k-1)*11)
            epnmax(k)= resupc(7+(k-1)*11)
            epnmoy(k)= resupc(8+(k-1)*11)
!
110      continue
!
    endif
!
! -----------------------------------------------------------------
! --  EVALUER LES CRITERES EXISTANTS
! -----------------------------------------------------------------
!
!
    do 120 k = 1, 2
!
        if (nomcri(1:7) .ne. 'FORMULE') then
!
! RECUPERATION DU COEFFICIENT DE PRE-ECROUISSAGE DONNE
! PAR L'UTILISATE
!
            call getvr8(' ', 'COEF_PREECROU', 1, iarg, 1,&
                        coepre, iret)
!
!        1/ C DE MATAKE
            if (nomcri(1:14) .eq. 'MATAKE_MODI_AC') then
                if (normax(k) .gt. 0.0d0) then
                    sigeq(k) = coepre*dtaum(k) + (vala*normax(k))
                    sigeq(k) = sigeq(k)*coefpa
                else
                    sigeq(k) = coepre*dtaum(k)
                    sigeq(k) = sigeq(k)*coefpa
                endif
                grdeq(k) = sigeq(k)
!
            endif
!
!        2/ C DE DANG VAN
            if (nomcri(1:16) .eq. 'DANG_VAN_MODI_AC') then
                if (phydrm .gt. 0.0d0) then
                    sigeq(k) = coepre*dtaum(k) + (vala*phydrm)
                    sigeq(k) = sigeq(k)*coefpa
                else
                    sigeq(k) = coepre*dtaum(k)
                    sigeq(k) = sigeq(k)*coefpa
                endif
                grdeq(k) = sigeq(k)
!
            endif
!
!        CALC NOMBRE DE CYCLES A LA RUPTURE ET DU DOMMAGE
            call rccome(nommat, 'FATIGUE', phenom, icodre)
            if (icodre .eq. 1) call u2mess('F', 'FATIGUE1_24')
!        POUR CRITERE= DANG_VAN OU MATAKE
!
!
            call limend(nommat, grdeq, 'WOHLER', ' ', endur)
            if (endur) then
                nrupt(k)=r8maem()
            else
!
!
                call rcvale(nommat, 'FATIGUE', 1, 'SIGM    ', grdeq(k),&
                            1, 'WOHLER  ', nrupt(k), icodre, 1)
            endif
            dom(k) = 1.d0/nrupt(k)
            nrupt(k)= nint(nrupt(k))
!
        endif
!
120  end do
!
!
! ---------------------------------------------------------------
!           EVALUER CRITERES FOURNIS PAR FORMULE
!---------------------------------------------------------------
    do 100 k = 1, 2
!
        if (nomcri(1:7) .eq. 'FORMULE') then
!        NOMBRE DE PARAMETRES DISPONIBLES
            nparma = 22
!        VALEURS DE CES PARAMETRES, CORRESSPOND A NOMPAR
            valpar(1) = dtaum(k)
            valpar(2) = phydrm
            valpar(3) = normax(k)
            valpar(4) = normoy(k)
            valpar(5) = epnmax(k)
            valpar(6) = epnmoy(k)
            valpar(7) = vepspe/2.d0
            valpar(8) = (eprmax - eprmin)/2.d0
            valpar(9) = sinm1m
            valpar(10) = dendis
            valpar(11) = dendie
            valpar(12) = (phydrm - phymin)/2.d0
            valpar(13) = (phydrm + phymin)/2.d0
            valpar(14) = vsige/2.d0
            valpar(15) = (sigmax - sigmin)/2.d0
            valpar(16) = epnm1m
            valpar(17) = vepst/2.d0
            valpar(18) = (strema -stremi)/4.d0
            valpar(19) = (etrema -etremi)/4.d0
            valpar(20) = epspac
            valpar(21) = raysph
            valpar(22) = vsige/(2.d0*1.732051D0)
!
!  RECUPERER LES NOMS DE PARAMETRES FOURNIS PAR L'UTILISATEUR
            chnom(20:24) = '.PROL'
            chnom(1:19) = nomfor
!
            call jeveuo(chnom, 'L', jprof)
            call fonbpa(nomfor, zk24(jprof), cbid, nparma, np,&
                        nompf)
!
            do 30 j = 1, np
                do 25 ipar = 1, nparma
                    if (nompf(j) .eq. nompar(ipar)) then
                        valpu(j) = valpar(ipar)
                        goto 30
                    endif
25              continue
30          continue
!
            call fointe('F', nomfor, np, nompf, valpu,&
                        grdeq(k), ibid)
!
! PAS DE CRITERE DE FATEMI ET SOCIE EN ELASTIQUE ET AMPLITUDE CONSTANTE,
! CELAAS DE SENS.
!
!        CALC NOMBRE DE CYCLES A LA RUPTURE ET DU DOMMAGE
            call rccome(nommat, 'FATIGUE', phenom, icodre)
            if (icodre .eq. 1) call u2mess('F', 'FATIGUE1_24')
!
!        POUR CRITERE= FORMULE
!
!
            call limend(nommat, grdeq(k), grdvie, forvie, endur)
!
            if (endur) then
                nrupt(k)=r8maem()
            else
!
                if (grdvie(1:6) .eq. 'WOHLER') then
                    nomgrd = 'SIGM    '
                    grdvie(7:8) = '  '
                    call rcvale(nommat, 'FATIGUE', 1, nomgrd, grdeq(k),&
                                1, grdvie, nrupt(k), icodre, 1)
                endif
!
                if (grdvie(1:8) .eq. 'MANSON_C') then
                    nomgrd = 'EPSI    '
                    call rcvale(nommat, 'FATIGUE', 1, nomgrd, grdeq(k),&
                                1, grdvie, nrupt(k), icodre, 1)
                endif
!
                if (grdvie(1:8) .eq. 'FORM_VIE') then
!
                    call renrfa(forvie, grdeq(k), nrupt(k), icodre)
!
                endif
!
                dom(k) = 1.d0/nrupt(k)
                nrupt(k) = nint(nrupt(k))
            endif
!
        endif
!
100  continue
!
! ------------------------------------------------------------------
!---  SORTIE LES RESULTAT
!-------------------------------------------------------------------
!
!        CONSON D'UN CHAM_ELEM SIMPLE PUIS D'UN CHAM_ELEM CONTENANT
!        POURE POINT DE GAUSS DE CHAQUE MAILLE MAX DE DTAU_MAX ET LE
!        VECTRMAL ASSOCIE.
    do 40 i = 1, 24
        vresu(i) = 0.0d0
40  end do
!
    do 101 k = 1, 2
!
        vresu(1+(k-1)*11) = dtaum(k)
        vresu(2+(k-1)*11) = nxm(k)
        vresu(3+(k-1)*11) = nym(k)
        vresu(4+(k-1)*11) = nzm(k)
        vresu(5+(k-1)*11) = normax(k)
        vresu(6+(k-1)*11) = normoy(k)
        vresu(7+(k-1)*11) = epnmax(k)
        vresu(8+(k-1)*11) = epnmoy(k)
        vresu(9+(k-1)*11) = grdeq(k)
        vresu(10+(k-1)*11) = nrupt(k)
        vresu(11+(k-1)*11) = dom(k)
101  end do
    vresu(23) = 0.0d0
    vresu(24) = 0.0d0
!
!      CALL JEDEMA()
end subroutine
