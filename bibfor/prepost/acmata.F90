subroutine acmata(jvectn, jvectu, jvectv, nbordr, kwork,&
                  sompgw, jrwork, tspaq, ipg, jvecpg,&
                  jdtaum, jresun, nommet, vrespc)
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/r8pi.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/raycir.h'
    include 'asterfort/taurlo.h'
    include 'asterfort/vecnuv.h'
    include 'asterfort/wkvect.h'
    integer :: jvectn, jvectu, jvectv, nbordr, kwork
    integer :: sompgw, jrwork, tspaq, ipg, jvecpg, jdtaum, jresun
    character(len=16) :: nommet
    real(kind=8) :: vrespc(24)
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
! TOLE CRP_20 CRP_21
! ---------------------------------------------------------------------
! BUT: POUR LA FATIGUE A AMPLITUDE CONSTANTE
!      DETERMINER LE PLAN DES MAX DES TAU_MAX ET CALCULER DES GRANDEURS
!
!
! REMARQUE: CETTE SUBROUTINE EST APPLICABLE POUR UN NOEUD OU IPG EGALE
!           A 1 ET SOMPGW = SOMNOW,JVECPG = JVECNO
! ----------------------------------------------------------------------
! ARGUMENTS :
!     JVECTN  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS NORMAUX.
!     JVECTU  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS u DU PLAN DE CISAILLEMENT.
!     JVECTV  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
!                     VECTEURS v DU PLAN DE CISAILLEMENT.
!     NBORDR  : IN  : NOMBRE DE NUMEROS D'ORDRE.
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
!   VRSESU      OUT   TABLEAU DES RESULTATS (GRANDEURS ET DOMMAGE).
!                     POUR L'INSTANT, LA DIMENSION DE VRESU EST 24
! ----------------------------------------------------------------------
    integer :: i, j, k, n
    integer :: nbvec, dim, mnmax(2), jvpg1, jvpg2
    integer :: jvecn2, jvecu2, jvecv2, jvecn1, jvecu1, jvecv1
    integer :: adrs, decal
!
    real(kind=8) :: epsilo, gamma, pi
    real(kind=8) :: gammam, phim, dgam2, dphi2, phi0, dtaum(2)
    real(kind=8) :: nxm(2), nym(2), nzm(2)
    real(kind=8) :: sixx, siyy, sizz, sixy, sixz, siyz, fxm(2), fym(2)
    real(kind=8) :: fzm(2), epsxx, epsyy, epszz, epsxy, epsxz, epsyz
    real(kind=8) :: norm(2), normax(2), snorm(2), epsxm(2), epsym(2)
    real(kind=8) :: epszm(2), epnorm(2), epnmax(2), sepnmx(2), normoy(2)
    real(kind=8) :: epnmoy(2)
    real(kind=8) :: phydro, phydrm
!
!     ------------------------------------------------------------------
!
!234567
!
    call wkvect('&&ACMATA.VECT_NORMA1', 'V V R', 27, jvecn1)
    call wkvect('&&ACMATA.VECT_TANGU1', 'V V R', 27, jvecu1)
    call wkvect('&&ACMATA.VECT_TANGV1', 'V V R', 27, jvecv1)
    call wkvect('&&ACMATA.VECT_NORMA2', 'V V R', 27, jvecn2)
    call wkvect('&&ACMATA.VECT_TANGU2', 'V V R', 27, jvecu2)
    call wkvect('&&ACMATA.VECT_TANGV2', 'V V R', 27, jvecv2)
!
    call wkvect('&&ACMATA.VECTPG1', 'V V R', 18*nbordr, jvpg1)
    call wkvect('&&ACMATA.VECTPG2', 'V V R', 18*nbordr, jvpg2)
!
    epsilo = 1.0d-7
    pi = r8pi()
!
!
! PROJECTION DE L'HISTORIQUE DU CISAILLEMENT DANS UN PLAN.
!
    nbvec = 209
!
    call taurlo(nbvec, jvectn, jvectu, jvectv, nbordr,&
                kwork, sompgw, jrwork, tspaq, ipg,&
                jvecpg)
!
!
! CALCMAX DES DELTA_TAU MAX ET DU VECTEUR NORMAL ASSOCIE POUR
! LE PE GAUSS COURANT DE LA MAILLE COURANTE.
!
! 1/ RA ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
!    DAU POUR UN POINT DE GAUSS ET DU VECTEUR DE TRAVAIL
!    PANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.
!
! 2/ CDU RAYON CIRCONSCRIT
!
    call raycir(jvecpg, jdtaum, jresun, nbordr, nbvec,&
                nommet)
!
! 3/ CDU 1ER MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE
!
    dtaum(1) = 0.0d0
    dtaum(2) = 0.0d0
    mnmax(1) = 1
    mnmax(2) = 1
!
    do 430 i = 1, nbvec
        if (zr(jdtaum + (i-1)) .gt. epsilo) then
            if ((zr(jdtaum + (i-1))-dtaum(1))/zr(jdtaum + (i-1)) .gt. epsilo) then
                dtaum(2) = dtaum(1)
                mnmax(2) = mnmax(1)
                dtaum(1) = zr(jdtaum + (i-1))
                mnmax(1) = i
            endif
            if (( (zr(jdtaum + (i-1))-dtaum(2))/zr(jdtaum + (i-1)) .gt. epsilo ) .and.&
                (i .ne. mnmax(1))) then
                dtaum(2) = zr(jdtaum + (i-1))
                mnmax(2) = i
            endif
        endif
430  end do
!
! 4/ 1-ER RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
!    EAX DES DELTA_TAU (DETERMINATION DU VECTEUR NORMAL A 2
!    DPRES).
!
    phydro = 0.0d0
    phydrm = 0.0d0
    dim = 27
!
    do 440 k = 1, 2
        norm(k) = 0.0d0
        normax(k) = 0.0d0
        snorm(k) = 0.0d0
        epnorm(k) = 0.0d0
        epnmax(k) = 0.0d0
        sepnmx(k) = 0.0d0
!
        nxm(k) = zr(jvectn + (mnmax(k)-1)*3)
        nym(k) = zr(jvectn + (mnmax(k)-1)*3 + 1)
        nzm(k) = zr(jvectn + (mnmax(k)-1)*3 + 2)
        gammam = atan2(sqrt(abs(1.0d0-nzm(k)**2)),nzm(k))
        if (gammam .lt. 0.0d0) then
            gammam = gammam + pi
        endif
!
        if ((abs(nym(k)) .lt. epsilo) .and. (abs(nxm(k)) .lt. epsilo)) then
            phim = 0.0d0
        else
            phim = atan2(abs(nym(k)),nxm(k))
        endif
        if (phim .lt. 0.0d0) then
            phim = phim + pi
        endif
!
        if (abs(gammam) .lt. epsilo) then
            gamma = 5.0d0*(pi/180.0d0)
            dphi2 = 60.0d0*(pi/180.0d0)
            phi0 = 0.0d0
            n = 0
!
            call vecnuv(1, 6, gamma, phi0, dphi2,&
                        n, 1, dim, zr( jvecn2), zr(jvecu2),&
                        zr(jvecv2))
!
            gamma = 0.0d0
            phi0 = pi
!
            call vecnuv(1, 1, gamma, phi0, dphi2,&
                        n, 1, dim, zr( jvecn2), zr(jvecu2),&
                        zr(jvecv2))
!
            nbvec = 7
            call taurlo(nbvec, jvecn2, jvecu2, jvecv2, nbordr,&
                        kwork, sompgw, jrwork, tspaq, ipg,&
                        jvpg2)
        else
            dgam2 = 2.0d0*(pi/180.0d0)
            dphi2 = dgam2/sin(gammam)
            n = 0
            do 460 j = 1, 3
                gamma = gammam + (j-2)*dgam2
!
                call vecnuv(1, 3, gamma, phim, dphi2,&
                            n, 2, dim, zr(jvecn2), zr(jvecu2),&
                            zr(jvecv2))
!
460          continue
!
            nbvec = 9
            call taurlo(nbvec, jvecn2, jvecu2, jvecv2, nbordr,&
                        kwork, sompgw, jrwork, tspaq, ipg,&
                        jvpg2)
        endif
!
! 4-1/E A ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
!     TAU POUR UN POINT DE GAUSS ET DU VECTEUR DE TRAVAIL
!     TANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.
!
! 4-2/L DU RAYON CIRCONSCRIT
!
        call raycir(jvpg2, jdtaum, jresun, nbordr, nbvec,&
                    nommet)
!
! 4-3/L DU 2EME MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE
!
        dtaum(k) = 0.0d0
        mnmax(k) = 1
!
        do 480 i = 1, nbvec
            if (zr(jdtaum + (i-1)) .gt. dtaum(k)) then
                dtaum(k) = zr(jdtaum + (i-1))
                mnmax(k) = i
            endif
480      continue
!
! 5/ 2-EXIME RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
!    EAX DES DELTA_TAU (DETERMINATION DU VECTEUR NORMAL A 1
!    DRES).
!
        nxm(k) = zr(jvecn2 + (mnmax(k)-1)*3)
        nym(k) = zr(jvecn2 + (mnmax(k)-1)*3 + 1)
        nzm(k) = zr(jvecn2 + (mnmax(k)-1)*3 + 2)
        gammam = atan2(sqrt(abs(1.0d0-nzm(k)**2)),nzm(k))
        if (gammam .lt. 0.0d0) then
            gammam = gammam + pi
        endif
!
        if ((abs(nym(k)) .lt. epsilo) .and. (abs(nxm(k)) .lt. epsilo)) then
            phim = 0.0d0
        else
            phim = atan2(abs(nym(k)),nxm(k))
        endif
        if (phim .lt. 0.0d0) then
            phim = phim + pi
        endif
!
        if (abs(gammam) .lt. epsilo) then
            gamma = 1.0d0*(pi/180.0d0)
            dphi2 = 60.0d0*(pi/180.0d0)
            phi0 = 0.0d0
            n = 0
!
            call vecnuv(1, 6, gamma, phi0, dphi2,&
                        n, 1, dim, zr( jvecn1), zr(jvecu1),&
                        zr(jvecv1))
!
            gamma = 0.0d0
            phi0 = pi
!
            call vecnuv(1, 1, gamma, phi0, dphi2,&
                        n, 1, dim, zr( jvecn1), zr(jvecu1),&
                        zr(jvecv1))
!
            nbvec = 7
            call taurlo(nbvec, jvecn1, jvecu1, jvecv1, nbordr,&
                        kwork, sompgw, jrwork, tspaq, ipg,&
                        jvpg1)
        else
            dgam2 = 1.0d0*(pi/180.0d0)
            dphi2 = dgam2/sin(gammam)
            n = 0
            do 510 j = 1, 3
                gamma = gammam + (j-2)*dgam2
!
                call vecnuv(1, 3, gamma, phim, dphi2,&
                            n, 2, dim, zr(jvecn1), zr(jvecu1),&
                            zr(jvecv1))
!
510          continue
!
            nbvec = 9
            call taurlo(nbvec, jvecn1, jvecu1, jvecv1, nbordr,&
                        kwork, sompgw, jrwork, tspaq, ipg,&
                        jvpg1)
        endif
!
! 5-1/E A ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
!     TAU POUR UN POINT DE GAUSS ET DU VECTEUR DE TRAVAIL
!     TANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.
!
!
! 5-2/L DU RAYON CIRCONSCRIT
!
        call raycir(jvpg1, jdtaum, jresun, nbordr, nbvec,&
                    nommet)
!
! 5-3/L DU 2EME MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE
!
        dtaum(k) = 0.0d0
        mnmax(k) = 1
!
        do 530 i = 1, nbvec
            if (zr(jdtaum + (i-1)) .gt. dtaum(k)) then
                dtaum(k) = zr(jdtaum + (i-1))
                mnmax(k) = i
            endif
530      continue
!
        nxm(k) = zr(jvecn1 + (mnmax(k)-1)*3)
        nym(k) = zr(jvecn1 + (mnmax(k)-1)*3 + 1)
        nzm(k) = zr(jvecn1 + (mnmax(k)-1)*3 + 2)
        gammam = atan2(sqrt(abs(1.0d0-nzm(k)**2)),nzm(k))
!
        if (gammam .lt. 0.0d0) then
            gammam = gammam + pi
        endif
!
        if ((abs(nym(k)) .lt. epsilo) .and. (abs(nxm(k)) .lt. epsilo)) then
            phim = 0.0d0
        else
            phim = atan2(abs(nym(k)),nxm(k))
        endif
        if (phim .lt. 0.0d0) then
            phim = phim + pi
        endif
!
        if (abs(gammam) .lt. epsilo) then
            gamma = 0.5d0*(pi/180.0d0)
            dphi2 = 60.0d0*(pi/180.0d0)
            phi0 = 0.0d0
            n = 0
!
            call vecnuv(1, 6, gamma, phi0, dphi2,&
                        n, 1, dim, zr( jvecn1), zr(jvecu1),&
                        zr(jvecv1))
!
            gamma = 0.0d0
            phi0 = pi
!
            call vecnuv(1, 1, gamma, phi0, dphi2,&
                        n, 1, dim, zr( jvecn1), zr(jvecu1),&
                        zr(jvecv1))
!
            nbvec = 7
            call taurlo(nbvec, jvecn1, jvecu1, jvecv1, nbordr,&
                        kwork, sompgw, jrwork, tspaq, ipg,&
                        jvpg1)
        else
            dgam2 = 0.5d0*(pi/180.0d0)
            dphi2 = dgam2/sin(gammam)
            n = 0
            do 550 j = 1, 3
                gamma = gammam + (j-2)*dgam2
!
                call vecnuv(1, 3, gamma, phim, dphi2,&
                            n, 2, dim, zr(jvecn2), zr(jvecu2),&
                            zr(jvecv2))
!
550          continue
!
            nbvec = 9
            call taurlo(nbvec, jvecn2, jvecu2, jvecv2, nbordr,&
                        kwork, sompgw, jrwork, tspaq, ipg,&
                        jvpg2)
        endif
!
!
! 5/ 3-IEME RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
!    EAX DES DELTA_TAU (DETERMINATION DU VECTEUR NORMAL A 1
!    DRES)
!
! 5-2/L DU RAYON CIRCONSCRIT
!
        call raycir(jvpg2, jdtaum, jresun, nbordr, nbvec,&
                    nommet)
!
! 5-3/L DU 2EME MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE
!
        dtaum(k) = 0.0d0
        mnmax(k) = 1
!
        do 580 i = 1, nbvec
            if (zr(jdtaum + (i-1)) .gt. dtaum(k)) then
                dtaum(k) = zr(jdtaum + (i-1))
                mnmax(k) = i
            endif
580      continue
!
        nxm(k) = zr(jvecn2 + (mnmax(k)-1)*3)
        nym(k) = zr(jvecn2 + (mnmax(k)-1)*3 + 1)
        nzm(k) = zr(jvecn2 + (mnmax(k)-1)*3 + 2)
        gammam = atan2(sqrt(abs(1.0d0-nzm(k)**2)),nzm(k))
        if (gammam .lt. 0.0d0) then
            gammam = gammam + pi
        endif
!
        if ((abs(nym(k)) .lt. epsilo) .and. (abs(nxm(k)) .lt. epsilo)) then
            phim = 0.0d0
        else
            phim = atan2(abs(nym(k)),nxm(k))
        endif
        if (phim .lt. 0.0d0) then
            phim = phim + pi
        endif
!
        if (abs(gammam) .lt. epsilo) then
            gamma = 0.25d0*(pi/180.0d0)
            dphi2 = 60.0d0*(pi/180.0d0)
            phi0 = 0.0d0
            n = 0
!
            call vecnuv(1, 6, gamma, phi0, dphi2,&
                        n, 1, dim, zr( jvecn1), zr(jvecu1),&
                        zr(jvecv1))
!
            gamma = 0.0d0
            phi0 = pi
!
            call vecnuv(1, 1, gamma, phi0, dphi2,&
                        n, 1, dim, zr( jvecn1), zr(jvecu1),&
                        zr(jvecv1))
!
            nbvec = 7
            call taurlo(nbvec, jvecn1, jvecu1, jvecv1, nbordr,&
                        kwork, sompgw, jrwork, tspaq, ipg,&
                        jvpg1)
        else
            dgam2 = 1.0d0*(pi/180.0d0)
            dphi2 = dgam2/sin(gammam)
            n = 0
            do 570 j = 1, 3
                gamma = gammam + (j-2)*dgam2
!
                call vecnuv(1, 3, gamma, phim, dphi2,&
                            n, 2, dim, zr(jvecn1), zr(jvecu1),&
                            zr(jvecv1))
!
570          continue
!
            nbvec = 9
            call taurlo(nbvec, jvecn1, jvecu1, jvecv1, nbordr,&
                        kwork, sompgw, jrwork, tspaq, ipg,&
                        jvpg1)
        endif
!
! CALCLA CONTRAINTE NORMALE MAXIMALE SUR LE PLAN CRITIQUE,
! DE LRAINTE NORMALE MOYENNE SUR LE PLAN CRITIQUE,
! DE LRMATION NORMALE MAXIMALE SUR LE PLAN CRITIQUE,
! DE LRMATION NORMALE MOYENNE SUR LE PLAN CRITIQUE.
!
!          CALL RCVALE(NOMMAT,'ELAS',0,'        ',R8B,1,'E       ',
!      &               VALE,ICODRE,0)
!          IF (ICODRE .EQ. 1) THEN
!             CALL U2MESS('F','PREPOST_11')
!          ENDIF
!          CALL RCVALE(NOMMAT,'ELAS',0,'        ',R8B,1,'NU      ',
!      &               VALNU,ICODRE,0)
!          IF (ICODRE .EQ. 1) THEN
!             CALL U2MESS('F','PREPOST_12')
!          ENDIF
!          C1 = (1+VALNU)/VALE
!          C2 = VALNU/VALE
!
!
! 5/ 3-IEME RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
!    EAX DES DELTA_TAU (DETERMINATION DU VECTEUR NORMAL A 1
!    DRES)
!
! 5-2/L DU RAYON CIRCONSCRIT
!
        call raycir(jvpg1, jdtaum, jresun, nbordr, nbvec,&
                    nommet)
!
! 5-3/L DU 2EME MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE
!
        dtaum(k) = 0.0d0
        mnmax(k) = 1
!
        do 590 i = 1, nbvec
            if (zr(jdtaum + (i-1)) .gt. dtaum(k)) then
                dtaum(k) = zr(jdtaum + (i-1))
                mnmax(k) = i
            endif
590      continue
!
        nxm(k) = zr(jvecn1 + (mnmax(k)-1)*3)
        nym(k) = zr(jvecn1 + (mnmax(k)-1)*3 + 1)
        nzm(k) = zr(jvecn1 + (mnmax(k)-1)*3 + 2)
!
        do 540 i = 1, nbordr
            decal = 18
!           ADR = (J-1)*TSPAQ+KWORK*SOMPGW*6+(IPG-1)*6
            adrs = (i-1)*tspaq+kwork*sompgw*decal+(ipg-1)*decal
            sixx = zr(jrwork + adrs + 0)
            siyy = zr(jrwork + adrs + 1)
            sizz = zr(jrwork + adrs + 2)
            sixy = zr(jrwork + adrs + 3)
            sixz = zr(jrwork + adrs + 4)
            siyz = zr(jrwork + adrs + 5)
!
            epsxx = zr(jrwork + adrs + 6)
            epsyy = zr(jrwork + adrs + 7)
            epszz = zr(jrwork + adrs + 8)
            epsxy = zr(jrwork + adrs + 9)
            epsxz = zr(jrwork + adrs + 10)
            epsyz = zr(jrwork + adrs + 11)
!
!
! CALCLA PRESSION HYDROSTATIQUE MAXIMALE = Max_t(1/3 Tr[SIG])
!
            if (k .lt. 2) then
!
! ON C PHYDRM QU'UNE FOIS, PARCE QUE LA PRESSION HYDROSTATIQUE
! EST ANTE PAR RAPPORT AU vect_n.
!
                phydro = (sixx + siyy + sizz)/3.0d0
!
                if (phydro .gt. phydrm) then
                    phydrm = phydro
                endif
            endif
!
!             EPSXX = C1*SIXX - C2*(SIXX + SIYY + SIZZ)
!             EPSYY = C1*SIYY - C2*(SIXX + SIYY + SIZZ)
!             EPSZZ = C1*SIZZ - C2*(SIXX + SIYY + SIZZ)
!             EPSXY = C1*SIXY
!             EPSXZ = C1*SIXZ
!             EPSYZ = C1*SIYZ
!
! CALCvect_F = [SIG].vect_n
!
            fxm(k) = sixx*nxm(k) + sixy*nym(k) + sixz*nzm(k)
            fym(k) = sixy*nxm(k) + siyy*nym(k) + siyz*nzm(k)
            fzm(k) = sixz*nxm(k) + siyz*nym(k) + sizz*nzm(k)
!
! CALCNORM = vect_F.vect_n
!
            norm(k) = fxm(k)*nxm(k) + fym(k)*nym(k) + fzm(k)*nzm(k)
!
            if (abs(norm(k)) .gt. normax(k)) then
                normax(k) = norm(k)
            endif
!
            snorm(k) = snorm(k) + norm(k)
!
! CALCvect_EPS = [EPS].vect_n
!
            epsxm(k) = epsxx*nxm(k) + epsxy*nym(k) + epsxz*nzm(k)
            epsym(k) = epsxy*nxm(k) + epsyy*nym(k) + epsyz*nzm(k)
            epszm(k) = epsxz*nxm(k) + epsyz*nym(k) + epszz*nzm(k)
!
! CALCEPSILON NORMALE = vect_EPS.vect_n
!
            epnorm(k) = epsxm(k)*nxm(k) + epsym(k)*nym(k) + epszm(k)* nzm(k)
!
            if (abs(epnorm(k)) .gt. epnmax(k)) then
                epnmax(k) = epnorm(k)
            endif
!
            sepnmx(k) = sepnmx(k) + epnorm(k)
540      continue
!
        normoy(k) = snorm(k)/nbordr
        epnmoy(k) = sepnmx(k)/nbordr
!
!
440  end do
!
! CONSON D'UN CHAM_ELEM SIMPLE PUIS D'UN CHAM_ELEM CONTENANT
! POURE POINT DE GAUSS DE CHAQUE MAILLE MAX DE DTAU_MAX ET LE
! VECTRMAL ASSOCIE.
!
!
    vrespc(1) = dtaum(1)
    vrespc(2) = nxm(1)
    vrespc(3) = nym(1)
    vrespc(4) = nzm(1)
    vrespc(5) = normax(1)
    vrespc(6) = normoy(1)
    vrespc(7) = epnmax(1)
    vrespc(8) = epnmoy(1)
    vrespc(9) = 0.0d0
    vrespc(10) = 0.0d0
    vrespc(11) = 0.0d0
    vrespc(12) = dtaum(2)
    vrespc(13) = nxm(2)
    vrespc(14) = nym(2)
    vrespc(15) = nzm(2)
    vrespc(16) = normax(2)
    vrespc(17) = normoy(2)
    vrespc(18) = epnmax(2)
    vrespc(19) = epnmoy(2)
    vrespc(20) = 0.0d0
    vrespc(21) = 0.0d0
    vrespc(22) = 0.0d0
    vrespc(23) = 0.0d0
    vrespc(24) = 0.0d0
!
    call jedetr('&&ACMATA.VECT_NORMA1')
    call jedetr('&&ACMATA.VECT_TANGU1')
    call jedetr('&&ACMATA.VECT_TANGV1')
    call jedetr('&&ACMATA.VECT_NORMA2')
    call jedetr('&&ACMATA.VECT_TANGU2')
    call jedetr('&&ACMATA.VECT_TANGV2')
    call jedetr('&&ACMATA.VECTPG1')
    call jedetr('&&ACMATA.VECTPG2')
end subroutine
