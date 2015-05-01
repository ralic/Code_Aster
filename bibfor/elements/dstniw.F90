subroutine dstniw(qsi, eta, carat3, dci, bca,&
                  an, am, wst, wmest)
    implicit  none
    real(kind=8) :: qsi, eta, carat3(*), dci(2, 2), bca(2, 3), an(3, 9)
    real(kind=8) :: am(3, 6), wst(9), wmest(6)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!.======================================================================
!
!  DSTNIW -- CALCUL DES FONCTIONS DE FORME CUBIQUES RELATIVES A
!            LA FLECHE W DANS LE CADRE DU CALCUL DE LA MATRICE
!            DE MASSE POUR LES ELEMENTS DST .
!            POUR LA RIGIDITE CES FONCTIONS SONT LINEAIRES,
!            POUR LA MASSE ON CHOISIT DES FONCTIONS CUBIQUES
!            EN CONSIDERANT QUE L'ELEMENT EST DE TYPE HERMITE
!            (ICI CUBIQUE INCOMPLET) , SOIT :
!            W = NI*WI + NQSII*(D WI)/(D QSI) + NETAI*(D WI)/(D ETA)
!            ON UTILISE LE FAIT QUE :
!                (D W)/(D X) = GAMMA_X - BETA_X
!            ET  (D W)/(D Y) = GAMMA_Y - BETA_Y
!
!            D'AUTRE PART , ON SAIT QUE :
!            | GAMMA_X |           | TX |
!            | GAMMA_Y | = [DCI] * | TY | = [DCI] * (T)
!
!            ET ENFIN (T) = [BCA] * (ALPHA)
!            AVEC (ALPHA) = [AN]*(UN)   DANS LE CAS NON EXCENTRE
!              ET (ALPHA) = [AN]*(UN) + [AM]*(UM) DANS LE CAS EXCENTRE
!
!            ON ABOUTIT A L'EXPRESSION :
!            W = WST*UN + WMEST*UM
!
!            UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)
!            UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)
!
!   ARGUMENT        E/S  TYPE         ROLE
!    INT            IN    I       INDICE DU POINT D'INTEGRATION
!    R(*)           IN    R       TABLEAU DE CARACTERISTIQUES
!                                 GEOMETRIQUES DE L'ELEMENT :
!                                 COS ET SIN DES ANGLES, LONGUEUR
!                                 DES COTES ,...
!    DCI(2,2)       IN    R       INVERSE DE LA MATRICE DE
!                                 CISAILLEMENT
!    BCA(2,3)       IN    R       MATRICE RELIANT LES EFFORTS
!                                 TRANCHANTS AUX ROTATIONS ALPHA
!                                 (T) = [BCA]*(ALPHA)
!    AN(3,9)        IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
!                                 AUX INCONNUES DE FLEXION UN
!    AM(3,6)        IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
!                                 AUX INCONNUES DE MEMBRANE UM
!    WST(9)         OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 W = WST*UN (+ WMEST*UM)
!    WMEST(6)       OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 W = WMEST*UM (+ WST*UN)
!     ------------------------------------------------------------------
    integer :: i, j
    real(kind=8) :: db(2, 3), dba(2, 9), dbam(2, 6), n(9)
    real(kind=8) :: lbd, zero, x4, x6, y4, y6
!     ------------------------------------------------------------------
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    zero = 0.0d0
!
    x4 = carat3(1)
    x6 = carat3(3)
    y4 = carat3(4)
    y6 = carat3(6)
!
    do 10 i = 1, 6
        wmest(i) = zero
10  end do
!
    lbd = 1.d0 - qsi - eta
!
! --- FONCTIONS DE FORME RELATIVES A LA FLECHE CORRESPONDANTES
! --- A L'INTERPOLATION DE TYPE HERMITE :
! ---     W = NI*WI + NQSII*(D WI)/(D QSI) + NETAI*(D WI)/(D ETA) :
!     -----------------------------------------------------------
    n(1) = lbd*lbd * (3.d0 - 2.d0*lbd) + qsi*eta*lbd * 2.d0
    n(2) = lbd*lbd * qsi + qsi*eta*lbd / 2.d0
    n(3) = lbd*lbd * eta + qsi*eta*lbd / 2.d0
    n(4) = qsi*qsi * (3.d0 - 2.d0*qsi) + qsi*eta*lbd * 2.d0
    n(5) = qsi*qsi * (-1.d0 + qsi) - qsi*eta*lbd
    n(6) = qsi*qsi * eta + qsi*eta*lbd / 2.d0
    n(7) = eta*eta * (3.d0 - 2.d0*eta) + qsi*eta*lbd * 2.d0
    n(8) = eta*eta * qsi + qsi*eta*lbd / 2.d0
    n(9) = eta*eta * (-1.d0 + eta) - qsi*eta*lbd
!
! --- CALCUL DE (GAMMA) = [DCI]*(T)
! --- SOIT      (GAMMA) = [DCI]*[BCA]*[AN]*(UN) S'IL N'Y A PAS
! ---                                           D'EXCENTREMENT
! ---           (GAMMA) = [DCI]*[BCA]*([AN]*(UN) +[AM]*(UM))
! ---                            SI LA PLAQUE EST EXCENTREE
! ---   EN FAIT ON CALCULE [DCI]*[BCA]*[AN] ET [DCI]*[BCA]*[AM] :
!       =======================================================
!
! ---   CALCUL DE  [DCI]*[BCA]*[AN] :
!       ---------------------------
    do 20 j = 1, 3
        db(1,j) = dci(1,1)*bca(1,j) + dci(1,2)*bca(2,j)
        db(2,j) = dci(2,1)*bca(1,j) + dci(2,2)*bca(2,j)
20  end do
    do 30 j = 1, 9
        dba(1,j) = db(1,1)*an(1,j) + db(1,2)*an(2,j) + db(1,3)*an(3,j)
        dba(2,j) = db(2,1)*an(1,j) + db(2,2)*an(2,j) + db(2,3)*an(3,j)
30  end do
!
! ---   CALCUL DE  [DCI]*[BCA]*[AM] :
!       ---------------------------
    do 40 j = 1, 6
        dbam(1,j) = db(1,1)*am(1,j) + db(1,2)*am(2,j) + db(1,3)*am(3, j)
        dbam(2,j) = db(2,1)*am(1,j) + db(2,2)*am(2,j) + db(2,3)*am(3, j)
40  end do
!
! ---   FONCTIONS D'INTERPOLATION WST RELATIVES AUX DDLS DE FLEXION
! ---   W, BETA_X ET BETA_Y :
!       -------------------
    do 50 j = 1, 9
        wst(j) = (&
                 dba(1,j)*x4 + dba(2,j)*y4) * (n(2) + n(5) + n(8)) - (dba(1,j)*x6 + dba(2,j)*y6) &
                 &* (n(3) + n(6) + n(9)&
                 )
50  end do
!
    wst(1) = wst(1) + n(1)
    wst(2) = wst(2) - n(2)*x4 + n(3)*x6
    wst(3) = wst(3) - n(2)*y4 + n(3)*y6
    wst(4) = wst(4) + n(4)
    wst(5) = wst(5) - n(5)*x4 + n(6)*x6
    wst(6) = wst(6) - n(5)*y4 + n(6)*y6
    wst(7) = wst(7) + n(7)
    wst(8) = wst(8) - n(8)*x4 + n(9)*x6
    wst(9) = wst(9) - n(8)*y4 + n(9)*y6
!
! ---   FONCTIONS D'INTERPOLATION WMEST RELATIVES AUX DDLS DE
! ---   MEMBRANE U ET V :
!       ---------------
    do 60 j = 1, 6
        wmest(j) = (&
                   dbam(1,j)*x4 + dbam(2,j)*y4) * (n(2) + n(5) + n(8) ) - (dbam(1,j)*x6 + dbam(2,&
                   &j)*y6) * (n(3) + n(6) + n(9)&
                   )
60  end do
!
end subroutine
