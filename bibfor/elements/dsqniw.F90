subroutine dsqniw(qsi, eta, caraq4, dci, bcm,&
                  bcb, bca, an, am, wsq,&
                  wmesq)
    implicit  none
    real(kind=8) :: qsi, eta, caraq4(*), dci(2, 2), bcb(2, 12), bca(2, 4)
    real(kind=8) :: an(4, 12), am(4, 8), bcm(2, 8), wsq(12), wmesq(8)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!  DSQNIW -- CALCUL DES FONCTIONS DE FORME CUBIQUES RELATIVES A
!            LA FLECHE W DANS LE CADRE DU CALCUL DE LA MATRICE
!            DE MASSE POUR LES ELEMENTS DSQ .
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
!            ET ENFIN (T) = [BCA] * (ALPHA) + [BCB]
!            AVEC (ALPHA) = [AN]*(UN)   DANS LE CAS NON EXCENTRE
!              ET (ALPHA) = [AN]*(UN) + [AM]*(UM) DANS LE CAS EXCENTRE
!
!            ON ABOUTIT A L'EXPRESSION :
!            W = WST*UN + WMESQ*UM
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
!    BCM(2,8)       IN    R       MATRICE RESULTANT DE LA DERIVATION
!                                 SECONDE DES FONCTIONS DE FORME
!                                 RELATIVES AUX DEPLACEMENTS EN
!                                 MEMBRANE ET APPARAISSANT DANS
!                                 L'EXPRESSION DES EFFORTS TRANCHANTS
!                                 (T) = [BCA]*(ALPHA) + [BCB]*(UFB)
!                                     + [BCM]*(UM)
!    BCB(2,12)      IN    R       MATRICE RESULTANT DE LA DERIVATION
!                                 SECONDE DES FONCTIONS DE FORME
!                                 DES ROTATIONS ET APPARAISSANT DANS
!                                 L'EXPRESSION DES EFFORTS TRANCHANTS
!                                 (T) = [BCA]*(ALPHA) + [BCB]*(UFB)
!                                     + [BCM]*(UM)
!    BCA(2,4)       IN    R       MATRICE RELIANT LES EFFORTS
!                                 TRANCHANTS AUX ROTATIONS ALPHA
!                                 (T) = [BCA]*(ALPHA) + [BCB]*(UFB)
!                                     + [BCM]*(UM)
!    AN(4,12)       IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
!                                 AUX INCONNUES DE FLEXION UN
!    AM(4,8)        IN    R       MATRICE RELIANT LES ROTATIONS ALPHA
!                                 AUX INCONNUES DE MEMBRANE UM
!    WSQ(12)        OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 W = WSQ*UN (+ WMESQ*UM)
!    WMESQ(8)       OUT   R       FONCTIONS DE FORME TELLES QUE
!                                 W = WMESQ*UM (+ WSQ*UN)
!     ------------------------------------------------------------------
    integer :: i, j, k
    real(kind=8) :: bn(2, 12), dba(2, 12), db(2, 4), dbam(2, 8), dcm(2, 8)
    real(kind=8) :: n(12)
    real(kind=8) :: pqsi, mqsi, peta, meta, qsic, etac
    real(kind=8) :: x5, x6, x7, x8, y5, y6, y7, y8
    real(kind=8) :: zero, undemi, un, huit
!     ------------------------------------------------------------------
    zero = 0.0d0
    undemi = 0.5d0
    un = 1.0d0
    huit = 8.0d0
!
    x5 = caraq4(1)
    x6 = caraq4(2)
    x7 = caraq4(3)
    x8 = caraq4(4)
    y5 = caraq4(5)
    y6 = caraq4(6)
    y7 = caraq4(7)
    y8 = caraq4(8)
!
    do 10 i = 1, 8
        wmesq(i) = zero
10  end do
!
    peta = un + eta
    meta = un - eta
    pqsi = un + qsi
    mqsi = un - qsi
    etac = un - eta * eta
    qsic = un - qsi * qsi
!
! --- FONCTIONS DE FORME RELATIVES A LA FLECHE CORRESPONDANTES
! --- A L'INTERPOLATION DE TYPE HERMITE :
! ---     W = NI*WI + NQSII*(D WI)/(D QSI) + NETAI*(D WI)/(D ETA) :
!     -----------------------------------------------------------
    n(1) = mqsi * meta / huit * (qsic + etac - qsi - eta)
    n(2) = mqsi * meta / huit * qsic
    n(3) = mqsi * meta / huit * etac
    n(4) = pqsi * meta / huit * (qsic + etac + qsi - eta)
    n(5) = - pqsi * meta / huit * qsic
    n(6) = pqsi * meta / huit * etac
    n(7) = pqsi * peta / huit * (qsic + etac + qsi + eta)
    n(8) = - pqsi * peta / huit * qsic
    n(9) = - pqsi * peta / huit * etac
    n(10) = mqsi * peta / huit * (qsic + etac - qsi + eta)
    n(11) = mqsi * peta / huit * qsic
    n(12) = - mqsi * peta / huit * etac
!
! --- CALCUL DE (GAMMA) = [DCI]*(T)
! --- SOIT      (GAMMA) = [DCI]*([BCA]*[AN]*(UN) + [BCB])
! ---                       S'IL N'Y A PAS D'EXCENTREMENT
! ---
! ---           (GAMMA) = [DCI]*([BCA]*([AN]*(UN) +[AM]*(UM)) + [BCB])
! ---                            SI LA PLAQUE EST EXCENTREE
! ---   EN FAIT ON CALCULE [DCI]*([BCA]*[AN] + [BCB])
! ---                   ET [DCI]*[BCA]*[AM] :
!       ==================================
!
! ---   CALCUL DE  [DCI]*([BCA]*[AN] + [BCB]) :
!       -------------------------------------
    do 20 i = 1, 2
        do 20 j = 1, 12
            bn(i,j) = zero
20      continue
    do 30 j = 1, 12
        do 40 k = 1, 4
            bn(1,j) = bn(1,j) + bca(1,k)*an(k,j)
            bn(2,j) = bn(2,j) + bca(2,k)*an(k,j)
40      end do
        bn(1,j) = bn(1,j) + bcb(1,j)
        bn(2,j) = bn(2,j) + bcb(2,j)
30  end do
    do 50 j = 1, 12
        dba(1,j) = dci(1,1)*bn(1,j) + dci(1,2)*bn(2,j)
        dba(2,j) = dci(2,1)*bn(2,j) + dci(2,2)*bn(2,j)
50  end do
!
! ---   FONCTIONS D'INTERPOLATION WST RELATIVES AUX DDLS DE FLEXION
! ---   W, BETA_X ET BETA_Y :
!       -------------------
    do 60 j = 1, 12
        wsq(j) = (&
                 - dba(1,j)*x5 + dba(2,j)*x8) * n(2) + (- dba(1,j)* y5 + dba(2,j)*y8) * n(2) + (-&
                 & dba(1,j)*x5 - dba(2,j)*x6) * n( 5) + (- dba(1,j)*y5 - dba(2,j)*y6) * n(5) + ( &
                 &dba(1,j)*x7 - dba(2,j)*x6) * n(8) + ( dba(1,j)*y7 - dba(2,j)*y6) * n(8) + ( dba&
                 &(1,j)*x7 + dba(2,j)*x8) * n(11) + ( dba(1,j)*y7 + dba( 2,j)*y8) * n(11&
                 )
60  end do
!
    wsq(1) = wsq(1) + n(1)
    wsq(2) = wsq(2) + (- x5*n(2) + x8*n(3) ) * undemi
    wsq(3) = wsq(3) + (- y5*n(2) + y8*n(3) ) * undemi
    wsq(4) = wsq(4) + n(4)
    wsq(5) = wsq(5) + (- x5*n(5) - x6*n(6) ) * undemi
    wsq(6) = wsq(6) + (- y5*n(5) - y6*n(6) ) * undemi
    wsq(7) = wsq(7) + n(7)
    wsq(8) = wsq(8) + ( x7*n(8) - x6*n(9) ) * undemi
    wsq(9) = wsq(9) + ( y7*n(8) - y6*n(9) ) * undemi
    wsq(10) = wsq(10) + n(10)
    wsq(11) = wsq(11) + ( x7*n(11) + x8*n(12)) * undemi
    wsq(12) = wsq(12) + ( y7*n(11) + y8*n(12)) * undemi
!
! ---   CALCUL DE  [DCI]*([BCA]*[AM]+[BCM]) :
!       -----------------------------------
    do 70 j = 1, 4
        db(1,j) = dci(1,1)*bca(1,j) + dci(1,2)*bca(2,j)
        db(2,j) = dci(2,1)*bca(1,j) + dci(2,2)*bca(2,j)
70  end do
    do 80 j = 1, 8
        dbam(1,j) = db(1,1)*am(1,j) + db(1,2)*am(2,j) + db(1,3)*am(3, j) + db(1,4)*am(4,j)
        dbam(2,j) = db(2,1)*am(1,j) + db(2,2)*am(2,j) + db(2,3)*am(3, j) + db(2,4)*am(4,j)
80  end do
    do 90 j = 1, 8
        dcm(1,j) = dci(1,1)*bcm(1,j) + dci(1,2)*bcm(2,j)
        dcm(2,j) = dci(2,1)*bcm(1,j) + dci(2,2)*bcm(2,j)
90  end do
    do 100 j = 1, 8
        dbam(1,j) = dbam(1,j) + dcm(1,j)
        dbam(2,j) = dbam(2,j) + dcm(2,j)
100  end do
!
! ---   FONCTIONS D'INTERPOLATION WMESQ RELATIVES AUX DDLS DE
! ---   MEMBRANE U ET V :
!       ---------------
    do 110 j = 1, 8
        wmesq(j) = (&
                   - dbam(1,j)*x5 + dbam(2,j)*x8) * n(2) + (- dbam(1, j)*y5 + dbam(2,j)*y8) * n(2&
                   &) + (- dbam(1,j)*x5 - dbam(2,j)*x6) * n(5) + (- dbam(1,j)*y5 - dbam(2,j)*y6) &
                   &* n(5) + ( dbam(1,j)* x7 - dbam(2,j)*x6) * n(8) + ( dbam(1,j)*y7 - dbam(2,j)*&
                   &y6) * n(8) + ( dbam(1,j)*x7 + dbam(2,j)*x8) * n(11) + ( dbam(1,j)* y7 + dbam(&
                   &2,j)*y8) * n(11&
                   )
110  end do
!
end subroutine
