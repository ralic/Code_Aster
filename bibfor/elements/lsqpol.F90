subroutine lsqpol(ordre, e1, npt, xx, yy,&
                  ordok, poly, sigma)
! aslint: disable=W1306
    implicit none
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
!
! ------------------------------------------------------------------
!
!              REGRESSION POLYNOMIALE DE TYPE MOINDRE CARRES
!                  (LEAST SQUARES POLYNOMIAL FITTING)
!
!              REFERENCE: BASIC SCIENTIFIC SUBROUTINES, VOL. II
!                         F.R. RUCKDESCHEL, BYTE/MCGRAWW-HILL, 1981
!                         ET D'APRES J-P MOREAU, PARIS
!
!                                                   P.KOECHLIN 02-2004
!
! ------------------------------------------------------------------
!
!   ENTREE
!       ORDRE : ORDRE DE LA REGRESSION (ORDRE>0)
!               = DEGRE MAX DU POLYNOME QUE L'ON CHERCHE
!       E1    : SI E1/=0 : PARAMETRE SERVANT DE CRITERE POUR PASSER A
!               L'ORDRE SUPERIEUR
!               ON COMPARE E1 A LA DIMINUTION DE LA DEVIATION STANDARD
!       NPT   : NOMBRE DE POINTS (NPT>1)
!       XX    : ABSCISSE DES POINTS (XX DISTINCTS)
!       YY    : ORDONNEES DES POINTS
!
!   SORTIE
!       ORDOK : ORDRE FINALEMENT OBTENU >=0
!                  (  AVANT VERIFICATION DU DEGRE:
!                        ORDOK <= ORDRE  SI E1/=0
!                        ORDOK = ORDRE   SI E1=0     )
!                  ORDOK=-1 CORRESPOND A UNE ERREUR
!       POLY  : POLYNOME OBTENU
!               POLY=POLY(1) + POLY(2)*X + POLY(2)*X^2 +...+ POLY(N)*X^N
!       SIGMA : DEVIATION STANDARD
!
!
!      LA ROUTINE N'EST VALABLE QUE POUR ORDRE >=1
!      MAIS EN SORTIE, ON A ORDOK >=0
!
    real(kind=8) :: grand
    parameter (grand = 1.0d20)
!
    integer :: ordre, npt
    real(kind=8) :: xx(npt), yy(npt)
    real(kind=8) :: poly(ordre+1), sigma
!
    real(kind=8) :: aa(ordre+1), bb(ordre+1), ff(ordre+1), poly2(ordre+1)
    real(kind=8) :: vv(npt), dd(npt), ee(npt)
    integer :: i, j, ordok, ordok2, ordloo
    real(kind=8) :: a1, a2, b1, b2, poly1, d1, e1, f1, f2, v1, v2
!
    do 10, i = 1,ordre+1
    poly(i) = 0.d0
    10 end do
    ordok = -1
!
    if (ordre .lt. 1) then
        goto 999
    endif
!
!      ON A TOUJOURS DES POINTS DISTINCTS (LES XX SONT DISTINCTS)
!      ON A TOUJOURS NPT>1
!
    if (npt .eq. 2) then
!         UNIQUEMENT POUR + DE PRECISION: MAIS LA ROUTINE MARCHE AUSSI
!         POUR NPT=2
        ordok = 1
        poly(2)=(yy(2)-yy(1))/(xx(2)-xx(1))
        poly(1)= (yy(1)+yy(2)-poly(2)*(xx(1)+xx(2)))/2.d0
        sigma=0.d0
        goto 999
    endif
!
    v1 = grand
!
! --- INITIALISATION ------------------------------
!
    do 13, i = 1,ordre+1
    aa(i) = 0.d0
    bb(i) = 0.d0
    ff(i) = 0.d0
    13 end do
    do 16, i = 1,npt
    vv(i) = 0.d0
    dd(i) = 0.d0
    16 end do
!
    d1 = sqrt(npt*1.0d0)
!
    do 18, i = 1,npt
    ee(i) = 1.d0 / d1
    18 end do
    f1 = d1
!
    a1 = 0.d0
    do 20, i=1, npt
    a1 = a1 + xx(i) * ee(i) * ee(i)
    20 end do
!
    poly1 = 0.d0
    do 30, i=1, npt
    poly1 = poly1 + yy(i) * ee(i)
    30 end do
!
    bb(1) = 1.d0 / f1
    ff(1) = bb(1) * poly1
!
    do 35, i = 1,npt
    vv(i) = vv(i) + poly1*ee(i)
    35 end do
!
! --- DEBUT BOUCLE ----------------------------------------
!
    do 40, ordloo=1,ordre
!
! SAVE LATEST RESULTS
    do 45, i = 1,ordre+1
    poly2(i) = poly(i)
45  continue
    ordok2 = ordok
    v2 = v1
    f2 = f1
    a2 = a1
!
    f1 = 0.d0
    do 50, i=1, npt
    b1 = ee(i)
    ee(i) = (xx(i) - a2) * b1 - f2 * dd(i)
    dd(i) = b1
    f1 = f1 + ee(i) * ee(i)
50  continue
!
    f1 = sqrt(f1)
    do 55, i=1, npt
    ee(i) = ee(i) / f1
55  continue
    a1 = 0.d0
    do 60, i=1, npt
    a1 = a1 + xx(i) * ee(i) * ee(i)
60  continue
!
    poly1 = 0.d0
    do 70, i=1, npt
    poly1 = poly1 + yy(i) * ee(i)
70  continue
!
    do 80, i=0,ordloo
    j = ordloo - i + 1
    b2 = bb(j)
    d1 = 0.d0
    if (j .gt. 1) d1 = bb(j - 1)
    d1 = d1 - a2 * bb(j) - f2 * aa(j)
    bb(j) = d1 / f1
    aa(j) = b2
80  continue
!
    do 83, i = 1,npt
    vv(i) = vv(i) + poly1*ee(i)
83  continue
!
    do 86, i = 1,ordre+1
    ff(i) = ff(i) + poly1*bb(i)
86  continue
!
    do 88, i = 1,ordre+1
    poly(i) = ff(i)
88  continue
!
    ordok = ordloo
!
    sigma = 0.d0
    do 90, i=1, npt
    sigma = sigma + (vv(i) - yy(i)) * (vv(i) - yy(i))
90  continue
!
!        NOTE THE DIVISION IS BY THE NUMBER OF DEGREES OF FREEDOM
    if (npt .gt. ordloo + 1) then
!          SIGMA = SQRT(SIGMA / DFLOAT(NPT - ORDLOO - 1))
        sigma = sqrt(sigma / (npt - ordloo - 1))
    else
        goto 999
    endif
!
    if (e1 .gt. 0.d0) then
!          TEST FOR MINIMAL IMPROVEMENT OR IF ERROR IS LARGER, QUIT
        if (( abs(v1 - sigma) .lt. (e1*sigma) ) .or. ( e1 * sigma .gt. e1 * v1 )) then
!           ABORTED SEQUENCE, RECOVER LAST VALUES
            ordok = ordok2
            sigma = v2
            do 100, i = 1,ordre+1
            poly(i) = poly2(i)
100          continue
            goto 999
        endif
    endif
!
    v1 = sigma
!
    40 end do
999  continue
end subroutine
