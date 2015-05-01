subroutine bsigma(ip, xl, phiy, phiz, b,&
                  intpol)
    implicit none
    integer :: ip, intpol
    real(kind=8) :: xl, phiy, phiz, b(4, 14)
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
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES FONCTIONS DE FORME DE DEFORMATIONS
!               GENERALISEES POUTRE 7 DDL A TROIS  POINTS DE GAUSS
!               POUR LE CALCUL DE LA RIGIDITE GEOMETRIQUE
!
!    - ARGUMENTS:
!        DONNEES:           IP      -->   POINTS DE GAUSS
!                           XL      -->   LONGUEUR DE L'ELEMENT
!                          PHIY     -->  COEFF DE CISAILLEMENT SUIVANT Y
!                          PHIZ     -->  COEFF DE CISAILLEMENT SUIVANT Z
!                         INTPOL    -->  INTERPOLATION DES FCTS DE FORME
!                                        (0) LINEAIRE
!                                        (1) CUBIQUE TORSION/FLEXION
!
!        RESULTATS:
!                         B     <--  MATRICE D'INTERPOLATION
!
!     DESCRIPTION DE LA NUMEROTATION DU SEG2
!
!       +-----------------+
!       1                 2
!
!    L'ORDRE DE DDL EST  : 1   UX   -SUIVANT L'AXE DE LA POUTRE
!                          2   UY   -I
!                          3   UZ    I DANS LA SECTION
!
!                          4   TX   -.ROTATIONS SUIVANT OX,OY,OZ
!                          5   TY    .
!                          6   TZ    .
!                          7   TX'   .PARAMETRE DE GAUCHISSEMENT
!
!    DEFORMATIONS        : 1   TX    .ROTATION SUIVANT OX
!                          2   UY'   .DERIVEE DE UY PAR RAPPORT A X
!                          3   UZ'   .DERIVEE DE UZ PAR RAPPORT A X
!                          4   TX'   .PARAMETRE DE GAUCHISSEMENT
! ......................................................................
    integer :: i, j
    real(kind=8) :: a, k, dy, dz
! ......................................................................
!
    a = 0.5d0*xl
!
    if (ip .eq. 1) then
        k = -sqrt(0.6d0)
    else if (ip.eq.2) then
        k = 0.d0
    else if (ip.eq.3) then
        k = sqrt(0.6d0)
    endif
!
    do 2 j = 1, 14
        do 1 i = 1, 4
            b(i,j) = 0
 1      continue
 2  end do
!
    if (intpol .eq. 0) then
        goto 100
    else if (intpol .eq. 1) then
        goto 200
    endif
!
! -------------------------------------------------------
200  continue
! --- INTERPOLATION CUBIQUE COHERENTE AVEC CELLE CHOISIE
! --- POUR LE CALCUL DE LA MATRICE DE RIGIDITE MATERIELLE
!
! TX
!
    b(1,4) = ((1.d0-k)*(1.d0-k)*(2.d0+k))/ (4.d0)
    b(1,7) = a*((1.d0-k)*(1.d0-(k**2)))/ (4.d0)
    b(1,11) = ((1.d0+k)*(1.d0+k)*(2.d0-k))/ (4.d0)
    b(1,14) = a*((1.d0+k)*(-1.d0+(k**2)))/ (4.d0)
!
! UY'
!
    dy = 1.d0/ (1.d0+phiy)
    b(2,2) = (3.d0*dy*k**2-2.d0-dy)/ (4.d0*a)
    b(2,6) = (3.d0*dy*k**2-2.d0*k-dy)/ (4.d0)
    b(2,9) = -b(2,2)
    b(2,13) = (3.d0*dy*k**2+2.d0*k-dy)/ (4.d0)
!
! UZ'
!
    dz = 1.d0/ (1.d0+phiz)
    b(3,3) = (3.d0*dz*k**2-2.d0-dz)/ (4.d0*a)
    b(3,5) = - (3.d0*dz*k**2-2.d0*k-dz)/ (4.d0)
    b(3,10) = -b(3,3)
    b(3,12) = - (3.d0*dz*k**2+2.d0*k-dz)/ (4.d0)
!
! TX'
!
    b(4,4) = (3.d0*k**2-3)/ (4.d0*a)
    b(4,7) = (3.d0*k**2-2.d0*k-1.d0)/ (4.d0)
    b(4,11) = (-3.d0*k**2+3.d0)/ (4.d0*a)
    b(4,14) = (3.d0*k**2+2.d0*k-1.d0)/ (4.d0)
!
    goto 9999
!
! -------------------------------------------------------
100  continue
! --- INTERPOLATION LINEAIRE POUR TOUS LES DDLS
!
! TX
!
    b(1,4) = (1.d0-k)/ (2.d0)
    b(1,11) = (1.d0+k)/ (2.d0)
!
! UY'
!
    b(2,2) = (-1.d0)/ (xl)
    b(2,9) = (1.d0)/ (xl)
!
! UZ'
!
    b(3,3) = (-1.d0)/ (xl)
    b(3,10) = (1.d0)/ (xl)
!
! TX'
!
    b(4,7) = (-1.d0)/ (xl)
    b(4,14) = (1.d0)/ (xl)
!
!
9999  continue
end subroutine
