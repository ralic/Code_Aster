subroutine jpd1ff(ip, xl, phiy, phiz, b)
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
    implicit none
    integer :: ip
    real(kind=8) :: xl, a, k, b(6, 12)
    real(kind=8) :: phiy, phiz
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES FONCTIONS DE FORME DE DEFORMATIONS
!               GENERALISEES POUTRE 6 DDL A TROIS  POINTS DE GAUSS
!
!    - ARGUMENTS:
!        DONNEES:           IP      -->   POINTS DE GAUSS
!                           XL      -->   LONGUEUR DE L'ELEMENT
!                          PHIY     -->  COEFF DE CISAILLEMENT SUIVANT Y
!                          PHIZ     -->  COEFF DE CISAILLEMENT SUIVANT Z
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
!                          4   TX   -.ROTATIONS SUIVANT OX,OY,OZ
!                          5   TY    .
!                          6   TZ    .
!    DEFORMATIONS        : 1   UX'   .LOGITUDINALE......................
!                          2 UY'-TZ. .CISAILLEMENT OY
!                          3 UZ'+TY. .CISAILLEMENT  OZ
!                          4   TX'   .COURBURE TORSION
!                          5   TY'   .COURBURE FLEXION OY
!                          6   TZ'   .COURBURE FLEXION OZ
! ......................................................................
!
!-----------------------------------------------------------------------
    integer :: i, j
    real(kind=8) :: dy, dz
!-----------------------------------------------------------------------
    a = 0.5d0*xl
!
    if (ip .eq. 1) then
        k = -sqrt(0.6d0)
    else if (ip.eq.2) then
        k = 0.d0
    else if (ip.eq.3) then
        k = sqrt(0.6d0)
    endif
    do 2 i = 1, 6
        do 1 j = 1, 12
            b(i,j) = 0
 1      continue
 2  end do
!
!                                     TIMOCH
!   UX'
!
    b(1,1) = -0.5d0/a
    b(1,7) = 0.5d0/a
!
!   UY'- TZ
!
    dy = 1.d0/ (1.d0+phiy)
    b(2,2) = (3.d0*dy*k**2-2.d0-dy)/ (4.d0*a) - 3.d0*dy* (k**2-1.d0)/ (4.d0*a)
    b(2,6) = (3.d0*dy*k**2-2.d0*k-dy)/ (4.d0) - (3.d0*dy* (k**2-1.d0)+2.d0* (1.d0-k))/4.d0
    b(2,8) = -b(2,2)
    b(2,12) = (3.d0*dy*k**2+2.d0*k-dy)/ (4.d0) - (3.d0*dy* (k**2-1.d0)+2.d0* (1.d0+k))/4.d0
!
!   UZ' + TY
!
    dz = 1.d0/ (1.d0+phiz)
    b(3,3) = (3.d0*dz*k**2-2.d0-dz)/ (4.d0*a) - 3.d0*dz* (k**2-1.d0)/ (4.d0*a)
    b(3,5) = - (3.d0*dz*k**2-2.d0*k-dz)/ (4.d0) + (3.d0*dz* (k**2-1.d0)+2.d0* (1.d0-k))/4.d0
    b(3,9) = -b(3,3)
    b(3,11) = - (3.d0*dz*k**2+2.d0*k-dz)/ (4.d0) + (3.d0*dz* (k**2-1.d0)+2.d0* (1.d0+k))/4.d0
!
! TX'
!
    b(4,4) = -0.5d0/a
    b(4,10) = 0.5d0/a
!
! TY'
!
    b(5,3) = - (6.d0*dz*k)/ (4.d0*a*a)
    b(5,5) = (6.d0*dz*k-2.d0)/ (4.d0*a)
    b(5,9) = -b(5,3)
    b(5,11) = (6.d0*dz*k+2.d0)/ (4.d0*a)
!
! TZ'
!
    b(6,2) = + (6.d0*dy*k)/ (4.d0*a*a)
    b(6,6) = (6.d0*dy*k-2.d0)/ (4.d0*a)
    b(6,8) = -b(6,2)
    b(6,12) = (6.d0*dy*k+2.d0)/ (4.d0*a)
!
end subroutine
