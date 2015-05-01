subroutine jsd1ff(ip, xl, phiy, phiz, b)
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
    implicit none
    integer :: ip
    real(kind=8) :: xl, a, k, b(7, 14)
    real(kind=8) :: phiy, phiz
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES FONCTIONS DE FORME DE DEFORMATIONS
!               GENERALISEES POUTRE 7 DDL A TROIS  POINTS DE GAUSS
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
!
!                          4   TX   -.ROTATIONS SUIVANT OX,OY,OZ
!                          5   TY    .
!                          6   TZ    .
!                          7   TX'   .PARAMETRE DE GAUCHISSEMENT
!    DEFORMATIONS        : 1   UX'   .LOGITUDINALE......................
!                          2 UY'-TZ. .CISAILLEMENT OY
!                          3 UZ'+TY. .CISAILLEMENT  OZ
!                          4   TX'   .COURBURE TORSION
!                          5   TY'   .COURBURE FLEXION OY
!                          6   TZ'   .COURBURE FLEXION OZ
!                          7   TX''  .COURBURE GAUCHISSEMENT
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
    do 2 i = 1, 7
        do 1 j = 1, 14
            b(i,j) = 0
 1      continue
 2  end do
!
!                                     TIMOCH
!   UX'
!
    b(1,1) = -0.5d0/a
    b(1,8) = 0.5d0/a
!
!   UY'- TZ
!
    dy = 1.d0/ (1.d0+phiy)
    b(2,2) = (3.d0*dy*k**2-2.d0-dy)/ (4.d0*a) - 3.d0*dy* (k**2-1.d0)/ (4.d0*a)
    b(2,6) = (3.d0*dy*k**2-2.d0*k-dy)/ (4.d0) - (3.d0*dy* (k**2-1.d0)+2.d0* (1.d0-k))/4.d0
    b(2,9) = -b(2,2)
    b(2,13) = (3.d0*dy*k**2+2.d0*k-dy)/ (4.d0) - (3.d0*dy* (k**2-1.d0)+2.d0* (1.d0+k))/4.d0
!
!   UZ' + TY
!
    dz = 1.d0/ (1.d0+phiz)
    b(3,3) = (3.d0*dz*k**2-2.d0-dz)/ (4.d0*a) - 3.d0*dz* (k**2-1.d0)/ (4.d0*a)
    b(3,5) = - (3.d0*dz*k**2-2.d0*k-dz)/ (4.d0) + (3.d0*dz* (k**2-1.d0)+2.d0* (1.d0-k))/4.d0
    b(3,10) = -b(3,3)
    b(3,12) = - (3.d0*dz*k**2+2.d0*k-dz)/ (4.d0) + (3.d0*dz* (k**2-1.d0)+2.d0* (1.d0+k))/4.d0
!
! TX'
!
    b(4,4) = (3.d0*k**2-3)/ (4.d0*a)
    b(4,7) = (3.d0*k**2-2.d0*k-1.d0)/ (4.d0)
    b(4,11) = (-3.d0*k**2+3.d0)/ (4.d0*a)
    b(4,14) = (3.d0*k**2+2.d0*k-1.d0)/ (4.d0)
!
! TY'
!
    b(5,3) = - (6.d0*dz*k)/ (4.d0*a*a)
    b(5,5) = (6.d0*dz*k-2.d0)/ (4.d0*a)
    b(5,10) = -b(5,3)
    b(5,12) = (6.d0*dz*k+2.d0)/ (4.d0*a)
!
! TZ'
!
    b(6,2) = + (6.d0*dy*k)/ (4.d0*a*a)
    b(6,6) = (6.d0*dy*k-2.d0)/ (4.d0*a)
    b(6,9) = -b(6,2)
    b(6,13) = (6.d0*dy*k+2.d0)/ (4.d0*a)
!
! TX''
!
    b(7,4) = (6.d0*k)/ (4.d0*a*a)
    b(7,7) = (6.d0*k-2.d0)/ (4.d0*a)
    b(7,11) = -b(7,4)
    b(7,14) = (6.d0*k+2.d0)/ (4.d0*a)
end subroutine
