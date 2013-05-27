subroutine matrot(angl, pgl)
    implicit none
    real(kind=8) :: angl(*), pgl(3, 3)
!       ----------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!       ----------------------------------------------------------------
!       CALCUL DE LA MATRICE ROTATION A PARTIR DES ANGLES NAUTIQUES
!
!       LES ANGLES NAUTIQUES SONT DEFINIS COMME ETANT LES ROTATIONS
!       QU IL FAUT EFFECTUER AUTOUR DE ZO , Y1 , X POUR PASSER DU
!       REPERE INITIAL (X0,Y0,Z0) AU REPERE FINAL (X,Y,Z) :
!       (X0,Y0,Z0)     >    (X1,Y1,Z0)    >    (X,Y1,Z2)    >    (X,Y,Z)
!                    APLHA              BETA              GAMMA
!
!       IN      ANGL(1) = ROTATION SENS DIRECT AUTOUR DE ZO
!               ANGL(2) = ROTATION SENS ANTI-DIRECT AUTOUR DE Y1
!               ANGL(3) = ROTATION SENS DIRECT AUTOUR DE X
!
!       OUT     PGL   = MATRICE PASSAGE REPERE GLOBAL > FINAL
!       ----------------------------------------------------------------
!
!-----------------------------------------------------------------------
    real(kind=8) :: cosa, cosb, cosg, sina, sinb, sing
!-----------------------------------------------------------------------
    cosa = cos( angl(1) )
    sina = sin( angl(1) )
    cosb = cos( angl(2) )
    sinb = sin( angl(2) )
    cosg = cos( angl(3) )
    sing = sin( angl(3) )
!
    pgl(1,1) = cosb*cosa
    pgl(2,1) = sing*sinb*cosa - cosg*sina
    pgl(3,1) = sing*sina + cosg*sinb*cosa
    pgl(1,2) = cosb*sina
    pgl(2,2) = cosg*cosa + sing*sinb*sina
    pgl(3,2) = cosg*sinb*sina - cosa*sing
    pgl(1,3) = -sinb
    pgl(2,3) = sing*cosb
    pgl(3,3) = cosg*cosb
!
end subroutine
