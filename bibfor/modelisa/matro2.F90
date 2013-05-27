subroutine matro2(angl, gamarc, theta, pgl1, pgl2)
    implicit none
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     CALCUL DE LA MATRICE ROTATION A PARTIR DES ANGLES NAUTIQUES
!            ET DE L'ORIENTATION DE LA POUTRE COURBE
!            ET DE LA POSITION DES NOEUDS DE LA POUTRE COURBE
!
!     LES ANGLES NAUTIQUES SONT ALPHA, BETA, GAMMA
!     L'ANGLE DE LA POUTRE COURBE EST GAMARC
!     REPERE INITIAL (X0,Y0,Z0) ---> REPERE FINAL (XP,YP,ZP) :
!     (X0,Y0,Z0)     >    (X1,Y1,Z0)    >    (X,Y1,Z2)    >   (X,Y,Z)
!                  APLHA              BETA             GAMARC
!                            (X,Y,Z)    >    (X,YP,ZP)
!                                     GAMMA
!
!     IN      ALPHA  : ROTATION SENS DIRECT AUTOUR DE ZO
!             BETA   : ROTATION SENS ANTI-DIRECT AUTOUR DE Y1
!             GAMMA  : ROTATION SENS DIRECT AUTOUR DE X
!             GAMARC : ROTATION SENS DIRECT AUTOUR DE X
!             THETA  : DEMI-ANGLE AU SOMMET
!
!     OUT     PGL1   : MATRICE PASSAGE REPERE GLOBAL > FINAL  NOEUD 1
!     OUT     PGL2   : MATRICE PASSAGE REPERE GLOBAL > FINAL  NOEUD 2
!     ------------------------------------------------------------------
    include 'asterfort/matrot.h'
    include 'asterfort/pmat.h'
    real(kind=8) :: angl(*), ang1(3), pgl1(3, 3), pgl2(3, 3)
    real(kind=8) :: m21, m31, m22, m32, m23, m33
    real(kind=8) :: ro(3, 3), ni(3, 3), nf(3, 3)
!
!-----------------------------------------------------------------------
    real(kind=8) :: cosa, cosb, cosg, cosgar, gamarc, sina, sinb
    real(kind=8) :: sing, singar, theta
!-----------------------------------------------------------------------
    cosa = cos( angl(1) )
    sina = sin( angl(1) )
    cosb = cos( angl(2) )
    sinb = sin( angl(2) )
    cosg = cos( angl(3) )
    sing = sin( angl(3) )
    cosgar = cos( gamarc )
    singar = sin( gamarc )
!
    m21 = singar*sinb*cosa - cosgar*sina
    m31 = singar*sina + cosgar*sinb*cosa
    m22 = cosgar*cosa + singar*sinb*sina
    m32 = cosgar*sinb*sina - cosa*singar
    m23 = singar*cosb
    m33 = cosgar*cosb
!
    ro(1,1) = cosb*cosa
    ro(2,1) = m21*cosg + m31*sing
    ro(3,1) = -m21*sing + m31*cosg
!
    ro(1,2) = cosb *sina
    ro(2,2) = m22*cosg + m32*sing
    ro(3,2) = -m22*sing + m32*cosg
!
    ro(1,3) = -sinb
    ro(2,3) = m23*cosg + m33*sing
    ro(3,3) = -m23*sing + m33*cosg
!
!     --- MATRICE AU NOEUD FINAL ---
    ang1(1) = -theta
    ang1(2) = 0.d0
    ang1(3) = 0.d0
    call matrot(ang1, nf)
    call pmat(3, nf, ro, pgl2)
!
!     --- MATRICE AU NOEUD INITIAL ---
    ang1(1) = theta
    call matrot(ang1, ni)
    call pmat(3, ni, ro, pgl1)
!
end subroutine
