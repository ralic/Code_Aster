subroutine calnd1(ic, np1, np2, np3, nbm,&
                  alpha, beta, gamma, orig, rc,&
                  theta, typch, nbseg, phii, depg,&
                  dist2)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DE LA FONCTION TEST NUMERATEUR DE L'EXPRESSION
! -----------   DE L'INCREMENT TEMPOREL
!
!               APPELANT : NEWTON
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterfort/ftest2.h'
    include 'asterfort/gloloc.h'
    include 'asterfort/projmg.h'
    integer :: ic, np1, np2, np3, nbm
    real(kind=8) :: alpha(2, *), beta(2, *), gamma(2, *), orig(6, *), rc(np3, *)
    real(kind=8) :: theta(np3, *)
    integer :: typch(*), nbseg(*)
    real(kind=8) :: phii(np2, np1, *), depg(*), dist2
!
! VARIABLES LOCALES
! -----------------
    real(kind=8) :: xloc(3), xglo(3), xxglo(3)
    real(kind=8) :: xorig(3), sina, cosa, sinb, cosb, sing, cosg
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL  FTEST2, GLOLOC, PROJMG
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!  1. CONVERSION DDLS GENERALISES -> DDLS PHYSIQUES
!     ---------------------------------------------
    call projmg(np1, np2, ic, nbm, phii,&
                depg, xglo)
!
!  2. PASSAGE REPERE GLOBAL -> LOCAL
!     ------------------------------
    xorig(1) = orig(1,ic)
    xorig(2) = orig(2,ic)
    xorig(3) = orig(3,ic)
    xxglo(1) = xglo(1) + orig(4,ic)
    xxglo(2) = xglo(2) + orig(5,ic)
    xxglo(3) = xglo(3) + orig(6,ic)
    sina = alpha(1,ic)
    cosa = alpha(2,ic)
    sinb = beta(1,ic)
    cosb = beta(2,ic)
    sing = gamma(1,ic)
    cosg = gamma(2,ic)
    call gloloc(xxglo, xorig, sina, cosa, sinb,&
                cosb, sing, cosg, xloc)
!
!  3. CALCUL DE LA FONCTION TEST
!     --------------------------
    call ftest2(np3, rc, theta, xloc, ic,&
                typch, nbseg, dist2)
!
! --- FIN DE CALND1.
end subroutine
