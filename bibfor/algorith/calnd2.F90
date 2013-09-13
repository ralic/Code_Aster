subroutine calnd2(ic, np1, np2, np3, nbm,&
                  typch, nbseg, alpha, beta, gamma,&
                  orig, rc, theta, phii, depg,&
                  vitgc, ddist2)
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
! DESCRIPTION : CALCUL DU DENOMINATEUR DE L'EXPRESSION DE L'INCREMENT
! -----------   TEMPOREL
!
!               APPELANT : NEWTON
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterfort/disbut.h"
#include "asterfort/gloloc.h"
#include "asterfort/projmg.h"
#include "asterfort/utmess.h"
    integer :: ic, np1, np2, np3, nbm, typch(*), nbseg(*)
    real(kind=8) :: alpha(2, *), beta(2, *), gamma(2, *), orig(6, *), rc(np3, *)
    real(kind=8) :: theta(np3, *), phii(np2, np1, *), depg(*), vitgc(*), ddist2
!
! VARIABLES LOCALES
! -----------------
    integer :: i, nbs, typobs
    real(kind=8) :: xglo(3), xxglo(3), xloc(3), xtglo(3), xtloc(3), xorig(3)
    real(kind=8) :: sina, cosa, sinb, cosb, sing, cosg, xjeu, ri, cost, sint
    real(kind=8) :: dnorm, ty, tz, temp2, temp3
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  SQRT
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL   DISBUT, GLOLOC, PROJMG
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    typobs = typch(ic)
!
! 1.  PASSAGE DDLS PHYSIQUES
!     ----------------------
    call projmg(np1, np2, ic, nbm, phii,&
                depg, xglo)
!
! 2.  PASSAGE REPERE GLOBAL -> LOCAL
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
! 3.  CALCUL DU DENOMINATEUR DE L'EXPRESSION DE L'INCREMENT TEMPOREL
!     --------------------------------------------------------------
! 3.0 OBSTACLE PARALLELE A YLOCAL
!     ---------------------------
    if (typobs .eq. 0) then
!
        temp2 = 0.0d0
        do 10 i = 1, nbm
            xtglo(1) = phii(ic,i,1)
            xtglo(2) = phii(ic,i,2)
            xtglo(3) = phii(ic,i,3)
            call gloloc(xtglo, xorig, sina, cosa, sinb,&
                        cosb, sing, cosg, xtloc)
            temp2 = temp2 + xtloc(2) * vitgc(i)
10      continue
        ddist2 = -2.0d0 * xloc(2) * temp2
!
! 3.1 OBSTACLE PARALLELE A ZLOCAL
!     ---------------------------
    else if (typobs.eq.1) then
!
        temp3 = 0.0d0
        do 11 i = 1, nbm
            xtglo(1) = phii(ic,i,1)
            xtglo(2) = phii(ic,i,2)
            xtglo(3) = phii(ic,i,3)
            call gloloc(xtglo, xorig, sina, cosa, sinb,&
                        cosb, sing, cosg, xtloc)
            temp3 = temp3 + xtloc(3) * vitgc(i)
11      continue
        ddist2 = -2.0d0 * xloc(3) * temp3
!
! 3.2 OBSTACLE CIRCULAIRE
!     -------------------
    else if (typobs.eq.2) then
!
        temp2 = 0.0d0
        temp3 = 0.0d0
        do 12 i = 1, nbm
            xtglo(1) = phii(ic,i,1)
            xtglo(2) = phii(ic,i,2)
            xtglo(3) = phii(ic,i,3)
            call gloloc(xtglo, xorig, sina, cosa, sinb,&
                        cosb, sing, cosg, xtloc)
            temp2 = temp2 + xtloc(2) * vitgc(i)
            temp3 = temp3 + xtloc(3) * vitgc(i)
12      continue
        ddist2 = -2.0d0 * ( xloc(2) * temp2 + xloc(3) * temp3 )
!
! 3.3 OBSTACLE DISCRETISE
!     -------------------
    else if (typobs.eq.3) then
!
! 3.3.1  CALCUL DU SIN ET DU COS DE L'ANGLE DE LA NORMALE
!        A L'OBSTACLE ET DE LA DISTANCE NORMALE DE CHOC
!
        nbs = nbseg(ic)
        ri = sqrt(xloc(2)*xloc(2) + xloc(3)*xloc(3))
        call disbut(np3, ic, xloc, typobs, xjeu,&
                    rc, theta, nbs, cost, sint,&
                    dnorm)
!
! 3.3.2  CALCUL DES DERIVEES PARTIELLES DE LA FONCTION TEST
!        PAR RAPPORT A XLOC(2) ET XLOC(3)
!
        ty = 2.0d0 * ( cost*(dnorm+ri) + dnorm*xloc(2)/ri )
        tz = 2.0d0 * ( sint*(dnorm+ri) + dnorm*xloc(3)/ri )
!
! 3.3.3  CALCUL DU DENOMINATEUR DE L'EXPRESSION DE L'INCREMENT TEMPOREL
!
        temp2 = 0.0d0
        temp3 = 0.0d0
        do 13 i = 1, nbm
            xtglo(1) = phii(ic,i,1)
            xtglo(2) = phii(ic,i,2)
            xtglo(3) = phii(ic,i,3)
            call gloloc(xtglo, xorig, sina, cosa, sinb,&
                        cosb, sing, cosg, xtloc)
            temp2 = temp2 + xtloc(2) * vitgc(i)
            temp3 = temp3 + xtloc(3) * vitgc(i)
13      continue
        ddist2 = ty * temp2 + tz * temp3
!
! 4.  SORTIE EN ERREUR FATALE SI TRAITEMENT NON PREVU POUR LE TYPE
!     D'OBSTACLE DEMANDE
!     ------------------
    else
        call utmess('F', 'ALGORITH_72')
!
    endif
!
! --- FIN DE CALND2.
end subroutine
