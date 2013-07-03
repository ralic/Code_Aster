subroutine ftest2(np3, rc, theta, xloc, ic,&
                  typch, nbseg, dist2)
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
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DE LA FONCTION TEST CHOC SUR BUTEE IC (CARRE)
! -----------
!               APPELANT : CALND1
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterfort/disbut.h"
#include "asterfort/u2mess.h"
    integer :: np3
    real(kind=8) :: rc(np3, *), theta(np3, *), xloc(*)
    integer :: ic, typch(*), nbseg(*)
    real(kind=8) :: dist2
!
! VARIABLES LOCALES
! -----------------
    integer :: typobs, nbs
    real(kind=8) :: xjeu, xlg, cost, sint, ri, dnorm
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  SQRT
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL   DISBUT
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    typobs = typch(ic)
!
!  0. OBSTACLE PLAN PARALLELE A YLOCAL
!     --------------------------------
    if (typobs .eq. 0) then
!
        xjeu = rc(1,ic)
        dist2 = xjeu*xjeu - xloc(2)*xloc(2)
!
!  1. OBSTACLE PLAN PARALLELE A ZLOCAL
!     --------------------------------
    else if (typobs.eq.1) then
!
        xjeu = rc(1,ic)
        dist2 = xjeu*xjeu - xloc(3)*xloc(3)
!
!  2. OBSTACLE CIRCULAIRE
!     -------------------
    else if (typobs.eq.2) then
!
        xjeu = rc(1,ic)
        xlg = xloc(2)*xloc(2) + xloc(3)*xloc(3)
        dist2 = xjeu*xjeu - xlg
!
!  3. OBSTACLE DISCRETISE
!     -------------------
    else if (typobs.eq.3) then
!
!....... CALCUL DU SIN ET DU COS DE L'ANGLE DE LA NORMALE A L'OBSTACLE
!....... ET DE LA DISTANCE NORMALE DE CHOC
!
        nbs = nbseg(ic)
        call disbut(np3, ic, xloc, typobs, xjeu,&
                    rc, theta, nbs, cost, sint,&
                    dnorm)
!
        ri = sqrt( xloc(2)*xloc(2) + xloc(3)*xloc(3) )
        dist2 = dnorm * (dnorm + 2.0d0*ri)
!
!  4. SORTIE EN ERREUR FATALE SI TRAITEMENT NON PREVU POUR LE TYPE
!     D'OBSTACLE DEMANDE
!     ------------------
    else
!
        call u2mess('F', 'ALGORITH_72')
!
    endif
!
! --- FIN DE FTEST2.
end subroutine
