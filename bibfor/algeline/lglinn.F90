subroutine lglinn(nbmat, mater, parame, derive, ge,&
                  ie, q, vecn, f0, delta,&
                  devg, devgii, traceg, dy)
!
    implicit      none
#include "asterfort/calcdy.h"
#include "asterfort/calcg.h"
#include "asterfort/drfdrg.h"
#include "asterfort/drfdrs.h"
#include "asterfort/drfnew.h"
#include "asterfort/drudrg.h"
#include "asterfort/drudrs.h"
#include "asterfort/hlode.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nbmat
    real(kind=8) :: mater(nbmat, 2), parame(5), derive(4), ge, ie
    real(kind=8) :: q(6), vecn(6), f0, delta, devg(6), devgii
    real(kind=8) :: traceg, dy(10)
! ======================================================================
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
!
!
! ======================================================================
! ======================================================================
! --- BUT : CALCUL DU PREMIER MULTIPLICATEUR PLASTIQUE -----------------
! ======================================================================
! IN  : NDT    : NOMBRE DE COMPOSANTES TOTALES DU TENSEUR --------------
! --- : NDI    : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
! --- : NR     : NOMBRE DE RELATIONS NON-LINEAIRES ---------------------
! --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
! --- : MATER  : PARAMETRES MATERIAU -----------------------------------
! --- : PARAME : VARIABLES D'ECROUISSAGES ------------------------------
! --- : DERIVE : DERIVEES DES VARIABLES D'ECROUISSAGES -----------------
! --- : GE     : DIRECTION D'ECOULEMENT ELASTIQUE ----------------------
! --- : IE     : PREMIER INVARIANT ELASTIQUE ---------------------------
! --- : Q      : DS/DE -------------------------------------------------
! --- : VECN   : VECTEUR N ---------------------------------------------
! --- : F0     : VALEUR SEUIL A L'ITERATION 0 --------------------------
! --- : DELTA  : INCREMENT DU LAMBDA -----------------------------------
! OUT : DEVG   : DEVIATEUR DU TENSEUR DE G -----------------------------
! --- : DEVGII : NORME DU DEVIATEUR DE G -------------------------------
! --- : TRACEG : TRACE DE G --------------------------------------------
! --- : DY     : INCREMENTS (SIG, I1, GAMP, EVP, DELTA) ----------------
! ======================================================================
    real(kind=8) :: mu, k, sigc, gamcjs, h0, mun
    real(kind=8) :: duds(6), dudg, dfds(6), dfdg, dfdl, g(6)
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( mun    = -1.0d0  )
! ======================================================================
    call jemarq()
! ======================================================================
! --- INITIALISATION DE DONNEES ----------------------------------------
! ======================================================================
    mu = mater( 4,1)
    k = mater( 5,1)
    sigc = mater( 9,2)
    gamcjs = mater(12,2)
! ======================================================================
! --- CALCUL DE H0, CALCUL INTERMEDIAIRE -------------------------------
! ======================================================================
    h0 = hlode(gamcjs, mun)
! ======================================================================
! --- CALCUL DES DIFFERENTES DERIVEES PRINCIPALES ----------------------
! ======================================================================
! --- CALCUL DE DUDS ---------------------------------------------------
! ======================================================================
    call drudrs(parame, q, h0, sigc, duds)
! ======================================================================
! --- CALCUL DE DUDG ---------------------------------------------------
! ======================================================================
    call drudrg(parame, derive, h0, sigc, ge,&
                ie, dudg)
! ======================================================================
! --- CALCUL DE DFDS ---------------------------------------------------
! ======================================================================
    call drfdrs(q, parame, h0, sigc, ge,&
                duds, dfds)
! ======================================================================
! --- CALCUL DE DFDG ---------------------------------------------------
! ======================================================================
    call drfdrg(parame, derive, h0, sigc, ge,&
                dudg, dfdg)
! ======================================================================
! --- CALCUL DE G ------------------------------------------------------
! ======================================================================
    call calcg(dfds, vecn, g, devg, traceg,&
               devgii)
! ======================================================================
! --- CALCUL DE DFDL ---------------------------------------------------
! ======================================================================
    call drfnew(devg, devgii, traceg, dfds, dfdg,&
                mu, k, dfdl)
! ======================================================================
! --- CALCUL DES DIFFERENTS INCREMENTS ---------------------------------
! ======================================================================
    call calcdy(mu, k, f0, devg, devgii,&
                traceg, dfdl, delta, dy)
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
