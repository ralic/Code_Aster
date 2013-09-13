subroutine betjpl(mod, nmat, mater, sig, vin,&
                  elgeom, dsde)
    implicit none
!       ================================================================
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
!       BETON_DOUBLE_DP: LOI ELASTO PLASTIQUE AVEC DOUBLE CRITERE DE
!       PLASTICITE AVEC UN SEUIL EN COMPRESSION ET UN SEUIL EN TRACTION
!       MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT ELASTO_PLASTIQUE
!       EN VITESSE A T OU T+DT
!       ----------------------------------------------------------------
!       IN  MOD    :  TYPE DE MODELISATION
!           NMAT   :  DIMENSION MATER
!           TEMP   :  TEMPERATURE
!           MATER  :  COEFFICIENTS MATERIAU
!           SIG    :  CONTRAINTES
!           VIN    :  VARIABLES INTERNES
!           ELGEOM :  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
!                     AUX LOIS DE COMPORTEMENT
!       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
!       ----------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/betfpp.h"
#include "asterfort/lcdevi.h"
#include "asterfort/lcdima.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcprsv.h"
#include "asterfort/lcprte.h"
#include "asterfort/lcsoma.h"
#include "asterfort/lcsove.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
    integer :: nmat, nseuil
    real(kind=8) :: un, zero, rac2, deux, trois
    parameter       ( deux = 2.d0   )
    parameter       ( trois = 3.d0   )
    parameter       ( un   =  1.d0   )
    parameter       ( zero =  0.d0   )
!
    real(kind=8) :: vin(*), sig(6)
    real(kind=8) :: hook(6, 6), dsde(6, 6), vtmp(6)
!
    real(kind=8) :: mater(nmat, 2), elgeom(*)
!
    character(len=8) :: mod
    real(kind=8) :: trav1(6), trav2(6), pi0(6), dev(6)
    real(kind=8) :: sigeq, p, matr1(6, 6)
    real(kind=8) :: fc, ft, beta, kuc, kut, ke
    real(kind=8) :: a, b, c, d
    real(kind=8) :: pc, pt, dfcdlc, dftdlt, dfcds(6), dftds(6)
    real(kind=8) :: coef1, coef2, hdfcds(6), hdftds(6)
    real(kind=8) :: cc, ccc, tt, ttt, ct, tc, discr
    integer :: iadzi, iazk24
    character(len=8) :: nomail
!       ----------------------------------------------------------------
    data  pi0       /un     , un    , un    , zero , zero , zero/
!       ----------------------------------------------------------------
!
!
! --- INITIALISATION
!
    kuc = 0
    kut = 0
    rac2 = sqrt (deux)
    beta = mater(3,2)
!
    a = rac2 * (beta - un) / (deux * beta - un)
    b = rac2 / trois * beta / (deux * beta - un)
    c = rac2
    d = deux * rac2 / trois
!
    call lcopli('ISOTROPE', mod, mater(1, 1), hook)
!
    pc = vin(1)
    pt = vin(2)
    nseuil = int (vin(4) + 0.5d0)
!
! --- CONTRAINTE EQUIVALENTE
!
    call lcdevi(sig, dev)
    call lcprsc(dev, dev, p)
    sigeq = sqrt (1.5d0 * p)
    if (sigeq .eq. zero) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call utmess('A', 'ALGORITH_48', sk=nomail)
        sigeq = 1.d0
    endif
!
! --  CALCUL DES ECROUISSAGES ET DERIVES DES COURBES D'ADOUCISSEMENT
!
    call betfpp(mater, nmat, elgeom, pc, pt,&
                nseuil, fc, ft, dfcdlc, dftdlt,&
                kuc, kut, ke)
!
! --- DERIVEES DU CRITERE EN COMPRESSION
!
    if (nseuil .eq. 1 .or. nseuil .eq. 3) then
!
        coef1 = un / (rac2 * b * sigeq)
        coef2 = a / (trois * b)
        call lcprsv(coef1, dev, trav1)
        call lcprsv(coef2, pi0, trav2)
        call lcsove(trav1, trav2, dfcds)
    endif
!
! --- DERIVEES DU CRITERE EN TRACTION
!
    if (nseuil .eq. 2 .or. nseuil .eq. 3) then
!
        coef1 = un / (rac2 * d * sigeq)
        coef2 = c / (trois * d)
        call lcprsv(coef1, dev, trav1)
        call lcprsv(coef2, pi0, trav2)
        call lcsove(trav1, trav2, dftds)
    endif
!
! --- DERIVEES DU CRITERE EN TRACTION AVEC PROJECTION AU SOMMET DES
!     CONES DE TRACTION ET DE COMPRESSION
!
    if (nseuil .eq. 11 .or. nseuil .eq. 33) then
        coef2 = a / (trois * b)
        call lcprsv(coef2, pi0, dfcds)
    endif
    if (nseuil .eq. 22 .or. nseuil .eq. 33) then
        coef2 = c / (trois * d)
        call lcprsv(coef2, pi0, dftds)
    endif
!
! --- MATRICE DE COMPORTEMENT TANGENT
!
    if (nseuil .eq. 3 .or. nseuil .eq. 33) then
        call lcprmv(hook, dfcds, hdfcds)
        call lcprmv(hook, dftds, hdftds)
        call lcprsc(dfcds, hdfcds, cc)
        call lcprsc(dftds, hdfcds, tc)
        call lcprsc(dfcds, hdftds, ct)
        call lcprsc(dftds, hdftds, tt)
        ccc = cc + dfcdlc
        ttt = tt + dftdlt
        discr = -un / (ccc*ttt - ct*tc)
        call lcprsv((discr*ttt), hdfcds, vtmp)
        call lcprte(hdfcds, vtmp, dsde)
        call lcprsv((discr*ccc), hdftds, vtmp)
        call lcprte(hdftds, vtmp, matr1)
        call lcsoma(matr1, dsde, dsde)
        call lcprsv(discr*ct, hdftds, vtmp)
        call lcprte(hdfcds, vtmp, matr1)
        call lcdima(dsde, matr1, dsde)
        call lcprsv(discr*tc, hdfcds, vtmp)
        call lcprte(hdftds, vtmp, matr1)
        call lcdima(dsde, matr1, dsde)
        call lcsoma(hook, dsde, dsde)
    endif
!
    if (nseuil .eq. 2 .or. nseuil .eq. 22) then
        call lcprmv(hook, dftds, hdftds)
        call lcprsc(dftds, hdftds, tt)
        ttt = tt + dftdlt
        discr = -un / ttt
        call lcprsv(discr, hdftds, vtmp)
        call lcprte(hdftds, vtmp, dsde)
        call lcsoma(hook, dsde, dsde)
    endif
!
    if (nseuil .eq. 1 .or. nseuil .eq. 11) then
        call lcprmv(hook, dfcds, hdfcds)
        call lcprsc(dfcds, hdfcds, cc)
        ccc = cc + dfcdlc
        discr = -un / ccc
        call lcprsv(discr, hdfcds, vtmp)
        call lcprte(hdfcds, vtmp, dsde)
        call lcsoma(hook, dsde, dsde)
    endif
!
end subroutine
