subroutine lcconv(loi, yd, dy, ddy, ye,&
                  nr, itmax, toler, iter, intg,&
                  nmat, mater, r, rini, epstr,&
                  typess, essai, icomp, nvi, vind,&
                  vinf, vind1, indi, bnews, mtrac,&
                  lreli, iret)
! aslint: disable=W1504
    implicit none
!       ================================================================
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
!     ----------------------------------------------------------------
!     ROUTINE D AIGUILLAGE
!     ----------------------------------------------------------------
!     CONTROLE DE LA CONVERGENCE DE LA METHODE DE NEWTON (LCPLNL):
!
!                     - CONTROLE DU NOMBRE D ITERATIONS
!                     - CONTROLE DE LA PRECISION DE CONVERGENCE
!                     - CONTROLE DE LA VALIDITE SOLUTION A CONVERGENCE
!                     - CONTROLE DES RE-INTEGRATIONS EVENTUELLES
!                     - CONTROLE DU REDECOUPAGE DU PAS DE TEMPS
!
!     ----------------------------------------------------------------
!     IN  LOI    :  MODELE DE COMPORTEMENT
!         TYPESS :  TYPE DE SOLUTION D ESSAI POUR DY(DEPEND DU MODELE)
!                    > VOIR XXXCVG ET XXXINI
!         ESSAI  :  VALEUR SOLUTION D ESSAI
!         ITMAX  :  NB MAXI D ITERATIONS LOCALES
!         TOLER  :  TOLERANCE A CONVERGENCE
!         ITER   :  NUMERO ITERATION COURANTE
!         INTG   :  NUMERO INTEGRATION COURANTE
!         NR     :  DIMENSION DY DDY
!         DY     :  VECTEUR SOLUTION = ( DSIG DVIN (DEPS3) )
!         DDY    :  VECTEUR CORRECTION SUR LA SOLUTION
!         ICOMP  :  COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS
!         NVI    :  NOMBRE DE VARIABLES INTERNES
!         VINF   :  VARIABLES INTERNES A L'INSTANT T+DT
!
!         VIND1  :  VARIABLES INTERNES A T (SAUV. INITIALE - HUJEUX)
!         INDI   :  MECANISMES POTENTIEL ACTIFS ( HUJEUX)
!         BNEWS  :  INDICATEUR DES MECANISMES DE TRACTION (HUJEUX)
!         MTRAC  :  INDICATEUR LIE A LA TRACTION(HUJEUX - BIS)
!         YE     :  VALEURS DES INCONNUES APRES LCINIT
!         LRELI  :  TYPE DE SCHEMA D'INTEGRATION
!
!     OUT IRET = 0:  CONVERGENCE
!         IRET = 1:  ITERATION SUIVANTE
!         IRET = 2:  RE-INTEGRATION
!         IRET = 3:  REDECOUPAGE DU PAS DE TEMPS
!         (VINF) UNIQUEMENT POUR LETK  - ETAT PLASTIQUE DESACTIVE?
!     ----------------------------------------------------------------
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/burcvg.h"
#include "asterfort/cvmcvg.h"
#include "asterfort/hujcvg.h"
#include "asterfort/irrcvg.h"
#include "asterfort/lccong.h"
#include "asterfort/lcmmcv.h"
#include "asterfort/lkicvg.h"
    integer :: typess, itmax, iter, intg, nr, icomp
    integer :: iret, nmat, nvi, indi(7)
    real(kind=8) :: toler, essai, ddy(*), dy(*), r(*), rini(*), yd(*)
    real(kind=8) :: mater(nmat, 2), epstr(6), vinf(nvi), vind1(nvi)
    real(kind=8) :: ye(nr), vind(nvi)
    character(len=16) :: loi
    aster_logical :: bnews(3), mtrac, lreli
!     ----------------------------------------------------------------
!
    if (loi(1:9) .eq. 'VISCOCHAB') then
!
        call cvmcvg(dy, ddy, nr, itmax, toler,&
                    iter, intg, typess, essai, icomp,&
                    iret)
!
    else if (loi(1:8) .eq. 'MONOCRIS') then
!
        call lcmmcv(yd, dy, ddy, nr, itmax,&
                    toler, iter, r, rini, epstr,&
                    iret)
!
    else if (loi(1:7) .eq. 'IRRAD3M') then
!
        call irrcvg(dy, ddy, nr, nmat, mater,&
                    itmax, toler, iter, r, rini,&
                    iret)
!
    else if (loi(1:15) .eq. 'BETON_BURGER_FP') then
!
        call burcvg(nr, itmax, toler, iter, dy,&
                    r, rini, iret)
!
    else if (loi(1:4) .eq. 'LETK') then
!
        call lkicvg(nr, itmax, toler, iter, r,&
                    nvi, vinf, dy, iret)
!
    else if (loi(1:6) .eq. 'HUJEUX') then
!
        call hujcvg(nmat, mater, nvi, vind, vinf,&
                    vind1, nr, yd, dy, r,&
                    indi, iter, itmax, intg, toler,&
                    bnews, mtrac, ye, lreli, iret)
!
    else
!
        call lccong(nr, itmax, toler, iter, r,&
                    rini, yd, dy, iret)
!
    endif
!
    ASSERT(iret.ge.0)
    ASSERT(iret.le.3)
!
end subroutine
