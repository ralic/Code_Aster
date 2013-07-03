subroutine rc36sa(nommat, mati, matj, snpq, spij,&
                  typeke, spmeca, spther, saltij, sm)
    implicit   none
#include "asterfort/prccm3.h"
    real(kind=8) :: mati(*), matj(*), snpq, spij, saltij, sm
    real(kind=8) :: typeke, spmeca, spther
    character(len=8) :: nommat
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!
!     CALCUL DE LA CONTRAINTE EQUIVALENTE ALTERNEE  SALT
!     CALCUL DU FACTEUR D'USAGE ET DE SON CUMUL
!
! IN  : NOMMAT : NOM MATERIAU
! IN  : MATI   : MATERIAU ASSOCIE A L'ETAT STABILISE I
! IN  : MATJ   : MATERIAU ASSOCIE A L'ETAT STABILISE J
! IN  : SNPQ   : AMPLITUDE DE VARIATION DES CONTRAINTES LINEARISEES
! IN  : SPIJ   : AMPLITUDE DE VARIATION DES CONTRAINTES TOTALES
! OUT : SALTIJ : AMPLITUDE DE CONTRAINTE ENTRE LES ETATS I ET J
!
!     ------------------------------------------------------------------
!
    real(kind=8) :: e, ec, para(3), m, n, ke, nadm, saltm, salth, kemeca, kether
    real(kind=8) :: kethe1
! DEB ------------------------------------------------------------------
!
! --- LE MATERIAU
!
    e = min ( mati(1) , matj(1) )
    ec = max ( mati(10) , matj(10) )
    sm = min ( mati(11) , matj(11) )
    m = max ( mati(12) , matj(12) )
    n = max ( mati(13) , matj(13) )
!
    para(1) = m
    para(2) = n
    para(3) = ec / e
!
! --- CALCUL DU COEFFICIENT DE CONCENTRATION ELASTO-PLASTIQUE KE
! --- CALCUL DE LA CONTRAINTE EQUIVALENTE ALTERNEE SALT
! --- CALCUL DU NOMBRE DE CYCLES ADMISSIBLE NADM
!
    if (typeke .lt. 0.d0) then
        call prccm3(nommat, para, sm, snpq, spij,&
                    ke, saltij, nadm)
    else
        call prccm3(nommat, para, sm, snpq, spmeca,&
                    kemeca, saltm, nadm)
!
!       CALCUL DE KE THER
!
        kethe1 = 1.86d0*(1.d0-(1.d0/(1.66d0+snpq/sm)))
        kether = max(1.d0,kethe1)
!
!        CALCUL DE SALTH
        salth= 0.5d0 * para(3) * kether * spther
!
        saltij = saltm + salth
    endif
!
end subroutine
