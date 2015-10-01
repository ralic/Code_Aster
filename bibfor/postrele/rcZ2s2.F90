subroutine rcZ2s2(sijm, sijth, sn)
    implicit   none
#include "asterfort/rctres.h"
    real(kind=8) :: sijm, sijth(6), sn(2)
!     ------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     CALCUL DU SN MAX SUR LES INSTANTS
!     ON GARDE LES DEUX VALEURS:  SIJM+STH ET SIJM-STH
!
! IN  : SIJM   : CONTRAINTES LINEARISEES OU EN PEAU (CHARGEMENTS MECA)
! IN  : STH    : CONTRAINTES LINEARISEES OU EN PEAU ( THERMOMECA)
! OUT : SN     : AMPLITUDE DE VARIATION DES CONTRAINTES DE TRESCA
!     ------------------------------------------------------------------
!
    real(kind=8) :: tresca
! DEB ------------------------------------------------------------------
!
    call rctres(sijth, tresca)
    sn(1) = tresca+sijm
    sn(2) = tresca+sijm
!
end subroutine
