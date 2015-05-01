function digde2(modelo)
use module_calcul, only : ca_iamloc_, ca_ilmloc_
implicit none
    integer :: digde2
!
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
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
    integer :: modelo
! ----------------------------------------------------------------------
! routine identique a digdel mais qui utilise des variables de module_calcul
! pour etre plus rapide.
!
!     entrees:
!        modelo : mode_local (son indice dans &cata.te.modeloc )
!
!     sorties:
!        digde2 : nombre de scalaires representant la grandeur pour le
!                 mode_local
!
! ----------------------------------------------------------------------
    integer :: modloc
! ----------------------------------------------------------------------
    modloc = ca_iamloc_ - 1 + zi(ca_ilmloc_-1+modelo)
    digde2 = zi(modloc-1+3)
end function
