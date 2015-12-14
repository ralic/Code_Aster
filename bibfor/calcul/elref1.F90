subroutine elref1(elrefe)
use calcul_module, only : ca_iactif_, ca_jnbelr_, ca_jnoelr_, ca_nute_
implicit none
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
#include "jeveux.h"
#include "asterfort/assert.h"
    character(len=8) :: elrefe
!----------------------------------------------------------------------
! but: recuperer l'elrefe d'un type_elem dans une routine te00ij
!----------------------------------------------------------------------
! Argument:
!   elrefe out  k8   :
!     - nom du elrefe "principal" du type_elem
!       associe a l'element fini que l'on traite dans la routine te00ij
!     - si le type_elem n'a pas d'elrefe :  elrefe='XXXXXXXX'
!----------------------------------------------------------------------

    ASSERT(ca_iactif_.eq.1)
    if (zi(ca_jnbelr_-1+2* (ca_nute_-1)+2) .eq. 0) then
        elrefe = 'XXXXXXXX'
    else
        elrefe = zk8(ca_jnoelr_-1+zi(ca_jnbelr_-1+2* (ca_nute_-1)+2))
    endif
    ASSERT(elrefe.ne.' ')
end subroutine
