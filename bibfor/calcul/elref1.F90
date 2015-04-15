subroutine elref1(elrefe)
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
!
!
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
use module_calcul, only : ca_iactif_, ca_jnbelr_, ca_jnoelr_, ca_nute_
implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
    character(len=8) :: elrefe
! ---------------------------------------------------------------------
! BUT: RECUPERER L'ELREFE D'UN TYPE_ELEM DANS UNE ROUTINE TE00IJ
! ---------------------------------------------------------------------
!     ARGUMENTS:
! ELREFE OUT  K8   :
!   - NOM DU ELREFE "PRINCIPAL" DU TYPE_ELEM
!     ASSOCIE A L'ELEMENT FINI QUE L'ON TRAITE DANS LA ROUTINE TE00IJ
!   - SI LE TYPE_ELEM N'A PAS D'ELREFE :  ELREFE='XXXXXXXX'
!----------------------------------------------------------------------
!
!
!
    ASSERT(ca_iactif_.eq.1)
    if (zi(ca_jnbelr_-1+2* (ca_nute_-1)+2) .eq. 0) then
        elrefe = 'XXXXXXXX'
    else
        elrefe = zk8(ca_jnoelr_-1+zi(ca_jnbelr_-1+2* (ca_nute_-1)+2))
    endif
    ASSERT(elrefe.ne.' ')
end subroutine
