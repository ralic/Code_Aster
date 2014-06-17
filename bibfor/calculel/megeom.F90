subroutine megeom(modelz, chgeoz)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: modelz, chgeoz
    character(len=8) :: modele
! ----------------------------------------------------------------------
!
!     ON CHERCHE LE NOM DU CHAMP DE GEOMETRIE DANS 1 MODELE
!
!     ENTREES:
!        MODELZ : NOM DU MODELE
!
!     SORTIES:
!        CHGEOZ : NOM DU CHAMP DE GEOMETRIE TROUVE.
!
! ----------------------------------------------------------------------
!
!     VARIABLES LOCALES:
!     ------------------
    character(len=24) :: chgeom
    character(len=8), pointer :: lgrf(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    modele = modelz
!
    ASSERT(modele.ne.' ')
    call jeveuo(modele//'.MODELE    .LGRF', 'L', vk8=lgrf)
    chgeom = lgrf(1)//'.COORDO'
    chgeoz = chgeom
    call jedema()
end subroutine
