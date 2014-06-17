subroutine memzme(modele, matel)
! ----------------------------------------------------------------------
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
!     CALCUL DES MATRICES ELEMENTAIRES DE MASSE MECA
!
! ----------------------------------------------------------------------
! IN  : MODELE : NOM DU MODELE (OBLIGATOIRE)
! IN  : MATEL  : NOM DU MATR_ELEM RESULTAT
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
#include "asterfort/utmess.h"
    character(len=19) :: matel
    character(len=8) :: lpain(1), lpaout(1), modele
    character(len=24) :: ligrmo, lchin(1), lchout(1), option, chgeom
!
!-----------------------------------------------------------------------
    character(len=24), pointer :: rerr(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    if (modele(1:1) .eq. ' ') then
        call utmess('F', 'CALCULEL2_82')
    endif
!
    call megeom(modele, chgeom)
!
    call memare('V', matel, modele, ' ', ' ',&
                'MASS_ZZ1')
    call jeveuo(matel//'.RERR', 'E', vk24=rerr)
    rerr(3) (1:3) = 'OUI'
!
    call jedetr(matel//'.RELR')
!
    lpaout(1) = 'PMATZZR'
    lchout(1) = matel(1:8)//'.ME001'
!
    ligrmo = modele//'.MODELE'
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
!
    option = 'MASS_ZZ1'
    call calcul('S', option, ligrmo, 1, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
    call reajre(matel, lchout(1), 'V')
!
    call jedema()
end subroutine
