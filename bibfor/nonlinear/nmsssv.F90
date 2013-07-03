subroutine nmsssv(modelz, mate, carele, lischa, vesstf)
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/memare.h"
#include "asterfort/ss2mme.h"
    character(len=*) :: modelz
    character(len=24) :: mate, carele
    character(len=19) :: vesstf, lischa
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - SOUS-STRUCTURATION)
!
! CALCUL DU VECTEUR CHARGEMENT SUR MACRO-ELEMENTS
!
! ----------------------------------------------------------------------
!
!
!
!
!
!
    character(len=8) :: modele
    character(len=24) :: fomul2
    integer :: iret
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    modele = modelz
    fomul2 = lischa(1:19)//'.FCSS'
!
! --- CALCUL
!
    call jeexin(fomul2, iret)
    if (iret .eq. 0) then
        call assert(.false.)
    else
        call memare('V', vesstf, modele, mate, carele(1:8),&
                    'CHAR_MECA')
        call jedetr(vesstf//'.RELC')
        call ss2mme(modele(1:8), 'SOUS_STRUC', vesstf, 'V')
    endif
!
    call jedema()
!
end subroutine
