subroutine nmpcin(matass)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=19) :: matass
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! RETOUR DU POINTEUR SUR LES DDLS ELIMINES PAR AFFE_CHAR_CINE
!
! ----------------------------------------------------------------------
!
!
! IN  MATASS : MATRICE DU PREMIER MEMBRE ASSEMBLEE
!
!
!
!
    integer :: jrefa, jccid
    aster_logical :: lvcine
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LA MATRICE DOIT EXISTER. SINON ON NE NE PEUT PAS DEVINER
! --- LES DDLS IMPOSES PAR AFFE_CHAR_CINE :
!
    call jeexin(matass(1:19)//'.REFA', jrefa)
    if (jrefa .eq. 0) then
        call utmess('F', 'ALGELINE2_88', sk=matass)
    endif
!
! --- ACCES POINTEUR
!
    call jeexin(matass(1:19)//'.CCID', jccid)
    lvcine = (jccid.gt.0)
    if (lvcine) then
        call jeveuo(matass(1:19)//'.CCID', 'L', jccid)
    else
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine
