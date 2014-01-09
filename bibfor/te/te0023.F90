subroutine te0023(option, nomte)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit       none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
! IN OPTION    : K16 :  OPTION DE CALCUL
!     'INI_STRX'
! IN NOMTE     : K16 : NOM DU TYPE ELEMENT
!     POUTRE
!        'MECA_POU_D_E'  'MECA_POU_D_T'  'MECA_POU_D_TG'
!        'MECA_POU_D_EM' 'MECA_POU_D_TGM'
!
! INITIALISATION DU CHAMP STRX_ELGA
!
    integer :: iorien, istrx, i, kpg, npg, ncomp
!     ------------------------------------------------------------------
    call jemarq()
!
    ASSERT(option .eq. 'INI_STRX')
!
    call jevech('PCAORIE', 'L', iorien)
    call jevech('PSTRX_R', 'E', istrx)
!
    if (nomte .eq. 'MECA_POU_D_TGM' .or. nomte.eq.'MECA_POU_D_EM') then
        if (nomte.eq.'MECA_POU_D_EM') then
            npg = 2
        else
            npg = 3
        endif
        ncomp = 18
        do 3 kpg = 1, npg
            do 2 i = 1, 15
                zr(istrx-1+ncomp*(kpg-1) +i) = 0.d0
 2          continue
            do 1 i = 1, 3
                zr(istrx-1+ncomp*(kpg-1)+15+i) = zr(iorien-1+i)
 1          continue
 3      continue
    else
        do 30 i = 1, 3
            zr(istrx-1+i) = zr(iorien-1+i)
30      continue
    endif
    call jedema()
end subroutine
