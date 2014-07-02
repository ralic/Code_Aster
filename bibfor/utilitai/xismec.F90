function xismec()
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
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
    aster_logical :: xismec
!
! BUT : POST_CHAM_XFEM : LE RESULTAT A POST-TRAITER EST-IL MECANIQUE?
!
! OUT XISMEC : VRAI SI LE RESULTAT A POST-TRAITER EST UN RESU MECANIQUE
!              FAUX SINON (ON VERIFIE ALORS QUE CE RESU EST THERMIQUE)
!
!-----------------------------------------------------------------------
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=16) :: k16tmp
    character(len=24) :: licham
    integer :: jlicha
    aster_logical :: lmeca
!-----------------------------------------------------------------------
!
    call jemarq()
!
    licham = '&&OP0196.LICHAM'
    call jeveuo(licham, 'L', jlicha)
    k16tmp = zk16(jlicha-1+1)
!
    if (k16tmp .eq. 'DEPL') then
        lmeca=.true.
    else if (k16tmp.eq.'TEMP') then
        lmeca = .false.
    else
        ASSERT(.false.)
    endif
!
    xismec=lmeca
!
    call jedema()
end function
