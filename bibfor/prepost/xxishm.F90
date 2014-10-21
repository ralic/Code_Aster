function xxishm(mailc, mailx, mo)
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
! person_in_charge: daniele.colombo at ifpen.fr
!
    implicit none
#include "asterf_types.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "jeveux.h" 
    aster_logical :: xxishm
!
! BUT : POST_CHAM_XFEM : LE RESULTAT A POST-TRAITER EST-IL HM?
!
! OUT XXISHM : VRAI SI LE RESULTAT A POST-TRAITER EST UN RESU HM
!              FAUX SINON
!
!-----------------------------------------------------------------------
    character(len=8) :: k8b, mo
    character(len=16) :: notype, notyp2
    character(len=24) :: mailc, mailx
    integer :: nbmac1, nbmac2
    integer :: jmac, jmail, ima, i, itypel
    integer :: jmax
    aster_logical :: pre1
!-----------------------------------------------------------------------
!
    call jemarq()
!
!     SI IL N'Y A PAS DE MAILLES CLASSIQUES DANS LE MODELE ON PASSE
!     DIRECTEMENT AUX MAILLES XFEM POUR SAVOIR SI ON EST EN HM
    if (mailc .eq. '&&OP0196.MAILC') goto 1
!
    call jeveuo(mailc, 'L', jmac)
    call jelira(mailc, 'LONMAX', nbmac1, k8b)
    call jeveuo(mo//'.MAILLE', 'L', jmail)
!
    do i = 1, nbmac1
!
        ima = zi(jmac-1+i)
!
        itypel = zi(jmail-1+ima)
        call jenuno(jexnum('&CATA.TE.NOMTE', itypel), notype)
    end do
!
  1 continue
!
    call jeveuo(mailx, 'L', jmax)
    call jelira(mailx, 'LONMAX', nbmac2, k8b)
    call jeveuo(mo//'.MAILLE', 'L', jmail)
!
    do i = 1, nbmac2
!
        ima = zi(jmax-1+i)
!
        itypel = zi(jmail-1+ima)
        call jenuno(jexnum('&CATA.TE.NOMTE', itypel), notyp2)
    end do
!
!
    if ((notype(1:2).eq.'HM') .or. (notyp2(1:2).eq.'HM')) then
        pre1=.true.
    else
        pre1=.false.
    endif
!
    xxishm=pre1
!
    call jedema()
end function
