subroutine w039c4(carte, ifi, form)
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cesred.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/ircame.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
    character(len=*) :: carte, form
    integer :: ifi
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
! ----------------------------------------------------------------------
!
!                  IMPRIMER UNE "CARTE" AU FORMAT MED
!
!     * FORMAT MED
!     * LA CARTE DOIT EXISTER
!     * LA CARTE NE DOIT CONTENIR QUE DES REELS
!     * TOUTES LES COMPOSANTES SONT IMPRIMEES (NON AFFECTEES ==> 0)
!
! ----------------------------------------------------------------------
!
!
    integer :: iret, nugd, n1, jnocmp
    integer :: jcesk, jcesd, jcesc, jcesv, jcesl, jdesc
    character(len=8) :: typech, tsca, nomgd, k8bid
    character(len=19) :: cart1, chels1, chels2
    character(len=64) :: nommed
! ----------------------------------------------------------------------

    call jemarq()

!   --- si ce n'est pas au format med
    if (form .ne. 'MED') goto 999

!   --- si la carte n'existe pas
    call exisd('CARTE', carte, iret)
    if (iret .eq. 0) goto 999

    cart1=carte
!   --- que des reels
    call jeveuo(cart1//'.DESC', 'L', jdesc)
    nugd = zi(jdesc)
    call jenuno(jexnum('&CATA.GD.NOMGD', nugd), nomgd)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    ASSERT(tsca.eq.'R')

! --- ON TRANSFORME LA CARTE EN UN CHAM_ELEM_S
    chels1='&&W039C4.CHELS1'
    call carces(cart1, 'ELEM', ' ', 'V', chels1,&
                'A', iret)

!     -- LE FORMAT MED REFUSE DE TRAITER PLUS DE 80 CMPS :
    call jelira(jexnum('&CATA.GD.NOMCMP', nugd), 'LONMAX', n1)
    if (n1 .gt. 80) then
!       -- ON NE GARDE QUE LES 80 PREMIERES :
        call utmess('A', 'CALCULEL4_24', sk=cart1)
        chels2='&&W039C4.CHELS2'
        call copisd('CHAM_ELEM_S', 'V', chels1, chels2)
        call jeveuo(jexnum('&CATA.GD.NOMCMP', nugd), 'L', jnocmp)
        call cesred(chels2, 0, [0], 80, zk8(jnocmp),&
                    'V', chels1)
        call detrsd('CHAM_ELEM_S', chels2)
    endif
!
!
! --- POUR AVOIR UN NOM MED PROCHE DE CELUI DE LA CARTE.
!        PAS DE '_' DEJA UTILISE PAR W039C1
    nommed = carte
    nommed(9:9) = '#'
    typech = 'ELEM'
!
! --- ON RECUPERE LES OBJETS
    call jeveuo(chels1//'.CESK', 'L', jcesk)
    call jeveuo(chels1//'.CESD', 'L', jcesd)
    call jeveuo(chels1//'.CESC', 'L', jcesc)
    call jeveuo(chels1//'.CESV', 'L', jcesv)
    call jeveuo(chels1//'.CESL', 'L', jcesl)
!
! --- ECRITURE DES CHAMPS AU FORMAT MED
    k8bid = ' '
    call ircame(ifi, nommed, chels1, typech, k8bid,&
                0, k8bid, k8bid, k8bid, 0,&
                0.0d0, 0, jcesk, jcesd, jcesc,&
                jcesv, jcesl, 0, [0], k8bid,&
                iret)
!
    ASSERT(iret.eq.0)
!
    call detrsd('CHAM_ELEM_S', chels1)
!
999 continue
    call jedema()
end subroutine
