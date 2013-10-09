subroutine chcore(chou)
    implicit none
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
!
!     BUT : TRANSFORMER UN CHAMP : REEL --> COMPLEXE
!
!     LE CHAMP COMPLEXE EST CONSTRUIT DE SORTE QUE:
!    - SA PARTIE REELLE CORRESPOND AUX VALEURS DU CHAMP REEL
!    - SA PARTIE IMAGINAIRE EST NULLE.
!     -----------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/sdchgd.h"
#include "asterfort/utmess.h"
    integer :: iret, ibid, jvale, nbval, jvalin, i
    real(kind=8) :: zero
    parameter(zero=0.d0)
    character(len=3) :: tsca
    character(len=4) :: docu
    character(len=8) :: chou, chin, nomgd
    character(len=24) :: k24b, vale, valin
!
    call jemarq()
!
!     RECUPERATION DU CHAMP REEL
    call getvid(' ', 'CHAM_GD', scal=chin, nbret=iret)
!
!     VERIFICATION : CHIN REEL?
    call dismoi('NOM_GD', chin, 'CHAMP', repk=nomgd)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    if (tsca .ne. 'R') then
        call utmess('F', 'UTILITAI_20', sk=chin)
    endif
!
!     COPIE CHIN --> CHOU
    call copisd('CHAMP', 'G', chin, chou)
!
!
!     MODIFICATIONS DE CHOU:
!    ======================
!
! --- 1. ".VALE"
!     ----------
!     CHAM_NO OU CHAM_ELEM ?
    vale(1:19)=chou
    k24b=vale(1:19)//'.DESC'
    call jeexin(k24b, ibid)
    if (ibid .gt. 0) then
        k24b=vale(1:19)//'.DESC'
        call jelira(k24b, 'DOCU', cval=docu)
    else
        k24b=vale(1:19)//'.CELD'
        call jelira(k24b, 'DOCU', cval=docu)
    endif
!
    if (docu .eq. 'CHNO') then
        vale(20:24)='.VALE'
    else if (docu.eq.'CHML') then
        vale(20:24)='.CELV'
    else
        call utmess('F', 'UTILITAI_21')
    endif
!
    call jelira(vale, 'LONMAX', nbval)
    call jedetr(vale)
    call jecreo(vale, 'G V C')
    call jeecra(vale, 'LONMAX', nbval)
    call jeveuo(vale, 'E', jvale)
!
    valin=vale
    valin(1:19)=chin
    call jeveuo(valin, 'L', jvalin)
!
    do 10 i = 1, nbval
        zc(jvale+i-1)=dcmplx(zr(jvalin+i-1),zero)
 10 end do
!
! --- 2. CHANGEMENT DE LA GRANDEUR
!     ----------------------------
    call sdchgd(chou, 'C')
!
    call jedema()
!
end subroutine
