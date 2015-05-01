subroutine chreco(chou)
    implicit none
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

!     BUT : TRANSFORMER UN CHAM_NO : COMPLEXE --> REEL

!     LE CHAMP COMPLEXE EST CONSTRUIT DE SORTE QUE:
!    - SA PARTIE REELLE CORRESPOND AUX VALEURS DU CHAMP REEL
!    - SA PARTIE IMAGINAIRE EST NULLE.
!-----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
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

    integer :: iret, jvale, nbval, jvalin, i
    character(len=3) :: tsca
    character(len=4) :: tych
    character(len=8) :: chou, chin, nomgd, partie
    character(len=24) :: vale, valin
    real(kind=8) :: x, y, c1
!----------------------------------------------------------------------

    call jemarq()

!   -- recuperation du champ complexe
    call getvid(' ', 'CHAM_GD', scal=chin, nbret=iret)

!   -- verification : chin cham_no et complexe ?
    call dismoi('TYPE_CHAMP', chin, 'CHAMP', repk=tych)
    if (tych .ne. 'NOEU')  call utmess('F', 'UTILITAI_37', sk=chin)
    call dismoi('NOM_GD', chin, 'CHAMP', repk=nomgd)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    if (tsca .ne. 'C')  call utmess('F', 'UTILITAI_35', sk=chin)

!   -- copie chin --> chou
    call copisd('CHAMP', 'G', chin, chou)


!    modifications de chou:
!    ======================

!   -- 1. ".vale"
!   -------------
    vale=chou
    vale(20:24)='.VALE'

    call jelira(vale, 'LONMAX', nbval)
    call jedetr(vale)
    call jecreo(vale, 'G V R')
    call jeecra(vale, 'LONMAX', nbval)
    call jeveuo(vale, 'E', jvale)

    valin=vale
    valin(1:19)=chin
    call jeveuo(valin, 'L', jvalin)

    call getvtx(' ', 'PARTIE', scal=partie, nbret=iret)

    c1=180.d0/r8pi()
    do i = 1, nbval
        if (partie .eq. 'REEL') then
            zr(jvale+i-1)=dble(zc(jvalin+i-1))
        else if (partie.eq.'IMAG') then
            zr(jvale+i-1)=dimag(zc(jvalin+i-1))
        else if (partie.eq.'MODULE') then
            zr(jvale+i-1)=abs(zc(jvalin+i-1))
        else if (partie.eq.'PHASE') then
            x=dble(zc(jvalin+i-1))
            y=dimag(zc(jvalin+i-1))
            zr(jvale+i-1)=atan2(y,x)*c1
        endif
    end do

!   -- 2. changement de la grandeur
!   --------------------------------
    call sdchgd(chou, 'R')


    call jedema()

end subroutine
