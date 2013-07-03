subroutine cocali(lis1z, lis2z, typz)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/juveca.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    character(len=*) :: lis1z, lis2z, typz
! ---------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
!     BUT :  CONCATENATION DES LISTES DE K8 LIS1 ET LIS2
!            ON AFFECTE TOUS LES ELEMENTS DE LA LISTE LIS2
!            A LA LISTE LIS1 APRES LE DERNIER ELEMENT DE LA LISTE
!            LIS1
!            SI LIS1 N'EXISTE PAS , ON LA CREE ET ON LUI AFFECTE
!            LA LONGUEUR ET LES ELEMENTS DE LIS2
!            SI LIS2 N'EXISTE PAS ON SORT EN ERREUR FATALE
!
! IN/OUT  LIS1   K24  : NOM DE LA LISTE A ENRICHIR
! IN      LIS2   K24  : NOM DE LA LISTE QUE L'ON CONCATENE A LA
!                       LISTE LIS1
! IN      TYPE   K1   : TYPE DE LA LISTE, POUR L'INSTANT SONT PREVUES
!                        'I'   LISTE D'ENTIERS
!                        'R'   LISTE DE REELS
!                        'K8'  LISTE DE K8
!
!
!
!
    character(len=1) :: k1bid
    character(len=2) :: type
    character(len=24) :: lis1, lis2
    character(len=24) :: valk(2)
!
!-----------------------------------------------------------------------
    integer :: i, idlis1, idlis2, iret, lonli1, lonli2
!-----------------------------------------------------------------------
    call jemarq()
    lis1 = lis1z
    lis2 = lis2z
    type = typz
!
    call jeexin(lis2, iret)
    if (iret .eq. 0) then
        valk(1) = lis2
        valk(2) = lis1
        call u2mesk('F', 'MODELISA4_25', 2, valk)
    else
        call jeveuo(lis2, 'L', idlis2)
        call jelira(lis2, 'LONMAX', lonli2, k1bid)
    endif
!
    call jeexin(lis1, iret)
    if (iret .eq. 0) then
        if (lonli2 .eq. 0) then
            valk(1) = lis2
            valk(2) = lis1
            call u2mesk('F', 'MODELISA4_26', 2, valk)
        else
            if (type .eq. 'K8') then
                call wkvect(lis1, 'V V K8', lonli2, idlis1)
                lonli1 = 0
            else if (type(1:1).eq.'R') then
                call wkvect(lis1, 'V V R', lonli2, idlis1)
                lonli1 = 0
            else if (type(1:1).eq.'I') then
                call wkvect(lis1, 'V V I', lonli2, idlis1)
                lonli1 = 0
            else
                call u2mesk('F', 'MODELISA4_27', 1, type)
            endif
!
        endif
    else
        call jeveuo(lis1, 'E', idlis1)
        call jelira(lis1, 'LONMAX', lonli1, k1bid)
        call juveca(lis1, lonli1+lonli2)
        call jeveuo(lis1, 'E', idlis1)
    endif
!
    if (type .eq. 'K8') then
        do 10 i = 1, lonli2
            zk8(idlis1+lonli1+i-1) = zk8(idlis2+i-1)
10      continue
    else if (type(1:1).eq.'R') then
        do 20 i = 1, lonli2
            zr(idlis1+lonli1+i-1) = zr(idlis2+i-1)
20      continue
    else if (type(1:1).eq.'I') then
        do 30 i = 1, lonli2
            zi(idlis1+lonli1+i-1) = zi(idlis2+i-1)
30      continue
    else
        call u2mesk('F', 'MODELISA4_27', 1, type)
    endif
!
! FIN -----------------------------------------------------------------
    call jedema()
end subroutine
