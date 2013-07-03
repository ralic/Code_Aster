subroutine mditm3(chain1, chain2, chain3, chain4, chain5,&
                  chain6, chain7, chain8, chain9, ilong,&
                  np1, nbnl)
! ---------------------------------------------------------------------
! DESCRIPTION
! -----------
!     CALCUL DE LA REPONSE DYNAMIQUE NON-LINEAIRE D'UNE STRUCTURE
!                    PAR UNE METHODE INTEGRALE
!                      (VERSION MULTIMODAL)
!
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
    implicit none
!
!
!
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    integer :: ilong, np1, nbnl, i, ilongg
    integer :: kordre, ktemps, kdepg, kvitg, kaccg, kdep, kfor, kvit, kptem
    integer :: jordre, jtemps, jdepg, jvitg, jaccg, jdep, jfor, jvit, jptem
    character(len=16) :: chain1, chain2, chain3, chain4, chain5, chain6
    character(len=16) :: chain7, chain8, chain9
!
    call jemarq()
!
    ilongg = ilong
    ilong = ilong + 10000
!
    if (ilongg .ne. 0) then
        call jeveuo(chain1, 'L', jordre)
        call jeveuo(chain2, 'L', jtemps)
        call jeveuo(chain3, 'L', jdepg)
        call jeveuo(chain4, 'L', jvitg)
        call jeveuo(chain5, 'L', jaccg)
        call jeveuo(chain9, 'L', jptem)
        if (nbnl .ne. 0) then
            call jeveuo(chain6, 'L', jdep)
            call jeveuo(chain7, 'L', jfor)
            call jeveuo(chain8, 'L', jvit)
            call wkvect('&&MDITM3_6', 'V V R8', nbnl*3*ilongg, kdep)
            call wkvect('&&MDITM3_7', 'V V R8', nbnl*3*ilongg, kfor)
            call wkvect('&&MDITM3_8', 'V V R8', nbnl*3*ilongg, kvit)
        endif
        call wkvect('&&MDITM3_1', 'V V I', ilongg, kordre)
        call wkvect('&&MDITM3_2', 'V V R8', ilongg, ktemps)
        call wkvect('&&MDITM3_3', 'V V R8', np1*ilongg, kdepg)
        call wkvect('&&MDITM3_4', 'V V R8', np1*ilongg, kvitg)
        call wkvect('&&MDITM3_5', 'V V R8', np1*ilongg, kaccg)
        call wkvect('&&MDITM3_9', 'V V R8', np1*ilongg, kptem)
        do 110 i = 0, ilongg-1
            zi(kordre+i) = zi(jordre + i)
            zr(ktemps+i) = zr(jtemps + i)
            zr(kptem+i) = zr(jptem + i)
110      continue
        do 120 i = 0, np1*ilongg-1
            zr(kdepg+i) = zr(jdepg + i)
            zr(kvitg+i) = zr(jvitg + i)
            zr(kaccg+i) = zr(jaccg + i)
120      continue
        do 130 i = 0, nbnl*3*ilongg-1
            zr(kdep+i) = zr(jdep + i)
            zr(kvit+i) = zr(jvit + i)
            zr(kfor+i) = zr(jfor + i)
130      continue
        call jedetr(chain1)
        call jedetr(chain2)
        call jedetr(chain3)
        call jedetr(chain4)
        call jedetr(chain5)
        call jedetr(chain9)
        if (nbnl .ne. 0) then
            call jedetr(chain6)
            call jedetr(chain7)
            call jedetr(chain8)
        endif
    endif
!
    call wkvect(chain1, 'V V I', ilong, jordre)
    call wkvect(chain2, 'V V R8', ilong, jtemps)
    call wkvect(chain3, 'V V R8', np1*ilong, jdepg)
    call wkvect(chain4, 'V V R8', np1*ilong, jvitg)
    call wkvect(chain5, 'V V R8', np1*ilong, jaccg)
    call wkvect(chain9, 'V V R8', np1*ilong, jptem)
    if (nbnl .ne. 0) then
        call wkvect(chain6, 'V V R8', nbnl*3*ilong, jdep)
        call wkvect(chain7, 'V V R8', nbnl*3*ilong, jfor)
        call wkvect(chain8, 'V V R8', nbnl*3*ilong, jvit)
    else
        call wkvect(chain6, 'V V R8', 1, jdep)
        call wkvect(chain7, 'V V R8', 1, jfor)
        call wkvect(chain8, 'V V R8', 1, jvit)
    endif
!
    if (ilongg .ne. 0) then
        do 10 i = 0, ilongg-1
            zi(jordre+i) = zi(kordre + i)
            zr(jtemps+i) = zr(ktemps + i)
            zr(jptem+i) = zr(kptem + i)
10      continue
        do 20 i = 0, np1*ilongg-1
            zr(jdepg+i) = zr(kdepg + i)
            zr(jvitg+i) = zr(kvitg + i)
            zr(jaccg+i) = zr(kaccg + i)
20      continue
        do 30 i = 0, nbnl*3*ilongg-1
            zr(jdep+i) = zr(kdep + i)
            zr(jvit+i) = zr(kvit + i)
            zr(jfor+i) = zr(kfor + i)
30      continue
        call jedetr('&&MDITM3_1')
        call jedetr('&&MDITM3_2')
        call jedetr('&&MDITM3_3')
        call jedetr('&&MDITM3_4')
        call jedetr('&&MDITM3_5')
        call jedetr('&&MDITM3_9')
        if (nbnl .ne. 0) then
            call jedetr('&&MDITM3_6')
            call jedetr('&&MDITM3_7')
            call jedetr('&&MDITM3_8')
        endif
    endif
!
    call jedema()
end subroutine
