subroutine irpara(resu, form, ifi, nbordr, ordr,&
                  nbpa, nompar, cecr)
! aslint: disable=W1303
    implicit none
#include "jeveux.h"
#include "asterc/isnnem.h"
#include "asterc/r8vide.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/rsadpa.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nompar(*)
    character(len=*) :: cecr
    character(len=*) :: resu, form
    integer :: nbordr, ordr(*), nbpa
!     ------------------------------------------------------------------
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
!     IMPRESSION DES PARAMETRES
!     ------------------------------------------------------------------
! IN  RESU   : K8  : NOM DU CONCEPT
! IN  FORM   : K8  : FORMAT D'ECRITURE
! IN  IFI    : IS  : UNITE LOGIQUE D'ECRITURE
! IN  NBORDR : I   : NOMBRE D'ORDRE
! IN  ORDR   : I   : LISTE DES NUMEROS D'ORDRE
! IN  NBPA   : I   : NOMBRE DE PARAMETRES
! IN  NOMPAR : K16 : NOM DES PARAMETRES
! IN  CECR   : K1  : CODE D'ECRITURE DES PARAMETRES 'L' LISTE
!                                                   'T' TABLEAU
!                                                   'E' EXCEL
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: necri, necrr, iundf
    real(kind=8) :: rundf
    character(len=4) :: ctype, chfin
    character(len=8) :: form1
    character(len=104) :: toto
    character(len=2000) :: titi
!     ------------------------------------------------------------------
!
!     --- INITIALISATION ---
!-----------------------------------------------------------------------
    integer :: i, iad, iec, ieci, iecr, ifi, ik16
    integer :: ik24, ik32, ik8, ik80, iord, ipa
    integer :: lk16pa, lk24pa, lk32pa, lk80pa, lk8pa, lnipa, lnrpa
    integer :: neck16, neck24, neck32, neck8, neck80
!-----------------------------------------------------------------------
    call jemarq()
    iundf = isnnem()
    rundf = r8vide()
    toto = ' '
!
!      --- IMPRESSION DES PARAMETRES AU FORMAT RESULTAT ---
    if (nbpa .ne. 0) then
        if (form .eq. 'RESULTAT') then
!
!     --- STOCKAGE DES NOMS ET VALEURS DES PARAMETRES A IMPRIMER ---
!
            necri = 0
            necrr = 0
            neck8 = 0
            neck16 = 0
            neck24 = 0
            neck32 = 0
            neck80 = 0
            call wkvect('&&IRPARA.NOMI_PARA', 'V V K16', nbpa, lnipa)
            call wkvect('&&IRPARA.NOMR_PARA', 'V V K16', nbpa, lnrpa)
            call wkvect('&&IRPARA.NOMK8_PARA', 'V V K16', nbpa, lk8pa)
            call wkvect('&&IRPARA.NOMK16_PARA', 'V V K16', nbpa, lk16pa)
            call wkvect('&&IRPARA.NOMK24_PARA', 'V V K16', nbpa, lk24pa)
            call wkvect('&&IRPARA.NOMK32_PARA', 'V V K16', nbpa, lk32pa)
            call wkvect('&&IRPARA.NOMK80_PARA', 'V V K16', nbpa, lk80pa)
            do 120 ipa = 1, nbpa
                call rsadpa(resu, 'L', 1, nompar(ipa), ordr(1),&
                            1, sjv=iad, styp=ctype, istop=0)
                if (ctype(1:1) .eq. 'I') then
                    if (zi(iad) .ne. iundf) then
                       zk16(lnipa+necri) = nompar(ipa)
                       necri = necri + 1
                    endif
                else if (ctype(1:1).eq.'R') then
                    if (zr(iad) .ne. rundf) then
                        zk16(lnrpa+necrr) = nompar(ipa)
                        necrr = necrr + 1
                    endif
                else if (ctype(1:2).eq.'K8') then
                    zk16(lk8pa+neck8) = nompar(ipa)
                    neck8 = neck8 + 1
                else if (ctype(1:3).eq.'K16') then
                    zk16(lk16pa+neck16) = nompar(ipa)
                    neck16 = neck16 + 1
                else if (ctype(1:3).eq.'K24') then
                    zk16(lk24pa+neck24) = nompar(ipa)
                    neck24 = neck24 + 1
                else if (ctype(1:3).eq.'K32') then
                    zk16(lk32pa+neck32) = nompar(ipa)
                    neck32 = neck32 + 1
                else if (ctype(1:3).eq.'K80') then
                    zk16(lk80pa+neck80) = nompar(ipa)
                    neck80 = neck80 + 1
                else
                    call utmess('A', 'PREPOST3_6', sk=ctype)
                endif
120          continue
!
!     --- IMPRESSION DES PARAMETRES ----
!
            write(ifi,'(/)')
            if (cecr(1:1) .eq. 'L') then
!            ----------------
                write(ifi,'(1X,3A)') 'IMPRESSION DES PARAMETRES DU ',&
                'CONCEPT ',resu
!
                do 200 iord = 1, nbordr
!
                    write(ifi,'(1X,A,I4,/)') 'POUR LE NUMERO D''ORDRE ',&
     &                                ordr(iord)
                    if (necri .ne. 0) then
                        do 202 iec = 1, necri
                            call rsadpa(resu, 'L', 1, zk16(lnipa-1+iec), ordr(iord),&
                                        1, sjv=iad, styp=ctype, istop=0)
                            write(ifi,'(14X,A,I12)') zk16(lnipa-1+iec)&
                            ,zi(iad)
202                      continue
                    endif
                    if (necrr .ne. 0) then
                        do 204 iec = 1, necrr
                            call rsadpa(resu, 'L', 1, zk16(lnrpa-1+iec), ordr(iord),&
                                        1, sjv=iad, styp=ctype, istop=0)
                            write(ifi,'(14X,A,1PE12.5)') zk16(lnrpa-1+&
                            iec), zr(iad)
204                      continue
                    endif
                    if (neck8 .ne. 0) then
                        do 206 iec = 1, neck8
                            call rsadpa(resu, 'L', 1, zk16(lk8pa-1+iec), ordr(iord),&
                                        1, sjv=iad, styp=ctype, istop=0)
                            write(ifi,'(14X,A,1X,A)') zk16(lk8pa-1+&
                            iec),zk8(iad)
206                      continue
                    endif
                    if (neck16 .ne. 0) then
                        do 208 iec = 1, neck16
                            call rsadpa(resu, 'L', 1, zk16(lk16pa-1+iec), ordr(iord),&
                                        1, sjv=iad, styp=ctype, istop=0)
                            write(ifi,'(14X,A,1X,A)') zk16(lk16pa-1+&
                            iec),zk16(iad)
208                      continue
                    endif
                    if (neck24 .ne. 0) then
                        do 210 iec = 1, neck24
                            call rsadpa(resu, 'L', 1, zk16(lk24pa-1+iec), ordr(iord),&
                                        1, sjv=iad, styp=ctype, istop=0)
                            write(ifi,'(14X,A,1X,A)') zk16(lk24pa-1+&
                            iec),zk24(iad)
210                      continue
                    endif
                    if (neck32 .ne. 0) then
                        do 212 iec = 1, neck32
                            call rsadpa(resu, 'L', 1, zk16(lk32pa-1+iec), ordr(iord),&
                                        1, sjv=iad, styp=ctype, istop=0)
                            write(ifi,'(14X,A,1X,A)') zk16(lk32pa-1+&
                            iec),zk32(iad)
212                      continue
                    endif
                    if (neck80 .ne. 0) then
                        do 214 iec = 1, neck80
                            call rsadpa(resu, 'L', 1, zk16(lk80pa-1+iec), ordr(iord),&
                                        1, sjv=iad, styp=ctype, istop=0)
                            write(ifi,'(14X,A)') zk16(lk32pa-1+iec)
                            write(ifi,'(1X,A)') zk80(iad)
214                      continue
                    endif
200              continue
!
            else if (cecr(1:1).eq.'T') then
!                ----------------
                write(ifi,'(1X,A,4(1X,A),/,(13X,4(1X,A)))')&
     &          'NUMERO_ORDRE',(zk16(lnipa-1+ieci),ieci=1,necri),&
     &                         (zk16(lnrpa-1+iecr),iecr=1,necrr),&
     &                         (zk16(lk8pa-1+ik8),ik8=1,neck8),&
     &                         (zk16(lk16pa-1+ik16),ik16=1,neck16)
                write(ifi,'(14X,2(A,9X))') (zk16(lk24pa-1+ik24),ik24=&
                1,neck24)
                write(ifi,'(14X,2(A,17X))') (zk16(lk32pa-1+ik32),ik32=&
                1,neck32)
                write(ifi,'(1X,A)') (zk16(lk80pa-1+ik80),ik80=1,&
                neck80)
                do 300 iord = 1, nbordr
                    i = 1
                    write(toto(i:i+13),1000) ordr(iord)
                    i=14
                    if (necri .ne. 0) then
                        do 302 iec = 1, necri
                            call rsadpa(resu, 'L', 1, zk16(lnipa-1+iec), ordr(iord),&
                                        1, sjv=iad, styp=ctype, istop=0)
                            write(toto(i:i+16),1001) zi(iad)
                            i=i+17
                            if (i .ge. 68) then
                                write(ifi,'(A)') toto
                                toto=' '
                                i=14
                            endif
302                      continue
                    endif
                    if (necrr .ne. 0) then
                        do 304 iec = 1, necrr
                            call rsadpa(resu, 'L', 1, zk16(lnrpa-1+iec), ordr(iord),&
                                        1, sjv=iad, styp=ctype, istop=0)
                            write(toto(i:i+16),1002) zr(iad)
                            i=i+17
                            if (i .ge. 68) then
                                write(ifi,'(A)') toto
                                toto=' '
                                i=14
                            endif
304                      continue
                    endif
                    if (neck8 .ne. 0) then
                        do 306 iec = 1, neck8
                            call rsadpa(resu, 'L', 1, zk16(lk8pa-1+iec), ordr(iord),&
                                        1, sjv=iad, styp=ctype, istop=0)
                            write(toto(i:i+16),1008) zk8(iad)
                            i=i+17
                            if (i .ge. 68) then
                                write(ifi,'(A)') toto
                                toto=' '
                                i=14
                            endif
306                      continue
                    endif
                    if (neck16 .ne. 0) then
                        do 308 iec = 1, neck16
                            call rsadpa(resu, 'L', 1, zk16(lk16pa-1+iec), ordr(iord),&
                                        1, sjv=iad, styp=ctype, istop=0)
                            write(toto(i:i+16),1016) zk16(iad)
                            i=i+17
                            if (i .ge. 68) then
                                write(ifi,'(A)') toto
                                toto=' '
                                i=14
                            endif
308                      continue
                    endif
                    if (neck24 .ne. 0) then
                        if (i .ne. 14) then
                            write(ifi,'(A)') toto
                            toto=' '
                        endif
                        i=14
                        do 310 iec = 1, neck24
                            call rsadpa(resu, 'L', 1, zk16(lk24pa-1+iec), ordr(iord),&
                                        1, sjv=iad, styp=ctype, istop=0)
                            write(toto(i:i+24),1024) zk24(iad)
                            i=i+25
                            if (i .ge. 50) then
                                write(ifi,'(A)') toto
                                toto=' '
                                i=14
                            endif
310                      continue
                    endif
                    if (neck32 .ne. 0) then
                        if (i .ne. 14) then
                            write(ifi,'(A)') toto
                            toto=' '
                        endif
                        i=14
                        do 312 iec = 1, neck32
                            call rsadpa(resu, 'L', 1, zk16(lk32pa-1+iec), ordr(iord),&
                                        1, sjv=iad, styp=ctype, istop=0)
                            write(toto(i:i+32),1032) zk32(iad)
                            i=i+33
                            if (i .ge. 64) then
                                write(ifi,'(A)') toto
                                toto=' '
                                i=14
                            endif
312                      continue
                    endif
                    if (neck80 .ne. 0) then
                        if (i .ne. 14) then
                            write(ifi,'(A)') toto
                            toto=' '
                        endif
                        do 314 iec = 1, neck80
                            call rsadpa(resu, 'L', 1, zk16(lk80pa-1+iec), ordr(iord),&
                                        1, sjv=iad, styp=ctype, istop=0)
                            write(ifi,'(A)') zk80(iad)
314                      continue
                    endif
                    if (toto .ne. ' ') write(ifi,'(A)') toto
300              continue
!
            else if (cecr(1:1).eq.'E') then
!                ----------------
                titi = ' '
                titi(1:12) = 'NUMERO_ORDRE'
                i = 14
                do 402 ieci = 1, necri
                    titi(i:i+15) = zk16(lnipa-1+ieci)
                    i = i + 17
402              continue
                do 404 iecr = 1, necrr
                    titi(i:i+15) = zk16(lnrpa-1+iecr)
                    i = i + 17
404              continue
                do 406 ik8 = 1, neck8
                    titi(i:i+15) = zk16(lk8pa-1+ik8)
                    i = i + 17
406              continue
                do 408 ik16 = 1, neck16
                    titi(i:i+15) = zk16(lk16pa-1+ik16)
                    i = i + 17
408              continue
                do 410 ik24 = 1, neck24
                    titi(i:i+15) = zk16(lk24pa-1+ik24)
                    i = i + 25
410              continue
                do 412 ik32 = 1, neck32
                    titi(i:i+15) = zk16(lk32pa-1+ik32)
                    i = i + 33
412              continue
                do 414 ik80 = 1, neck80
                    titi(i:i+15) = zk16(lk80pa-1+ik80)
                    i = i + 81
414              continue
                call codent(i, 'G', chfin)
                form1 = '(A'//chfin//')'
                write(ifi,form1) titi(1:i)
!
                do 420 iord = 1, nbordr
                    titi = ' '
                    write(titi(1:12),'(I12)') ordr(iord)
                    i = 14
                    do 422 iec = 1, necri
                        call rsadpa(resu, 'L', 1, zk16(lnipa-1+iec), ordr( iord),&
                                    1, sjv=iad, styp=ctype, istop=0)
                        write(titi(i:i+15),'(I12)') zi(iad)
                        i = i + 17
422                  continue
                    do 424 iec = 1, necrr
                        call rsadpa(resu, 'L', 1, zk16(lnrpa-1+iec), ordr( iord),&
                                    1, sjv=iad, styp=ctype, istop=0)
                        write(titi(i:i+15),'(1PD12.5)') zr(iad)
                        i = i + 17
424                  continue
                    do 426 iec = 1, neck8
                        call rsadpa(resu, 'L', 1, zk16(lk8pa-1+iec), ordr( iord),&
                                    1, sjv=iad, styp=ctype, istop=0)
                        titi(i:i+15) = zk8(iad)
                        i = i + 17
426                  continue
                    do 428 iec = 1, neck16
                        call rsadpa(resu, 'L', 1, zk16(lk16pa-1+iec), ordr(iord),&
                                    1, sjv=iad, styp=ctype, istop=0)
                        titi(i:i+15) = zk16(iad)
                        i = i + 17
428                  continue
                    do 430 iec = 1, neck24
                        call rsadpa(resu, 'L', 1, zk16(lk24pa-1+iec), ordr(iord),&
                                    1, sjv=iad, styp=ctype, istop=0)
                        titi(i:i+23) = zk24(iad)
                        i = i + 25
430                  continue
                    do 432 iec = 1, neck32
                        call rsadpa(resu, 'L', 1, zk16(lk32pa-1+iec), ordr(iord),&
                                    1, sjv=iad, styp=ctype, istop=0)
                        titi(i:i+31) = zk32(iad)
                        i = i + 33
432                  continue
                    do 434 iec = 1, neck80
                        call rsadpa(resu, 'L', 1, zk16(lk80pa-1+iec), ordr(iord),&
                                    1, sjv=iad, styp=ctype, istop=0)
                        titi(i:i+79) = zk80(iad)
                        i = i + 81
434                  continue
                    call codent(i, 'G', chfin)
                    form1 = '(A'//chfin//')'
                    write(ifi,form1) titi(1:i)
420              continue
            endif
        endif
    endif
!
    call jedetr('&&IRPARA.NOMI_PARA')
    call jedetr('&&IRPARA.NOMR_PARA')
    call jedetr('&&IRPARA.NOMK8_PARA')
    call jedetr('&&IRPARA.NOMK16_PARA')
    call jedetr('&&IRPARA.NOMK24_PARA')
    call jedetr('&&IRPARA.NOMK32_PARA')
    call jedetr('&&IRPARA.NOMK80_PARA')
    1000 format(i12,1x)
    1001 format(i12,5x)
    1002 format(1pd12.5,5x)
    1008 format(a,9x)
    1016 format(a,1x)
    1024 format(1x,a)
    1032 format(1x,a)
    call jedema()
end subroutine
