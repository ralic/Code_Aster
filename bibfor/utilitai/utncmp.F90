subroutine utncmp(cham19, ncmp, nomobj)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dgmode.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: ncmp
    character(len=*) :: cham19, nomobj
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
!     ------------------------------------------------------------------
!
!     RECUPERE LE NOMBRE ET LES NOMS DES COMPOSANTES D'UN CHAMP
!
!     ------------------------------------------------------------------
!
    integer ::  jprno, gd, nec, tabec(10), j, ino, iec, icmp, ncmpmx
    integer :: jcmp, iad, kcmp, igr, mode, nnoe, jceld, nbgrel, irepe, nbel
    integer :: jmod, imodel, ilong, idescr, jdesc, nb
    character(len=4) :: tych
    character(len=24) :: valk(2)
    character(len=8) ::  noma
    character(len=19) :: ch19, prno, noligr
!     ------------------------------------------------------------------
    call jemarq()
!
    ch19 = cham19
    ncmp = 0
!
    call dismoi('TYPE_CHAMP', ch19, 'CHAMP', repk=tych)
    call dismoi('NOM_MAILLA', ch19, 'CHAMP', repk=noma)
    call dismoi('NUM_GD', ch19, 'CHAMP', repi=gd)
!
    call jeveuo('&CATA.GD.DESCRIGD', 'L', idescr)
    nec = nbec( gd)
    ASSERT(nec.le.10)
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iad)
    call wkvect('&&UTNCMP.ICMP', 'V V I', ncmpmx, jcmp)
!
!     ==================================================================
!                            C H A M _ N O
!     ==================================================================
    if (tych(1:4) .eq. 'NOEU') then
        call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nnoe)
        call dismoi('PROF_CHNO', ch19, 'CHAM_NO', repk=prno)
        if (prno .eq. ' ') then
!AS OU LE CHAMP EST A PROFIL CONSTANT (CHAMP DE GEOMETRIE)
            call jeveuo(ch19//'.DESC', 'L', jdesc)
            ncmp = - zi(jdesc+1)
            do 32 iec = 1, nec
                tabec(iec)= zi(jdesc+1+iec)
 32         continue
            nb = 0
            do 34 icmp = 1, ncmpmx
                if (exisdg(tabec,icmp)) then
                    do 36 j = 1, ncmp
                        if (zi(jcmp+j-1) .eq. icmp) goto 34
 36                 continue
                    nb = nb + 1
                    zi(jcmp+nb-1) = icmp
                endif
 34         continue
        else
            call jeveuo(jexnum(prno//'.PRNO', 1), 'L', jprno)
            do 10 ino = 1, nnoe
                do 12 iec = 1, nec
                    tabec(iec)= zi(jprno-1+(ino-1)*(nec+2)+2+iec )
 12             continue
                do 14 icmp = 1, ncmpmx
                    if (exisdg(tabec,icmp)) then
                        do 16 j = 1, ncmp
                            if (zi(jcmp+j-1) .eq. icmp) goto 14
 16                     continue
                        ncmp = ncmp + 1
                        zi(jcmp+ncmp-1) = icmp
                    endif
 14             continue
 10         continue
        endif
!
!     ==================================================================
!                             C H A M _ E L E M
!     ==================================================================
    else if (tych(1:2) .eq. 'EL') then
        call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbel)
        call dismoi('NOM_LIGREL', ch19, 'CHAMP', repk=noligr)
        call dismoi('NB_GREL', noligr, 'LIGREL', repi=nbgrel)
        call jeveuo(ch19//'.CELD', 'L', jceld)
        call jeveuo(noligr//'.REPE', 'L', irepe)
        call jeveuo('&CATA.TE.MODELOC', 'L', imodel)
        call jeveuo(jexatr('&CATA.TE.MODELOC', 'LONCUM'), 'L', ilong)
        do 20 igr = 1, nbgrel
            mode=zi(jceld-1+zi(jceld-1+4+igr) +2)
            if (mode .eq. 0) goto 20
            jmod = imodel+zi(ilong-1+mode)-1
            nec = nbec( zi(jmod-1+2))
            call dgmode(mode, imodel, ilong, nec, tabec)
            do 22 icmp = 1, ncmpmx
                if (exisdg( tabec , icmp )) then
                    do 24 j = 1, ncmp
                        if (zi(jcmp+j-1) .eq. icmp) goto 22
 24                 continue
                    ncmp = ncmp + 1
                    zi(jcmp+ncmp-1) = icmp
                endif
 22         continue
 20     continue
!
    else
        valk(1) = tych
        valk(2) = ch19
        call utmess('F', 'ALGORITH9_69', nk=2, valk=valk)
    endif
!
    if (ncmp .eq. 0) then
        call utmess('F', 'UTILITAI5_53')
    endif
!
    call wkvect(nomobj, 'V V K8', ncmp, kcmp)
    do 30 icmp = 1, ncmp
        zk8(kcmp+icmp-1) = zk8(iad-1+zi(jcmp+icmp-1))
 30 end do
    call jedetr('&&UTNCMP.ICMP')
!
    call jedema()
end subroutine
