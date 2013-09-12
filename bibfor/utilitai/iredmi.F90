subroutine iredmi(macr)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/r8pi.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/iredm1.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rslipa.h"
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: macr
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
!     INTERFACE ASTER - MISS3D : PROCEDURE  IMPR_MACR_ELEM
!     ------------------------------------------------------------------
    integer :: vali(2)
!
    character(len=8) :: k8b, mael, basemo, masse, noma, listam
    character(len=16) :: nomcmd
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, i2, iam, icamor, icmass, icrigi, ier
    integer :: iret, isamor, ismass, isrigi, ival1, ival2, ival3
    integer :: j, j2, jamo2, jamor, jfreq, jmass, jordr
    integer :: jrefe, jrigi, k, lamor, n1, n2
    integer :: nbamor, nbmode, nbmods, nbmodt
    real(kind=8) :: petir8, pi, r8b
!-----------------------------------------------------------------------
    call jemarq()
    mael = macr
    call getres(k8b, k8b, nomcmd)
    pi = r8pi()
    petir8 = 1.d-40
!
!     ----- RECUPERATION DES MODES -----
    call jeveuo(mael//'.MAEL_REFE', 'L', jrefe)
    basemo = zk24(jrefe)(1:8)
    noma = zk24(jrefe+1)(1:8)
    call jelira(basemo//'           .ORDR', 'LONMAX', nbmodt)
    call jeveuo(basemo//'           .ORDR', 'L', jordr)
!
    call dismoi('F', 'NB_MODES_DYN', basemo, 'RESULTAT', nbmode,&
                k8b, ier)
    call dismoi('F', 'NB_MODES_STA', basemo, 'RESULTAT', nbmods,&
                k8b, ier)
    nbmodt = nbmode + nbmods
!
    call jeveuo(mael//'.MAEL_MASS_REFE', 'L', jrefe)
    masse = zk24(jrefe+1)
!
!     ----- RECUPERATION DES FREQUENCES -----
    call rslipa(basemo, 'FREQ', '&&IREDMI.LIFREQ', jfreq, nbmodt)
!
!
!     ----- EXTRACTION DU MACRO-ELEMENT DYNAMIQUE -----
!
    if (nbmode .eq. 0) then
        call wkvect('&&IREDMI.DMASS', 'V V R', 1, jmass)
        call wkvect('&&IREDMI.DRIGI', 'V V R', 1, jrigi)
        call wkvect('&&IREDMI.DAMOR', 'V V R', 1, lamor)
    else
        call wkvect('&&IREDMI.DMASS', 'V V R', nbmode*nbmode, jmass)
        call wkvect('&&IREDMI.DRIGI', 'V V R', nbmode*nbmode, jrigi)
        call wkvect('&&IREDMI.DAMOR', 'V V R', nbmode*nbmode, lamor)
    endif
    if (nbmods .eq. 0) then
        call wkvect('&&IREDMI.SMASS', 'V V R', 1, ismass)
        call wkvect('&&IREDMI.SRIGI', 'V V R', 1, isrigi)
        call wkvect('&&IREDMI.SAMOR', 'V V R', 1, isamor)
    else
        call wkvect('&&IREDMI.SMASS', 'V V R', nbmods*nbmods, ismass)
        call wkvect('&&IREDMI.SRIGI', 'V V R', nbmods*nbmods, isrigi)
        call wkvect('&&IREDMI.SAMOR', 'V V R', nbmods*nbmods, isamor)
    endif
    if (nbmode .eq. 0 .or. nbmods .eq. 0) then
        call wkvect('&&IREDMI.CMASS', 'V V R', 1, icmass)
        call wkvect('&&IREDMI.CRIGI', 'V V R', 1, icrigi)
        call wkvect('&&IREDMI.CAMOR', 'V V R', 1, icamor)
    else
        call wkvect('&&IREDMI.CMASS', 'V V R', nbmode*nbmods, icmass)
        call wkvect('&&IREDMI.CRIGI', 'V V R', nbmode*nbmods, icrigi)
        call wkvect('&&IREDMI.CAMOR', 'V V R', nbmode*nbmods, icamor)
    endif
!
    call jeveuo(mael//'.MAEL_MASS_VALE', 'L', ival1)
    call jeveuo(mael//'.MAEL_RAID_VALE', 'L', ival2)
    do 20 j = 1, nbmode
        do 21 i = 1, j
            k =j*(j-1)/2 + i
            zr(jmass+i-1+(j-1)*nbmode) = zr(ival1+k-1) + petir8
            zr(jmass+j-1+(i-1)*nbmode) = zr(ival1+k-1) + petir8
            zr(jrigi+i-1+(j-1)*nbmode) = zr(ival2+k-1) + petir8
            zr(jrigi+j-1+(i-1)*nbmode) = zr(ival2+k-1) + petir8
21      continue
20  continue
    do 22 j = nbmode+1, nbmodt
        do 23 i = 1, nbmode
            k = j*(j-1)/2 + i
            j2 = j - nbmode
            zr(icmass+j2-1+(i-1)*nbmods) = zr(ival1+k-1) + petir8
            zr(icrigi+j2-1+(i-1)*nbmods) = zr(ival2+k-1) + petir8
23      continue
        do 24 i = nbmode+1, j
            k = j*(j-1)/2 + i
            i2 = i - nbmode
            j2 = j - nbmode
            zr(ismass+i2-1+(j2-1)*nbmods) = zr(ival1+k-1) + petir8
            zr(ismass+j2-1+(i2-1)*nbmods) = zr(ival1+k-1) + petir8
            zr(isrigi+i2-1+(j2-1)*nbmods) = zr(ival2+k-1) + petir8
            zr(isrigi+j2-1+(i2-1)*nbmods) = zr(ival2+k-1) + petir8
24      continue
22  continue
!
    call jeexin(mael//'.MAEL_AMOR_VALE', iret)
    if (iret .ne. 0) then
        call jeveuo(mael//'.MAEL_AMOR_VALE', 'L', ival3)
        do 30 j = 1, nbmode
            do 31 i = 1, j
                k =j*(j-1)/2 + i
                zr(lamor+i-1+(j-1)*nbmode) = zr(ival3+k-1) + petir8
                zr(lamor+j-1+(i-1)*nbmode) = zr(ival3+k-1) + petir8
31          continue
30      continue
        do 32 j = nbmode+1, nbmodt
            do 33 i = 1, nbmode
                k = j*(j-1)/2 + i
                j2 = j - nbmode
                zr(icamor+j2-1+(i-1)*nbmods) = zr(ival3+k-1) + petir8
33          continue
            do 34 i = nbmode+1, j
                k = j*(j-1)/2 + i
                i2 = i - nbmode
                j2 = j - nbmode
                zr(isamor+i2-1+(j2-1)*nbmods) = zr(ival3+k-1) + petir8
                zr(isamor+j2-1+(i2-1)*nbmods) = zr(ival3+k-1) + petir8
34          continue
32      continue
    else
        ival3 = 0
    endif
!
!     ----- RECUPERATION DES AMORTISSEMENTS -----
    call getvr8(' ', 'AMOR_REDUIT', nbval=0, nbret=n1)
    call getvid(' ', 'LIST_AMOR', nbval=0, nbret=n2)
    if (nbmode .eq. 0) then
        call wkvect('&&IREDMI.AMORTISSEMENT', 'V V R', 1, jamor)
    else
        call wkvect('&&IREDMI.AMORTISSEMENT', 'V V R', nbmode, jamor)
    endif
    if (n1 .ne. 0 .or. n2 .ne. 0) then
        if (n1 .ne. 0) then
            nbamor = -n1
            call getvr8(' ', 'AMOR_REDUIT', nbval=nbamor, vect=zr(jamor), nbret=n1)
        else
            call getvid(' ', 'LIST_AMOR', scal=listam, nbret=n2)
            call jelira(listam//'           .VALE', 'LONMAX', nbamor)
            call jeveuo(listam//'           .VALE', 'L', jamor)
        endif
        if (nbamor .gt. nbmode) then
            vali (1) = nbamor
            vali (2) = nbmode
            call u2mesg('F', 'UTILITAI6_44', 0, ' ', 2,&
                        vali, 0, 0.d0)
        endif
        if (nbamor .lt. nbmode) then
            call wkvect('&&IREDMI.AMORTISSEMEN2', 'V V R', nbmode, jamo2)
            do 40 iam = 1, nbamor
                zr(jamo2+iam-1) = zr(jamor+iam-1)
40          continue
            do 42 iam = nbamor+1, nbmode
                zr(jamo2+iam-1) = zr(jamor+nbamor-1)
42          continue
            nbamor = nbmode
            jamor = jamo2
        endif
    else
        do 44 k = 1, nbmode
            zr(jamor+k-1) = zr(&
                            lamor+(k-1)*(nbmode+1))/ (4.d0*pi*zr( jfreq+k-1)*zr(jmass+(k-1)*(nbmo&
                            &de+1))&
                            )
44      continue
    endif
!
    call iredm1(masse, noma, basemo, nbmode, nbmods,&
                ival3, zr(jmass), zr(jrigi), zr(jamor), zr(jfreq),&
                zr(ismass), zr(isrigi), zr(isamor), zr(icmass), zr(icrigi),&
                zr(icamor))
!
!
! --- MENAGE
!
    call jedetr('&&IREDMI.LIFREQ')
    call jedetr('&&IREDMI.DMASS')
    call jedetr('&&IREDMI.DRIGI')
    call jedetr('&&IREDMI.DAMOR')
    call jedetr('&&IREDMI.SMASS')
    call jedetr('&&IREDMI.SRIGI')
    call jedetr('&&IREDMI.SAMOR')
    call jedetr('&&IREDMI.CMASS')
    call jedetr('&&IREDMI.CRIGI')
    call jedetr('&&IREDMI.CAMOR')
    call jedetr('&&IREDMI.AMORTISSEMENT')
    call jedetr('&&IREDMI.AMORTISSEMEN2')
!
    call jedema()
end subroutine
