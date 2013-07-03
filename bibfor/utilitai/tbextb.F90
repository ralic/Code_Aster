subroutine tbextb(tabin, basout, tabout, npacri, lipacr,&
                  lcrpa, vi, vr, vc, vk,&
                  lprec, lcrit, iret)
! aslint: disable=W1501
    implicit   none
#include "jeveux.h"
!
#include "asterc/ismaem.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: npacri, vi(*), iret
    real(kind=8) :: vr(*), lprec(*)
    complex(kind=8) :: vc(*)
    character(len=*) :: tabin, basout, tabout, lipacr(*), lcrpa(*), vk(*)
    character(len=*) :: lcrit(*)
! ----------------------------------------------------------------------
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
!     FILTRAGE ET EXTRACTION D'UNE NOUVELLE TABLE.
! ----------------------------------------------------------------------
! IN  : TABIN  : NOM DE LA TABLE DONT ON VEUT EXTRAIRE DES LIGNES
! IN  : BASOUT : BASE DE CREATION DE "TABOUT"
! IN  : TABOUT : NOM DE LA TABLE QUI CONTIENDRA LES LIGNES EXTRAITES
! IN  : NPACRI : NOMBRE DE PARAMETRES IMPLIQUES DANS LES CRITERES
! IN  : LIPACR : LISTE DES PARAMETRES CRITERES
! IN  : LCRPA  : LISTE DES CRITERES DE COMPARAISON
! IN  : VI     : LISTE DES CRITERES POUR LES PARAMETRES "I"
! IN  : VR     : LISTE DES CRITERES POUR LES PARAMETRES "R"
! IN  : VC     : LISTE DES CRITERES POUR LES PARAMETRES "C"
! IN  : VK     : LISTE DES CRITERES POUR LES PARAMETRES "K"
! IN  : LPREC  : PRECISION POUR LES PARAMETRES "R"
! IN  : LCRIT  : CRITERE POUR LES PARAMETRES "R"
! OUT : IRET   : =  0 , OK
!                = 10 , LE PARAMETRE N'EXISTE PAS
!                = 20 , PAS DE LIGNES POUR LE PARAMETRE DONNE
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: irt, nbpara, nblign, jtbnp, nbpu, jnumi
    integer :: jtblp, i, j, k, n, jvale, itrouv, jtype, itrou2
    integer :: ki, kr, kc, kk, jvall, jparr, nbp
    integer :: jvali, jvalr, jvalc, jvalk, imax, imin
    real(kind=8) :: prec, refr, rmax, rmin
    complex(kind=8) :: cmin, cmax
    character(len=1) :: base
    character(len=4) :: type, crit
    character(len=8) :: rela
    character(len=19) :: nomtab
    character(len=24) :: nomjv, nomjvl, inpar, jnpar
    logical :: lok
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nomtab = tabin
    base = basout(1:1)
    iret = 0
!
!     --- VERIFICATION DE LA BASE ---
!
    call assert(base.eq.'V' .or. base.eq.'G')
!
!     --- VERIFICATION DE LA TABLE ---
!
    call jeexin(nomtab//'.TBBA', irt)
    if (irt .eq. 0) then
        call u2mess('F', 'UTILITAI4_64')
    endif
!
    call jeveuo(nomtab//'.TBNP', 'E', jtbnp)
    nbpara = zi(jtbnp )
    nblign = zi(jtbnp+1)
    if (nbpara .eq. 0) then
        call u2mess('F', 'UTILITAI4_65')
    endif
    if (nblign .eq. 0) then
        call u2mess('F', 'UTILITAI4_66')
    endif
!
!     --- VERIFICATION QUE LES PARAMETRES EXISTENT DANS LA TABLE ---
!
    call jeveuo(nomtab//'.TBLP', 'L', jtblp)
    do 10 i = 1, npacri
        inpar = lipacr(i)
        do 12 j = 1, nbpara
            jnpar = zk24(jtblp+4*(j-1))
            if (inpar .eq. jnpar) goto 10
12      continue
        iret = 10
        goto 9999
10  end do
!
    nbpu = nblign
    call wkvect('&&TBEXTB.NUMERO', 'V V I', nbpu, jnumi)
    do 18 i = 1, nbpu
        zi(jnumi+i-1) = i
18  end do
!
    ki = 0
    kr = 0
    kc = 0
    kk = 0
    do 20 i = 1, npacri
        itrouv = 0
        inpar = lipacr(i)
        rela = lcrpa(i)
        do 22 j = 1, nbpara
            jnpar = zk24(jtblp+4*(j-1))
            if (inpar .eq. jnpar) then
                type = zk24(jtblp+4*(j-1)+1)
                nomjv = zk24(jtblp+4*(j-1)+2)
                nomjvl = zk24(jtblp+4*(j-1)+3)
                call jeveuo(nomjv, 'L', jvale)
                call jeveuo(nomjvl, 'L', jvall)
                if (rela .eq. 'VIDE') then
                    do 50 k = 1, nbpu
                        n = zi(jnumi+k-1)
                        zi(jnumi+k-1) = 0
                        if (zi(jvall+n-1) .eq. 0) then
                            itrouv = itrouv + 1
                            zi(jnumi+itrouv-1) = n
                        endif
50                  continue
                    goto 24
                else if (rela .eq. 'NON_VIDE') then
                    do 51 k = 1, nbpu
                        n = zi(jnumi+k-1)
                        zi(jnumi+k-1) = 0
                        if (zi(jvall+n-1) .eq. 1) then
                            itrouv = itrouv + 1
                            zi(jnumi+itrouv-1) = n
                        endif
51                  continue
                    goto 24
                else if (rela .eq. 'MAXI') then
                    itrou2 = 0
                    if (type(1:1) .eq. 'I') then
                        imax = -ismaem()
                        do 52 k = 1, nbpu
                            n = zi(jnumi+k-1)
                            zi(jnumi+k-1) = 0
                            if (zi(jvall+n-1) .eq. 0) goto 52
                            if (zi(jvale+n-1) .gt. imax) then
                                imax = zi(jvale+n-1)
                                itrou2 = n
                            endif
52                      continue
                    else if (type(1:1) .eq. 'R') then
                        rmax = -r8maem()
                        do 53 k = 1, nbpu
                            n = zi(jnumi+k-1)
                            zi(jnumi+k-1) = 0
                            if (zi(jvall+n-1) .eq. 0) goto 53
                            if (zr(jvale+n-1) .gt. rmax) then
                                rmax = zr(jvale+n-1)
                                itrou2 = n
                            endif
53                      continue
                    endif
                    if (itrou2 .ne. 0) then
                        itrouv = itrouv + 1
                        zi(jnumi+itrouv-1) = itrou2
                    endif
                    goto 24
                else if (rela .eq. 'MAXI_ABS') then
                    itrou2 = 0
                    if (type(1:1) .eq. 'I') then
                        imax = -ismaem()
                        do 54 k = 1, nbpu
                            n = zi(jnumi+k-1)
                            zi(jnumi+k-1) = 0
                            if (zi(jvall+n-1) .eq. 0) goto 54
                            if (abs(zi(jvale+n-1)) .gt. imax) then
                                imax = abs(zi(jvale+n-1))
                                itrou2 = n
                            endif
54                      continue
                    else if (type(1:1) .eq. 'R') then
                        rmax = -r8maem()
                        do 55 k = 1, nbpu
                            n = zi(jnumi+k-1)
                            zi(jnumi+k-1) = 0
                            if (zi(jvall+n-1) .eq. 0) goto 55
                            if (abs(zr(jvale+n-1)) .gt. rmax) then
                                rmax = abs(zr(jvale+n-1))
                                itrou2 = n
                            endif
55                      continue
                    else if (type(1:1) .eq. 'C') then
                        cmax = dcmplx( -1.d-50 , -1.d-50 )
                        do 60 k = 1, nbpu
                            n = zi(jnumi+k-1)
                            zi(jnumi+k-1) = 0
                            if (zi(jvall+n-1) .eq. 0) goto 60
                            if (abs(zc(jvale+n-1)) .gt. abs(cmax)) then
                                cmax = zc(jvale+n-1)
                                itrou2 = n
                            endif
60                      continue
                    endif
                    if (itrou2 .ne. 0) then
                        itrouv = itrouv + 1
                        zi(jnumi+itrouv-1) = itrou2
                    endif
                    goto 24
                else if (rela .eq. 'MINI') then
                    itrou2 = 0
                    if (type(1:1) .eq. 'I') then
                        imin = ismaem()
                        do 56 k = 1, nbpu
                            n = zi(jnumi+k-1)
                            zi(jnumi+k-1) = 0
                            if (zi(jvall+n-1) .eq. 0) goto 56
                            if (zi(jvale+n-1) .lt. imin) then
                                imin = zi(jvale+n-1)
                                itrou2 = n
                            endif
56                      continue
                    else if (type(1:1) .eq. 'R') then
                        rmin = r8maem()
                        do 57 k = 1, nbpu
                            n = zi(jnumi+k-1)
                            zi(jnumi+k-1) = 0
                            if (zi(jvall+n-1) .eq. 0) goto 57
                            if (zr(jvale+n-1) .lt. rmin) then
                                rmin = zr(jvale+n-1)
                                itrou2 = n
                            endif
57                      continue
                    endif
                    if (itrou2 .ne. 0) then
                        itrouv = itrouv + 1
                        zi(jnumi+itrouv-1) = itrou2
                    endif
                    goto 24
                else if (rela .eq. 'MINI_ABS') then
                    itrou2 = 0
                    if (type(1:1) .eq. 'I') then
                        imin = ismaem()
                        do 58 k = 1, nbpu
                            n = zi(jnumi+k-1)
                            zi(jnumi+k-1) = 0
                            if (zi(jvall+n-1) .eq. 0) goto 58
                            if (abs(zi(jvale+n-1)) .lt. imin) then
                                imin = abs(zi(jvale+n-1))
                                itrou2 = n
                            endif
58                      continue
                    else if (type(1:1) .eq. 'R') then
                        rmin = r8maem()
                        do 59 k = 1, nbpu
                            n = zi(jnumi+k-1)
                            zi(jnumi+k-1) = 0
                            if (zi(jvall+n-1) .eq. 0) goto 59
                            if (abs(zr(jvale+n-1)) .lt. rmin) then
                                rmin = abs(zr(jvale+n-1))
                                itrou2 = n
                            endif
59                      continue
                    else if (type(1:1) .eq. 'C') then
                        cmin = dcmplx ( +1.d+50 , +1.d+50 )
                        do 61 k = 1, nbpu
                            n = zi(jnumi+k-1)
                            zi(jnumi+k-1) = 0
                            if (zi(jvall+n-1) .eq. 0) goto 61
                            if (abs(zc(jvale+n-1)) .lt. abs(cmin)) then
                                cmin = zc(jvale+n-1)
                                itrou2 = n
                            endif
61                      continue
                    endif
                    if (itrou2 .ne. 0) then
                        itrouv = itrouv + 1
                        zi(jnumi+itrouv-1) = itrou2
                    endif
                    goto 24
                endif
                if (type(1:1) .eq. 'I') then
                    ki = ki + 1
                    do 30 k = 1, nbpu
                        n = zi(jnumi+k-1)
                        zi(jnumi+k-1) = 0
                        if (zi(jvall+n-1) .eq. 0) goto 30
                        if (rela .eq. 'EQ') then
                            if (zi(jvale+n-1) .eq. vi(ki)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'LT') then
                            if (zi(jvale+n-1) .lt. vi(ki)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'GT') then
                            if (zi(jvale+n-1) .gt. vi(ki)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'NE') then
                            if (zi(jvale+n-1) .ne. vi(ki)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'LE') then
                            if (zi(jvale+n-1) .le. vi(ki)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'GE') then
                            if (zi(jvale+n-1) .ge. vi(ki)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        endif
30                  continue
                    goto 24
                else if (type(1:1) .eq. 'R') then
                    kr = kr + 1
                    prec = lprec(kr)
                    crit = lcrit(kr)
                    do 31 k = 1, nbpu
                        n = zi(jnumi+k-1)
                        zi(jnumi+k-1) = 0
                        if (zi(jvall+n-1) .eq. 0) goto 31
                        if (rela .eq. 'EQ') then
                            refr = zr(jvale+n-1)
                            if (crit .eq. 'RELA') then
                                lok = (abs(vr(kr)-refr) .le. prec*abs( refr))
                            else if (crit .eq. 'EGAL') then
                                lok = ( vr(kr) .eq. refr )
                            else
                                lok = ( abs(vr(kr) - refr) .le. prec )
                            endif
                            if (lok) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'LT') then
                            if (zr(jvale+n-1) .lt. vr(kr)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'GT') then
                            if (zr(jvale+n-1) .gt. vr(kr)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'NE') then
                            if (zr(jvale+n-1) .ne. vr(kr)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'LE') then
                            if (zr(jvale+n-1) .le. vr(kr)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'GE') then
                            if (zr(jvale+n-1) .ge. vr(kr)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        endif
31                  continue
                    goto 24
                else if (type(1:1) .eq. 'C') then
                    kc = kc + 1
                    do 32 k = 1, nbpu
                        n = zi(jnumi+k-1)
                        zi(jnumi+k-1) = 0
                        if (zi(jvall+n-1) .eq. 0) goto 32
                        if (rela .eq. 'EQ') then
                            if (zc(jvale+n-1) .eq. vc(kc)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'NE') then
                            if (zc(jvale+n-1) .ne. vc(kc)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        endif
32                  continue
                    goto 24
                else if (type(1:3) .eq. 'K80') then
                    kk = kk + 1
                    do 33 k = 1, nbpu
                        n = zi(jnumi+k-1)
                        zi(jnumi+k-1) = 0
                        if (zi(jvall+n-1) .eq. 0) goto 33
                        if (rela .eq. 'EQ') then
                            if (zk80(jvale+n-1) .eq. vk(kk)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'NE') then
                            if (zk80(jvale+n-1) .ne. vk(kk)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        endif
33                  continue
                    goto 24
                else if (type(1:3) .eq. 'K32') then
                    kk = kk + 1
                    do 34 k = 1, nbpu
                        n = zi(jnumi+k-1)
                        zi(jnumi+k-1) = 0
                        if (zi(jvall+n-1) .eq. 0) goto 34
                        if (rela .eq. 'EQ') then
                            if (zk32(jvale+n-1) .eq. vk(kk)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'NE') then
                            if (zk32(jvale+n-1) .ne. vk(kk)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        endif
34                  continue
                    goto 24
                else if (type(1:3) .eq. 'K24') then
                    kk = kk + 1
                    do 35 k = 1, nbpu
                        n = zi(jnumi+k-1)
                        zi(jnumi+k-1) = 0
                        if (zi(jvall+n-1) .eq. 0) goto 35
                        if (rela .eq. 'EQ') then
                            if (zk24(jvale+n-1) .eq. vk(kk)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'NE') then
                            if (zk24(jvale+n-1) .ne. vk(kk)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        endif
35                  continue
                    goto 24
                else if (type(1:3) .eq. 'K16') then
                    kk = kk + 1
                    do 36 k = 1, nbpu
                        n = zi(jnumi+k-1)
                        zi(jnumi+k-1) = 0
                        if (zi(jvall+n-1) .eq. 0) goto 36
                        if (rela .eq. 'EQ') then
                            if (zk16(jvale+n-1) .eq. vk(kk)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'NE') then
                            if (zk16(jvale+n-1) .ne. vk(kk)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        endif
36                  continue
                    goto 24
                else if (type(1:2) .eq. 'K8') then
                    kk = kk + 1
                    do 37 k = 1, nbpu
                        n = zi(jnumi+k-1)
                        zi(jnumi+k-1) = 0
                        if (zi(jvall+n-1) .eq. 0) goto 37
                        if (rela .eq. 'EQ') then
                            if (zk8(jvale+n-1) .eq. vk(kk)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        else if (rela .eq. 'NE') then
                            if (zk8(jvale+n-1) .ne. vk(kk)) then
                                itrouv = itrouv + 1
                                zi(jnumi+itrouv-1) = n
                            endif
                        endif
37                  continue
                    goto 24
                endif
            endif
22      continue
24      continue
        if (itrouv .eq. 0) then
            iret = 20
            goto 9999
        endif
        nbpu = itrouv
20  end do
!
!     --- ON RECUPERE LES PARAMETRES ET LEURS TYPES  ---
!
    call wkvect('&&TBEXTB.TYPE_R', 'V V K8', nbpara, jtype)
    call wkvect('&&TBEXTB.PARA_R', 'V V K24', nbpara, jparr)
    do 38 i = 1, nbpara
        zk24(jparr+i-1) = zk24(jtblp+4*(i-1))
        zk8 (jtype+i-1) = zk24(jtblp+4*(i-1)+1)
38  end do
!
!     --- CREATION DE LA TABLE ---
!
    call tbcrsd(tabout, basout)
    call tbajpa(tabout, nbpara, zk24(jparr), zk8(jtype))
    call wkvect('&&TBEXTB.VALE_I', 'V V I', nbpara, jvali)
    call wkvect('&&TBEXTB.VALE_R', 'V V R', nbpara, jvalr)
    call wkvect('&&TBEXTB.VALE_C', 'V V C', nbpara, jvalc)
    call wkvect('&&TBEXTB.VALE_K', 'V V K80', nbpara, jvalk)
    do 40 k = 1, nbpu
        ki = 0
        kr = 0
        kc = 0
        kk = 0
        n = zi(jnumi+k-1)
        nbp = 0
        do 42 j = 1, nbpara
            jnpar = zk24(jtblp+4*(j-1) )
            type = zk24(jtblp+4*(j-1)+1)
            nomjv = zk24(jtblp+4*(j-1)+2)
            nomjvl = zk24(jtblp+4*(j-1)+3)
            call jeveuo(nomjv, 'L', jvale)
            call jeveuo(nomjvl, 'L', jvall)
            if (zi(jvall+n-1) .eq. 0) goto 42
            nbp = nbp + 1
            zk24(jparr+nbp-1) = jnpar
            if (type(1:1) .eq. 'I') then
                ki = ki + 1
                zi(jvali+ki-1) = zi(jvale+n-1)
            else if (type(1:1) .eq. 'R') then
                kr = kr + 1
                zr(jvalr+kr-1) = zr(jvale+n-1)
            else if (type(1:1) .eq. 'C') then
                kc = kc + 1
                zc(jvalc+kc-1) = zc(jvale+n-1)
            else if (type(1:3) .eq. 'K80') then
                kk = kk + 1
                zk80(jvalk+kk-1) = zk80(jvale+n-1)
            else if (type(1:3) .eq. 'K32') then
                kk = kk + 1
                zk80(jvalk+kk-1) = zk32(jvale+n-1)
            else if (type(1:3) .eq. 'K24') then
                kk = kk + 1
                zk80(jvalk+kk-1) = zk24(jvale+n-1)
            else if (type(1:3) .eq. 'K16') then
                kk = kk + 1
                zk80(jvalk+kk-1) = zk16(jvale+n-1)
            else if (type(1:2) .eq. 'K8') then
                kk = kk + 1
                zk80(jvalk+kk-1) = zk8(jvale+n-1)
            endif
42      continue
        if (nbp .eq. 0) goto 40
        call tbajli(tabout, nbp, zk24(jparr), zi(jvali), zr(jvalr),&
                    zc(jvalc), zk80(jvalk), 0)
40  end do
!
9999  continue
    call jedetr('&&TBEXTB.NUMERO')
    call jedetr('&&TBEXTB.TYPE_R')
    call jedetr('&&TBEXTB.PARA_R')
    call jedetr('&&TBEXTB.VALE_I')
    call jedetr('&&TBEXTB.VALE_R')
    call jedetr('&&TBEXTB.VALE_C')
    call jedetr('&&TBEXTB.VALE_K')
!
    call jedema()
end subroutine
