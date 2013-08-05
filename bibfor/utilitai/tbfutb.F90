subroutine tbfutb(tabout, basout, ntab, ltabin, para,&
                  typpar, vi, vr, vc, vk)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: ntab, vi(*)
    real(kind=8) :: vr(*)
    complex(kind=8) :: vc(*)
    character(len=*) :: tabout, basout, ltabin(*), para, typpar, vk(*)
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
!     FUSIONNER PLUSIEURS TABLES EN UNE SEULE TABLE.
! ----------------------------------------------------------------------
! IN  : TABOUT : NOM DE LA TABLE QUE L'ON VEUT OBTENIR
! IN  : BASOUT : BASE DE CREATION DE "TABOUT"
! IN  : NTAB   : NOMBRE DE TABLES QUE L'ON VEUT FUSIONNER
! IN  : LTABIN : NOMS DES TABLES QUE L'ON VEUT FUSIONNER
! IN  : PARA   : NOUVEAU PARAMETRE NECESSAIRE
! IN  : TYPPAR : TYPE DU NOUVEAU PARAMETRE
! IN  : VI     : LISTE DES CRITERES POUR LES PARAMETRES "I"
! IN  : VR     : LISTE DES CRITERES POUR LES PARAMETRES "R"
! IN  : VC     : LISTE DES CRITERES POUR LES PARAMETRES "C"
! IN  : VK     : LISTE DES CRITERES POUR LES PARAMETRES "K"
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: iret, nbpara, nblign, jtbnp, nbpu, nbpart, ipar
    integer :: jtblp, i, j, k, jvale, jtype
    integer :: ki, kr, kc, kk, jvall, jparr
    integer :: jvali, jvalr, jvalc, jvalk
    character(len=1) :: base
    character(len=4) :: type, ktype
    character(len=19) :: nomtab
    character(len=24) :: nomjv, nomjvl, inpar, jnpar, knpar
    character(len=24) :: valk(3)
! ----------------------------------------------------------------------
!
    call jemarq()
!
    base = basout(1:1)
!
!     --- VERIFICATION DE LA BASE ---
!
    ASSERT(base.eq.'V' .or. base.eq.'G')
!
!     --- VERIFICATION DES TABLES ---
!
    inpar = para
    nbpart = 0
    nbpu = 0
    do 10 i = 1, ntab
        nomtab = ltabin(i)
        call jeexin(nomtab//'.TBBA', iret)
        if (iret .eq. 0) then
            call u2mess('F', 'UTILITAI4_64')
        endif
!
        call jeveuo(nomtab//'.TBNP', 'L', jtbnp)
        nbpara = zi(jtbnp )
        nblign = zi(jtbnp+1)
        nbpart = nbpart + nbpara
        nbpu = max ( nbpu , nblign )
        if (nbpara .eq. 0) then
            call u2mess('F', 'UTILITAI4_65')
        endif
        if (nblign .eq. 0) then
            call u2mess('F', 'UTILITAI4_66')
        endif
!
        call jeveuo(nomtab//'.TBLP', 'L', jtblp)
        do 12 j = 1, nbpara
            jnpar = zk24(jtblp+4*(j-1))
            if (inpar .eq. jnpar) then
                valk (1) = jnpar
                valk (2) = nomtab
                call u2mesg('F', 'UTILITAI8_20', 2, valk, 0,&
                            0, 0, 0.d0)
            endif
12      continue
!
10  end do
!
!     --- ON ELIMINE LES PARAMETRES DOUBLONS ---
!
    nbpart = nbpart + 1
    call wkvect('&&TBFUTB.TYPE_R', 'V V K8', nbpart, jtype)
    call wkvect('&&TBFUTB.PARA_R', 'V V K24', nbpart, jparr)
    ipar = 1
    if (para(1:1) .ne. ' ') then
        zk24(jparr) = para
        zk8(jtype) = typpar
    else
        zk24(jparr) = zk24(jtblp)
        zk8(jtype) = zk24(jtblp+1)
    endif
    do 20 i = 1, ntab
        nomtab = ltabin(i)
        call jeveuo(nomtab//'.TBNP', 'L', jtbnp)
        call jeveuo(nomtab//'.TBLP', 'L', jtblp)
        nbpara = zi(jtbnp )
        do 22 j = 1, nbpara
            jnpar = zk24(jtblp+4*(j-1))
            type = zk24(jtblp+4*(j-1)+1)
            do 24 k = 1, ipar
                knpar = zk24(jparr+k-1)
                ktype = zk8(jtype+k-1)
                if (knpar .eq. jnpar) then
                    if (type .ne. ktype) then
                        valk (1) = jnpar
                        valk (2) = jnpar
                        valk (3) = knpar
                        call u2mesg('F', 'UTILITAI8_21', 3, valk, 0,&
                                    0, 0, 0.d0)
                    endif
                    goto 22
                endif
24          continue
            ipar = ipar + 1
            zk24(jparr+ipar-1) = jnpar
            zk8(jtype+ipar-1) = type
22      continue
20  end do
    nbpart = ipar
!
!     --- CREATION DE LA TABLE ---
!
    call tbcrsd(tabout, basout)
    call tbajpa(tabout, nbpart, zk24(jparr), zk8(jtype))
    call wkvect('&&TBFUTB.VALE_I', 'V V I', nbpu, jvali)
    call wkvect('&&TBFUTB.VALE_R', 'V V R', nbpu, jvalr)
    call wkvect('&&TBFUTB.VALE_C', 'V V C', nbpu, jvalc)
    call wkvect('&&TBFUTB.VALE_K', 'V V K80', nbpu, jvalk)
    do 30 i = 1, ntab
        nomtab = ltabin(i)
        call jeveuo(nomtab//'.TBNP', 'L', jtbnp)
        call jeveuo(nomtab//'.TBLP', 'L', jtblp)
        nbpara = zi(jtbnp )
        nblign = zi(jtbnp+1)
        do 40 k = 1, nblign
            ki = 0
            kr = 0
            kc = 0
            kk = 0
            if (para .ne. ' ') then
                ipar = 1
                zk24(jparr) = para
            else
                ipar = 0
            endif
            if (typpar(1:1) .eq. 'I') then
                ki = ki + 1
                zi(jvali+ki-1) = vi(i)
            else if (typpar(1:1) .eq. 'R') then
                kr = kr + 1
                zr(jvalr+kr-1) = vr(i)
            else if (typpar(1:1) .eq. 'C') then
                kc = kc + 1
                zc(jvalc+kc-1) = vc(i)
            else if (typpar(1:3) .eq. 'K80') then
                kk = kk + 1
                zk80(jvalk+kk-1) = vk(i)
            else if (typpar(1:3) .eq. 'K32') then
                kk = kk + 1
                zk80(jvalk+kk-1) = vk(i)
            else if (typpar(1:3) .eq. 'K24') then
                kk = kk + 1
                zk80(jvalk+kk-1) = vk(i)
            else if (typpar(1:3) .eq. 'K16') then
                kk = kk + 1
                zk80(jvalk+kk-1) = vk(i)
            else if (typpar(1:2) .eq. 'K8') then
                kk = kk + 1
                zk80(jvalk+kk-1) = vk(i)
            endif
            do 42 j = 1, nbpara
                jnpar = zk24(jtblp+4*(j-1))
                type = zk24(jtblp+4*(j-1)+1)
                nomjv = zk24(jtblp+4*(j-1)+2)
                nomjvl = zk24(jtblp+4*(j-1)+3)
                call jeveuo(nomjv, 'L', jvale)
                call jeveuo(nomjvl, 'L', jvall)
                if (zi(jvall+k-1) .eq. 0) goto 42
                ipar = ipar + 1
                zk24(jparr+ipar-1) = jnpar
                if (type(1:1) .eq. 'I') then
                    ki = ki + 1
                    zi(jvali+ki-1) = zi(jvale+k-1)
                else if (type(1:1) .eq. 'R') then
                    kr = kr + 1
                    zr(jvalr+kr-1) = zr(jvale+k-1)
                else if (type(1:1) .eq. 'C') then
                    kc = kc + 1
                    zc(jvalc+kc-1) = zc(jvale+k-1)
                else if (type(1:3) .eq. 'K80') then
                    kk = kk + 1
                    zk80(jvalk+kk-1) = zk80(jvale+k-1)
                else if (type(1:3) .eq. 'K32') then
                    kk = kk + 1
                    zk80(jvalk+kk-1) = zk32(jvale+k-1)
                else if (type(1:3) .eq. 'K24') then
                    kk = kk + 1
                    zk80(jvalk+kk-1) = zk24(jvale+k-1)
                else if (type(1:3) .eq. 'K16') then
                    kk = kk + 1
                    zk80(jvalk+kk-1) = zk16(jvale+k-1)
                else if (type(1:2) .eq. 'K8') then
                    kk = kk + 1
                    zk80(jvalk+kk-1) = zk8(jvale+k-1)
                endif
42          continue
            call tbajli(tabout, ipar, zk24(jparr), zi(jvali), zr(jvalr),&
                        zc(jvalc), zk80(jvalk), 0)
40      continue
30  end do
!
!
!
    call jedetr('&&TBFUTB.TYPE_R')
    call jedetr('&&TBFUTB.PARA_R')
    call jedetr('&&TBFUTB.VALE_I')
    call jedetr('&&TBFUTB.VALE_R')
    call jedetr('&&TBFUTB.VALE_C')
    call jedetr('&&TBFUTB.VALE_K')
!
    call jedema()
end subroutine
