subroutine op0004()
    implicit none
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
!     OPERATEUR DEFI_NAPPE
!     STOCKAGE DANS UN OBJET DE TYPE FONCTION
!     -----------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/foimpr.h"
#include "asterfort/foordn.h"
#include "asterfort/foston.h"
#include "asterfort/foverf.h"
#include "asterfort/fovern.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/lxlgut.h"
#include "asterfort/ordon1.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/uttrif.h"
#include "asterfort/wkvect.h"
    character(len=4) :: interp(2)
    character(len=24) :: valk
    character(len=8) :: k8b
    character(len=16) :: nomcmd, typfon, verif
    character(len=19) :: nomfon
    logical :: defonc
    integer :: iret, iret2
    integer :: vali(2)
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ifm, ifonc, iocc, ival, jval, l
    integer :: l1, ladrf, lnomf, lpar, lpar2, lpara, lparc
    integer :: lpro, lval, mxva, n, n1, n2, n3
    integer :: nbcoup, nbfonc, nbpara, nbval, niv, nprol, nv
!
    real(kind=8) :: rbid, toto
!-----------------------------------------------------------------------
    call jemarq()
!
    verif = ' '
    defonc = .false.
    call getres(nomfon, typfon, nomcmd)
    call getvtx(' ', 'VERIF', 0, iarg, 1,&
                verif, n1)
    call getvr8(' ', 'PARA', 0, iarg, 0,&
                toto, n2)
    call getvid(' ', 'FONCTION', 0, iarg, 0,&
                k8b, n3)
    nbpara = abs(n2)
    if (n3 .ne. 0) then
        nbfonc = -n3
    else
        defonc = .true.
        call getfac('DEFI_FONCTION', nbfonc)
    endif
!
    if (nbpara .ne. nbfonc) then
        vali (1) = nbpara
        vali (2) = nbfonc
        call u2mesg('F', 'UTILITAI8_3', 0, ' ', 2,&
                    vali, 0, 0.d0)
    endif
!
!     --- VERIFICATION DE LA CROISSANCE DES PARAMETRES ---
    if (verif .eq. 'CROISSANT') then
        call wkvect('&&OP0004.TEMP.PARA', 'V V R', nbpara, lparc)
        call getvr8(' ', 'PARA', 0, iarg, nbpara,&
                    zr(lparc), n)
!        VERIF QUE LES PARA SONT STRICT CROISSANTS
        iret=2
        call foverf(zr(lparc), nbpara, iret)
        if (iret .ne. 2) then
            call u2mess('F', 'UTILITAI2_72')
        endif
        call jedetr('&&OP0004.TEMP.PARA')
    endif
!
    if (defonc) then
        do 10 iocc = 1, nbfonc
            call getvr8('DEFI_FONCTION', 'VALE', iocc, iarg, 0,&
                        rbid, nv)
            nv = -nv
            if (mod(nv,2) .ne. 0) then
                vali (1) = iocc
                call u2mesg('F', 'UTILITAI8_4', 0, ' ', 1,&
                            vali, 0, 0.d0)
            endif
            if (verif .eq. 'CROISSANT') then
                nbcoup = nv / 2
                call wkvect('&&OP0004.TEMP.PARA', 'V V R', nv, lpara)
                call wkvect('&&OP0004.TEMP.PAR2', 'V V R', nbcoup, lpar2)
                call getvr8('DEFI_FONCTION', 'VALE', iocc, iarg, nv,&
                            zr(lpara), nbval)
                do 12 i = 0, nbcoup-1
                    zr(lpar2+i) = zr(lpara+2*i)
12              continue
!              VERIF QUE LES PARA SONT STRICT CROISSANTS
                iret=2
                call foverf(zr(lpar2), nbcoup, iret)
                if (iret .ne. 2) then
                    call u2mess('F', 'UTILITAI2_72')
                endif
                call jedetr('&&OP0004.TEMP.PARA')
                call jedetr('&&OP0004.TEMP.PAR2')
            endif
10      continue
    endif
!
!
! --- RECUPERATION DU NIVEAU D'IMPRESSION
    call infmaj()
    call infniv(ifm, niv)
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.PROL ---
    nprol = 7 + 2*nbfonc
    call assert(lxlgut(nomfon).le.24)
    call wkvect(nomfon//'.PROL', 'G V K24', nprol, lpro)
    zk24(lpro ) = 'NAPPE   '
    call getvtx(' ', 'INTERPOL', 0, iarg, 2,&
                interp, l1)
    if (l1 .eq. 1) interp(2) = interp(1)
    zk24(lpro+1) = interp(1)//interp(2)
    call getvtx(' ', 'NOM_PARA', 0, iarg, 1,&
                zk24(lpro+2), l)
    call getvtx(' ', 'NOM_RESU', 0, iarg, 1,&
                zk24(lpro+3), l)
    call getvtx(' ', 'PROL_GAUCHE', 0, iarg, 1,&
                zk24(lpro+4)(1:1), l)
    call getvtx(' ', 'PROL_DROITE', 0, iarg, 1,&
                zk24(lpro+4)(2:2), l)
    zk24(lpro+5) = nomfon
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.PARA ---
    call wkvect(nomfon//'.PARA', 'G V R', nbpara, lpar)
    call getvr8(' ', 'PARA', 0, iarg, nbpara,&
                zr(lpar), n)
!
    call wkvect('&&OP0004.NOM.FONCTIONS', 'V V K24', nbfonc, lnomf)
    if (defonc) then
        call getvtx(' ', 'NOM_PARA_FONC', 0, iarg, 1,&
                    zk24(lpro+6), l)
        mxva = 0
        do 20 ifonc = 1, nbfonc
            call getvr8('DEFI_FONCTION', 'VALE', ifonc, iarg, 0,&
                        rbid, nbval)
            mxva = max(mxva,-nbval)
20      continue
        call wkvect('&&OP0004.VALEURS.LUES', 'V V R', mxva, jval)
        call wkvect('&&OP0004.POINTEURS.F', 'V V I', nbfonc, ladrf)
        do 30 ifonc = 1, nbfonc
            zk24(lnomf+ifonc-1) = '&&OP0004.F'
            call codent(ifonc, 'G', zk24(lnomf+ifonc-1)(11:19))
            zk24(lnomf+ifonc-1)(20:24) = '.VALE'
            call getvr8('DEFI_FONCTION', 'VALE', ifonc, iarg, mxva,&
                        zr(jval), nbval)
            call wkvect(zk24(lnomf+ifonc-1), 'V V R', nbval, lval)
            zi(ladrf+ifonc-1) = lval
            nbcoup = nbval / 2
            do 32 ival = 1, nbcoup
                zr(lval-1+ival) = zr(jval-1+2*ival-1)
                zr(lval-1+nbcoup+ival) = zr(jval-1+2*ival)
32          continue
!
!           --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
!               ET REMISE DES ABSCISSES EN ORDRE CROISSANT
!           CE N'EST PAS LA PEINE SI LA CROISSANTE STRICTE A ETE IMPOSEE
            if (verif .ne. 'CROISSANT') then
                iret2=0
                call foverf(zr(lval), nbcoup, iret2)
                if (iret2 .eq. 0) then
                    typfon='FONCTION'
                    call uttrif(zr(lval), nbcoup, typfon)
                    valk = nomfon
                    call u2mesg('A', 'UTILITAI8_5', 1, valk, 0,&
                                0, 0, 0.d0)
                else if (iret2.lt.0) then
                    call ordon1(zr(lval), nbcoup)
                    vali (1) = ifonc
                    call u2mesg('A', 'UTILITAI8_6', 0, ' ', 1,&
                                vali, 0, 0.d0)
                endif
            endif
!
            call getvtx('DEFI_FONCTION', 'INTERPOL', ifonc, iarg, 2,&
                        interp, l1)
            if (l1 .eq. 1) interp(2) = interp(1)
            zk24(lpro+6+2*ifonc-1) = interp(1)//interp(2)
            call getvtx('DEFI_FONCTION', 'PROL_GAUCHE', ifonc, iarg, 1,&
                        zk24(lpro+6+2*ifonc)(1:1), l)
            call getvtx('DEFI_FONCTION', 'PROL_DROITE', ifonc, iarg, 1,&
                        zk24(lpro+6+2*ifonc)(2:2), l)
30      continue
    else
        call getvid(' ', 'FONCTION', 0, iarg, nbfonc,&
                    zk24(lnomf), n)
        call fovern(zk24(lnomf), nbfonc, zk24(lpro), iret)
    endif
!
!     --- ON ORDONNE LA NAPPE SUIVANT LES PARAMETRES CROISSANTS ---
    if (verif .ne. 'CROISSANT') then
        iret=0
        call foordn(zr(lpar), zk24(lnomf), nbpara, nbfonc, iret)
        if (iret .ne. 0) then
            call u2mess('F', 'UTILITAI2_73')
        endif
    endif
!
!     --- CREATION ET REMPLISSAGE DE LA COLLECTION NOMFON.VALE ---
    call jecrec(nomfon//'.VALE', 'G V R', 'NU', 'CONTIG', 'VARIABLE',&
                nbfonc)
    call foston(nomfon//'.VALE', zk24(lnomf), nbfonc)
!
!     --- CREATION D'UN TITRE ---
    call titre()
!
!     --- IMPRESSIONS ---
    if (niv .gt. 1) call foimpr(nomfon, niv, ifm, 0, k8b)
!
    call jedema()
end subroutine
