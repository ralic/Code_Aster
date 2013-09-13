subroutine calcsp(casint, nomu, table, freq, masg,&
                  nbm, nbmr, imod1, nuor, ivite)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     CALCUL POUR CHAQUE VITESSES DES INTERSPECTRES DE REPONSES
!-----------------------------------------------------------------------
! IN  : CASINT: BOOLEEN CARACTERISANT L'OPTION DE CALCUL
!       CASINT= TRUE   => ON CALCULE TOUS LES INTERSPECTRES
!       CASINT= FALSE  => ON CALCULE LES AUTOSPECTRES UNIQUEMENT
! IN  : NOMU  : NOM UTILISATEUR DU CONCEPT TABL_INTSP CORRESPONDANT
!               AUX INTERSPECTRES DE REPONSES : A PRODUIRE
! IN  : TABLE : NOM UTILISATEUR DU CONCEPT TABL_INTSP CORRESPONDANT
!               AUX INTERSPECTRES D'EXCITATIONS : DONNEE DU CALCUL
! IN  : FREQ  : TABLEAU DES FREQUENCES ET AMORTISSEMENTS
! IN  : MASG  : TABLEAU DES MASSES GENERALISEES
! IN  : NBM   : NOMBRE DE MODES DE LA BASE DE CONCEPT MELASFLU
! IN  : NBMR  : NOMBRE DE MODES PRIS EN COMPTE
! IN  : IMOD1 : INDICE DU PREMIER MODE PRIS EN COMPTE DANS LA BASE DE
!               CONCEPT MELASFLU
! IN  : NUOR  : LISTE DES NUMEROS D'ORDRE DES MODES PRIS EN COMPTE
! IN  : IVITE : NUMERO VITESSE DU FLUIDE
!     ----------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    logical :: casint
    character(len=8) :: nomu, table
    integer :: nbm, nbmr, imod1, nuor(*), ivite
    real(kind=8) :: freq(*), masg(*)
!
!-----------------------------------------------------------------------
    integer :: ideb, ifonc, ihi, ihi1, ihr
    integer :: ihr1, il, im, im1, im2, imb, imodf
    integer :: ip, iv, lvale, nbpf
!
    real(kind=8) :: fr, fri, hhi, hhr, hii1, hii2, hir1
    real(kind=8) :: hir2, pi
!-----------------------------------------------------------------------
    integer :: ival(3), vali(2)
    integer :: lnumi, lnumj, lfreq, i1, nbabs
    integer :: lrnumi, lrnumj, lrfreq, mxval, mrxval, ipf
    real(kind=8) :: mgi, ksi, hdenom
    character(len=24) :: chnumi, chnumj, chfreq, chvale
    character(len=24) :: crnumi, crnumj, crfreq, crvale
!
!-----------------------------------------------------------------------
    call jemarq()
!
    pi = r8pi()
    imodf = imod1 + nbmr - 1
!
    chnumi = table//'.NUMI'
    chnumj = table//'.NUMJ'
    chfreq = table//'.FREQ'
    chvale = table//'.VALE'
    call jeveuo(chnumi, 'L', lnumi)
    call jeveuo(chnumj, 'L', lnumj)
    call jeveuo(chfreq, 'L', lfreq)
    call jelira(chnumi, 'LONMAX', mxval)
    call jelira(chfreq, 'LONMAX', nbpf)
!
    crnumi = nomu//'.NUMI'
    crnumj = nomu//'.NUMJ'
    crfreq = nomu//'.FREQ'
    crvale = nomu//'.VALE'
    call wkvect(crfreq, 'G V R', nbpf, lrfreq)
    do 240 ip = 1, nbpf
        zr(lrfreq+ip-1) = zr(lfreq+ip-1)
240  end do
!
    mrxval = 0
    do 250 im2 = 1, nbmr
        ideb = im2
        if (casint) ideb = 1
        do 260 im1 = ideb, im2
            mrxval = mrxval+1
260      continue
250  end do
!
    call wkvect(crnumi, 'G V I', mrxval, lrnumi)
    call wkvect(crnumj, 'G V I', mrxval, lrnumj)
!
    call jecrec(crvale, 'G V R', 'NU', 'DISPERSE', 'VARIABLE',&
                mrxval)
!
! --- CREATION DE VECTEURS DE TRAVAIL ---
!
    call wkvect('&&CALCSP.TEMP.HR  ', 'V V R8', nbmr*nbpf, ihr)
    call wkvect('&&CALCSP.TEMP.HI  ', 'V V R8', nbmr*nbpf, ihi)
!
    iv = ivite
    do 25 im = imod1, imodf
        fri = freq(2*nbm*(iv-1)+2*(im-1)+1)
        if (fri .lt. 0.d0) then
            vali(1) = iv
            vali(2) = nuor(im)
            call utmess('A', 'MODELISA2_90', ni=2, vali=vali)
            goto 20
        endif
25  continue
!
    do 30 im = imod1, imodf
!
        mgi = masg(im)*4.d0*pi*pi
        fri = freq(2*nbm*(iv-1)+2*(im-1)+1)
        ksi = freq(2*nbm*(iv-1)+2*(im-1)+2)
        if (ksi .le. 0.d0) ksi = 1.d-06
!
        imb = im - imod1 + 1
!
        do 40 ip = 1, nbpf
            fr = zr(lfreq+ip-1)
            ihr1 = ihr+nbpf*(imb-1)+ip-1
            ihi1 = ihi+nbpf*(imb-1)+ip-1
            zr(ihr1) = (mgi*(fri*fri - fr*fr))
            zr(ihi1) = (mgi*ksi*fr*fri*2.d0)
!
40      continue
30  continue
!
    ipf = 1
    do 50 im2 = 1, nbmr
!
        ival(3) = nuor(im2)
!
        ideb = im2
        if (casint) ideb = 1
!
        do 60 im1 = ideb, im2
!
            ival(2) = nuor(im1)
!
            do 200 i1 = 1, mxval
                if ((zi(lnumi-1+i1) .eq. ival(3)) .and. (zi(lnumj-1+ i1) .eq. ival(2))) then
                    call jeveuo(jexnum(chvale, i1), 'L', ifonc)
                endif
200          continue
!
            call jecroc(jexnum(crvale, ipf))
            zi(lrnumi-1+ipf) = ival(3)
            zi(lrnumj-1+ipf) = ival(2)
            if (ival(2) .eq. ival(3)) then
                nbabs = nbpf
            else
                nbabs = 2*nbpf
            endif
            call jeecra(jexnum(crvale, ipf), 'LONMAX', nbabs)
            call jeecra(jexnum(crvale, ipf), 'LONUTI', nbabs)
            call jeveuo(jexnum(crvale, ipf), 'E', lvale)
            ipf = ipf + 1
!
            do 80 il = 1, nbpf
                hir1 = zr(ihr+nbpf*(im1-1)+il-1)
                hii1 = zr(ihi+nbpf*(im1-1)+il-1)
                hir2 = zr(ihr+nbpf*(im2-1)+il-1)
                hii2 = zr(ihi+nbpf*(im2-1)+il-1)
                hdenom = (hir1*hir1+hii1*hii1)*(hir2*hir2+hii2*hii2)
                hhr = (hir1*hir2 + hii1*hii2)/hdenom
                hhi = (hir2*hii1 - hir1*hii2)/hdenom
!
                if (ival(2) .eq. ival(3)) then
                    zr(lvale+il-1) = hhr*zr(ifonc+il-1)
                else
                    zr(lvale+2*(il-1)) = hhr*zr( ifonc+2*(il-1))- hhi*zr(ifonc+2*(il-1)+1 )
                    zr(lvale+2*(il-1)+1)= hhr*zr(ifonc+2*(il-1)+1)+&
                    hhi*zr(ifonc+2*(il-1))
                endif
80          continue
!
60      continue
50  continue
20  continue
!
!
! --- MENAGE
!
    call jedetr('&&CALCSP.TEMP.HR  ')
    call jedetr('&&CALCSP.TEMP.HI  ')
!
    call jedema()
end subroutine
