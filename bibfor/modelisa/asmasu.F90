subroutine asmasu(ma1, ma2, mag)
    implicit none
#include "jeveux.h"
!
#include "asterfort/codent.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    character(len=8) :: ma1, ma2, mag
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     OPERATEUR: ASSE_MAILLAGE / CAS DE L ASSEMBLAGE DE MAILLAGES
!     AVEC SUPERPOSITION
!
!-----------------------------------------------------------------------
!
    character(len=1) :: kkk
    character(len=8) :: kind, kbid
    character(len=19) :: coordo
    character(len=8) :: noma, nono
    character(len=24) :: nogma, nogmab, nogno, nognob
    integer :: nbma, nbm1, nbm2, nbno, nbn1, nbn2, nbgma, nbgm1, nbgm2
    integer :: i1, icompt, ino, l1, l2, l3, i, n, ncoor, k, ifm, niv
    integer :: iadim1, iadim2, iadime
    integer :: iagma1, iagma2, iagmax
    integer :: iacon1, iacon2, iaconx
    integer :: iagno1, iagno2, iagnox
    integer :: iatyp1, iatyp2, iatypx
    integer :: nbgno, nbgn1, nbgn2, ii, igeomr, iadesc, ibid, iarefe
    integer :: iatyma, iacoo1, iacoo2, iavale, iret, iret1, iret2
!
!     ------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
!     --OBJET .DIME :
!     ---------------
    call jeveuo(ma1//'.DIME', 'L', iadim1)
    call jeveuo(ma2//'.DIME', 'L', iadim2)
    call wkvect(mag//'.DIME', 'G V I', 6, iadime)
!CC SOMME POUR : 1 LE NB DE NOEUDS,
!CC              2       DE NOEUDS LAGRANGES,
!CC              3       DE MAILLES,
!CC              4       DE SUPER MAILLES
!CC              5       DU MAJORANT DE SUPER MAILLES
    do 10,i=1,5
    zi(iadime-1+i)=zi(iadim1-1+i)+zi(iadim2-1+i)
    10 end do
!
    ncoor=max(zi(iadim1-1+6),zi(iadim2-1+6))
    zi(iadime-1+6)=ncoor
!
    nbma=zi(iadime-1+3)
    nbm1=zi(iadim1-1+3)
    nbm2=zi(iadim2-1+3)
!
    nbno=zi(iadime-1+1)
    nbn1=zi(iadim1-1+1)
    nbn2=zi(iadim2-1+1)
!
!     --OBJET .NOMMAI:
!     ----------------
    if (nbma .gt. 0) then
        call jecreo(mag//'.NOMMAI', 'G N K8')
        call jeecra(mag//'.NOMMAI', 'NOMMAX', nbma, kbid)
        do 21,i=1,nbm1
        call codent(i, 'G', kind)
        noma='M'//kind
        call jecroc(jexnom(mag//'.NOMMAI', noma))
21      continue
        do 22,i=1,nbm2
        call codent(nbm1+i, 'G', kind)
        noma='M'//kind
        call jecroc(jexnom(mag//'.NOMMAI', noma))
22      continue
    endif
!
!     --OBJET .NOMNOE:
!     ----------------
    if (nbno .gt. 0) then
        call jecreo(mag//'.NOMNOE', 'G N K8')
        call jeecra(mag//'.NOMNOE', 'NOMMAX', nbno, kbid)
        do 31,i=1,nbn1
        call codent(i, 'G', kind)
        nono='N'//kind
        call jecroc(jexnom(mag//'.NOMNOE', nono))
31      continue
        do 32,i=1,nbn2
        call codent(nbn1+i, 'G', kind)
        nono='N'//kind
        call jecroc(jexnom(mag//'.NOMNOE', nono))
32      continue
    endif
!
!
!     --OBJET .CONNEX:
!     -----------------
    if (nbma .gt. 0) then
        call jecrec(mag//'.CONNEX', 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                    nbma)
        l1=0
        l2=0
        if (nbm1 .gt. 0) call jelira(ma1//'.CONNEX', 'LONT', l1, kbid)
        if (nbm2 .gt. 0) call jelira(ma2//'.CONNEX', 'LONT', l2, kbid)
        l3= l1+l2
        call jeecra(mag//'.CONNEX', 'LONT', l3, kbid)
        do 41,i=1,nbm1
        call jeveuo(jexnum(ma1//'.CONNEX', i), 'L', iacon1)
        call jelira(jexnum(ma1//'.CONNEX', i), 'LONMAX', n, kbid)
        call jeecra(jexnum(mag//'.CONNEX', i), 'LONMAX', n, kbid)
        call jeveuo(jexnum(mag//'.CONNEX', i), 'E', iaconx)
        do 411,ii=1,n
        zi(iaconx-1+ii)=zi(iacon1-1+ii)
411      continue
41      continue
        do 42,i=1,nbm2
        i1= i+nbm1
        call jeveuo(jexnum(ma2//'.CONNEX', i), 'L', iacon2)
        call jelira(jexnum(ma2//'.CONNEX', i), 'LONMAX', n, kbid)
        call jeecra(jexnum(mag//'.CONNEX', i1), 'LONMAX', n, kbid)
        call jeveuo(jexnum(mag//'.CONNEX', i1), 'E', iaconx)
        do 421,ii=1,n
        zi(iaconx-1+ii)=zi(iacon2-1+ii)+nbn1
421      continue
42      continue
    endif
!
!     -- CREATION DU CHAMP .COORDO :
!     ------------------------------
    coordo= mag//'.COORDO'
!
    call jenonu(jexnom('&CATA.GD.NOMGD', 'GEOM_R'), igeomr)
    call wkvect(coordo//'.DESC', 'G V I', 3, iadesc)
    call jeecra(coordo//'.DESC', 'DOCU', ibid, 'CHNO')
    zi (iadesc-1+1)= igeomr
!     -- TOUJOURS 3 COMPOSANTES X, Y ET Z
    zi (iadesc-1+2)= -3
!     -- 14 = 2**1 + 2**2 + 2**3
    zi (iadesc-1+3)= 14
!
    call wkvect(coordo//'.REFE', 'G V K24', 4, iarefe)
    zk24(iarefe-1+1)= mag
    call jeveuo(ma1//'.COORDO    .VALE', 'L', iacoo1)
    call jeveuo(ma2//'.COORDO    .VALE', 'L', iacoo2)
    call wkvect(coordo//'.VALE', 'G V R', 3*nbno, iavale)
!     -- COORDONNEES DES NOEUDS :
    do 51 , ino=1, nbn1
    do 511, k=1,3
    zr(iavale-1+3*(ino-1)+k)=zr(iacoo1-1+3*(ino-1)+k)
511  continue
    51 end do
    do 52 , ino=1, nbn2
    do 521, k=1,3
    zr(iavale-1+3*(nbn1+ino-1)+k)=zr(iacoo2-1+3*(ino-1)+k)
521  continue
    52 end do
!
!
!     --OBJET .TYPMAIL:
!     -----------------
    if (nbma .gt. 0) then
        call wkvect(mag//'.TYPMAIL', 'G V I', nbma, ibid)
        do 61,i=1,nbm1
        call jeveuo(ma1//'.TYPMAIL', 'L', iatyma)
        iatyp1=iatyma-1+i
        call jeveuo(mag//'.TYPMAIL', 'E', iatyma)
        iatypx=iatyma-1+i
        zi(iatypx)=zi(iatyp1)
61      continue
        do 62,i=1,nbm2
        i1=i+nbm1
        call jeveuo(ma2//'.TYPMAIL', 'L', iatyma)
        iatyp2=iatyma-1+i
        call jeveuo(mag//'.TYPMAIL', 'E', iatyma)
        iatypx=iatyma-1+i1
        zi(iatypx)=zi(iatyp2)
62      continue
    endif
!
!
!     --OBJET .GROUPEMA:
!     -----------------
    call jeexin(ma1//'.GROUPEMA', iret1)
    call jeexin(ma2//'.GROUPEMA', iret2)
    nbgm1 = 0
    nbgm2 = 0
    if (iret1 .gt. 0) call jelira(ma1//'.GROUPEMA', 'NUTIOC', nbgm1, kbid)
    if (iret2 .gt. 0) call jelira(ma2//'.GROUPEMA', 'NUTIOC', nbgm2, kbid)
    nbgma = nbgm1 + nbgm2
    if (nbgma .gt. 0) then
        call jecreo(mag//'.PTRNOMMAI', 'G N K24')
        call jeecra(mag//'.PTRNOMMAI', 'NOMMAX', nbgma, ' ')
        call jecrec(mag//'.GROUPEMA', 'G V I', 'NO '//mag//'.PTRNOMMAI', 'DISPERSE', 'VARIABLE',&
                    nbgma)
        do 71,i=1,nbgm1
        call jeveuo(jexnum(ma1//'.GROUPEMA', i), 'L', iagma1)
        call jelira(jexnum(ma1//'.GROUPEMA', i), 'LONUTI', n, kbid)
        call jenuno(jexnum(ma1//'.GROUPEMA', i), nogma)
        call jecroc(jexnom(mag//'.GROUPEMA', nogma))
        call jeecra(jexnum(mag//'.GROUPEMA', i), 'LONMAX', max(1, n), kbid)
        call jeecra(jexnum(mag//'.GROUPEMA', i), 'LONUTI', n, kbid)
        call jeveuo(jexnum(mag//'.GROUPEMA', i), 'E', iagmax)
        do 711, ii=1,n
        zi(iagmax-1+ii)=zi(iagma1-1+ii)
711      continue
71      continue
        icompt = 0
        do 72,i=1,nbgm2
        call jeveuo(jexnum(ma2//'.GROUPEMA', i), 'L', iagma2)
        call jelira(jexnum(ma2//'.GROUPEMA', i), 'LONUTI', n, kbid)
        call jenuno(jexnum(ma2//'.GROUPEMA', i), nogma)
        call jeexin(jexnom(mag//'.GROUPEMA', nogma), iret)
        if (iret .gt. 0) then
            call u2mesk('A', 'MODELISA2_21', 1, nogma)
            nogmab=nogma
            ii = lxlgut(nogmab(1:7))
            do 724,k=ii+1,7
            nogmab(k:k)='_'
724          continue
            do 722,k=0,9
            call codent(k, 'G', kkk)
            nogmab(8:8)=kkk
            call jeexin(jexnom(mag//'.GROUPEMA', nogmab), iret)
            if (iret .eq. 0) goto 723
722          continue
723          continue
            write (ifm,*) ' LE GROUP_MA '//nogma//' DU MAILLAGE '&
                //ma2//' EST RENOMME '//nogmab//' DANS '//mag
            nogma=nogmab
        endif
        icompt = icompt + 1
        i1 = nbgm1 + icompt
        call jecroc(jexnom(mag//'.GROUPEMA', nogma))
        call jeecra(jexnum(mag//'.GROUPEMA', i1), 'LONMAX', max(1, n), kbid)
        call jeecra(jexnum(mag//'.GROUPEMA', i1), 'LONUTI', n, kbid)
        call jeveuo(jexnum(mag//'.GROUPEMA', i1), 'E', iagmax)
        do 721, ii=1,n
        zi(iagmax-1+ii)=zi(iagma2-1+ii)+nbm1
721      continue
72      continue
    endif
!
!
!     --OBJET .GROUPENO:
!     -----------------
    call jeexin(ma1//'.GROUPENO', iret1)
    call jeexin(ma2//'.GROUPENO', iret2)
    nbgn1 = 0
    nbgn2 = 0
    if (iret1 .gt. 0) call jelira(ma1//'.GROUPENO', 'NUTIOC', nbgn1, kbid)
    if (iret2 .gt. 0) call jelira(ma2//'.GROUPENO', 'NUTIOC', nbgn2, kbid)
    nbgno = nbgn1 + nbgn2
    if (nbgno .gt. 0) then
        call jecreo(mag//'.PTRNOMNOE', 'G N K24')
        call jeecra(mag//'.PTRNOMNOE', 'NOMMAX', nbgno, ' ')
        call jecrec(mag//'.GROUPENO', 'G V I', 'NO '//mag//'.PTRNOMNOE', 'DISPERSE', 'VARIABLE',&
                    nbgno)
        do 81,i=1,nbgn1
        call jeveuo(jexnum(ma1//'.GROUPENO', i), 'L', iagno1)
        call jelira(jexnum(ma1//'.GROUPENO', i), 'LONUTI', n, kbid)
        call jenuno(jexnum(ma1//'.GROUPENO', i), nogma)
        call jecroc(jexnom(mag//'.GROUPENO', nogma))
        call jeecra(jexnum(mag//'.GROUPENO', i), 'LONMAX', max(1, n), kbid)
        call jeecra(jexnum(mag//'.GROUPENO', i), 'LONUTI', n, kbid)
        call jeveuo(jexnum(mag//'.GROUPENO', i), 'E', iagnox)
        do 811, ii=1,n
        zi(iagnox-1+ii)=zi(iagno1-1+ii)
811      continue
81      continue
        icompt = 0
        do 82,i=1,nbgn2
        call jeveuo(jexnum(ma2//'.GROUPENO', i), 'L', iagno2)
        call jelira(jexnum(ma2//'.GROUPENO', i), 'LONUTI', n, kbid)
        call jenuno(jexnum(ma2//'.GROUPENO', i), nogno)
        call jeexin(jexnom(mag//'.GROUPENO', nogno), iret)
        if (iret .gt. 0) then
            call u2mesk('A', 'MODELISA2_22', 1, nogno)
            nognob=nogno
            ii = lxlgut(nognob(1:7))
            do 824,k=ii+1,7
            nognob(k:k)='_'
824          continue
            do 822,k=0,9
            call codent(k, 'G', kkk)
            nognob(8:8)=kkk
            call jeexin(jexnom(mag//'.GROUPENO', nognob), iret)
            if (iret .eq. 0) goto 823
822          continue
823          continue
            write (ifm,*) ' LE GROUP_NO '//nogno//' DU MAILLAGE '&
                //ma2//' EST RENOMME '//nognob//' DANS '//mag
            nogno=nognob
        endif
        icompt = icompt + 1
        i1 = nbgn1 + icompt
        call jecroc(jexnom(mag//'.GROUPENO', nogno))
        call jeecra(jexnum(mag//'.GROUPENO', i1), 'LONMAX', max(1, n), kbid)
        call jeecra(jexnum(mag//'.GROUPENO', i1), 'LONUTI', n, kbid)
        call jeveuo(jexnum(mag//'.GROUPENO', i1), 'E', iagnox)
        do 821, ii=1,n
        zi(iagnox-1+ii)=zi(iagno2-1+ii)+nbn1
821      continue
82      continue
    endif
!
    call jedema()
end subroutine
