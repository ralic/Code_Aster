subroutine asmael(ma1, ma2, mag)
    implicit none
#include "jeveux.h"
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
#include "asterfort/ssdmte.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
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
!     POUR LA SOUS-STRUCTURATION
!
!-----------------------------------------------------------------------
!
    character(len=8) :: noma, nono, nosma, nomacr
    character(len=24) :: valk(3), nogma, nogno
    real(kind=8) :: x, y, z, drefe, dij
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, i1, i1noe, i1nol, iacon1, iacon2, iaconx
    integer :: iacoo2, iacoor, iadesm, iadim1, iadim2, iadime, iadimp
    integer :: iagma1, iagma2, iagmax, iagno1, iagno2, iagnox, iancnf
    integer :: ianmc1, ianmc2, ianmcr, ianon2, iapar1, iapar2, iaparr
    integer :: iasup1, iasup2, iasupm, iatyma, iatyp1, iatyp2, iatypx
    integer :: ibid, ico, icompt, ii, iret, iret1, iret2
    integer :: itrou, j, l1, l2, l3, n, nbgm1
    integer :: nbgm2, nbgma, nbgn1, nbgn2, nbgno, nbl1, nbm1
    integer :: nbm2, nbma, nbn1, nbn2, nbno, nbnoe, nbnol
    integer :: nbsm1, nbsm2, nbsma, ncoor
!-----------------------------------------------------------------------
    call jemarq()
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
    do 11,i=1,5
    zi(iadime-1+i)=zi(iadim1-1+i)+zi(iadim2-1+i)
    11 end do
    if (zi(iadim1-1+6) .ne. zi(iadim2-1+6)) then
        call utmess('A', 'SOUSTRUC_1')
    endif
!
    ncoor=zi(iadim1-1+6)
    zi(iadime-1+6)=ncoor
!
    nbsma=zi(iadime-1+4)
    nbsm1=zi(iadim1-1+4)
    nbsm2=zi(iadim2-1+4)
!
    nbma=zi(iadime-1+3)
    nbm1=zi(iadim1-1+3)
    nbm2=zi(iadim2-1+3)
!
    nbno=zi(iadime-1+1)
    nbn1=zi(iadim1-1+1)
    nbn2=zi(iadim2-1+1)
!
    nbl1=zi(iadim1-1+2)
!
!
!     --OBJET .NOMACR :
!     -----------------
    if (nbsma .gt. 0) then
        call wkvect(mag//'.NOMACR', 'G V K8', nbsma, ianmcr)
        if (nbsm1 .gt. 0) call jeveuo(ma1//'.NOMACR', 'L', ianmc1)
        if (nbsm2 .gt. 0) call jeveuo(ma2//'.NOMACR', 'L', ianmc2)
        do 12,i=1,nbsm1
        zk8(ianmcr-1+i)=zk8(ianmc1-1+i)
12      continue
        do 13,i=1,nbsm2
        zk8(ianmcr-1+nbsm1+i)=zk8(ianmc2-1+i)
13      continue
    endif
!
!
!     --OBJET .DIME_2 (V):
!     -----------------
    if (nbsma .gt. 0) then
        call wkvect(mag//'.DIME_2', 'V V I', 4*nbsma, iadimp)
        i1noe= 0
        i1nol= 0
        do 14,i=1,nbsma
        nomacr=zk8(ianmcr-1+i)
        call jeveuo(nomacr//'.DESM', 'L', iadesm)
        nbnoe= zi(iadesm-1+2)
        nbnol= zi(iadesm-1+8)+zi(iadesm-1+9)
        zi(iadimp-1+4*(i-1)+1)=nbnoe
        zi(iadimp-1+4*(i-1)+2)=nbnol
        zi(iadimp-1+4*(i-1)+3)=i1noe
        zi(iadimp-1+4*(i-1)+4)=i1nol
        i1noe= i1noe+nbnoe
        i1nol= i1nol+nbnol
14      continue
    endif
!
!
!     --OBJET .PARA_R :
!     -----------------
    if (nbsma .gt. 0) then
        call wkvect(mag//'.PARA_R', 'G V R', 14*nbsma, iaparr)
        if (nbsm1 .gt. 0) call jeveuo(ma1//'.PARA_R', 'L', iapar1)
        if (nbsm2 .gt. 0) call jeveuo(ma2//'.PARA_R', 'L', iapar2)
        do 16,i=1,14*nbsm1
        zr(iaparr-1+i)=zr(iapar1-1+i)
16      continue
        do 17,i=1,14*nbsm2
        zr(iaparr-1+nbsm1+i)=zr(iapar2-1+i)
17      continue
    endif
!
!
!     --OBJET .SUPMAIL:
!     -----------------
    if (nbsma .gt. 0) then
        call jecrec(mag//'.SUPMAIL', 'G V I', 'NO', 'DISPERSE', 'VARIABLE',&
                    nbsma)
        do 18,i=1,nbsm1
        call jeveuo(jexnum(ma1//'.SUPMAIL', i), 'L', iasup1)
        call jelira(jexnum(ma1//'.SUPMAIL', i), 'LONMAX', n)
        call jenuno(jexnum(ma1//'.SUPMAIL', i), nosma)
        call jecroc(jexnom(mag//'.SUPMAIL', nosma))
        call jeecra(jexnum(mag//'.SUPMAIL', i), 'LONMAX', n)
        call jeveuo(jexnum(mag//'.SUPMAIL', i), 'E', iasupm)
        do 181,ii=1,n
        if (zi(iasup1-1+ii) .le. nbn1) then
            zi(iasupm-1+ii)=zi(iasup1-1+ii)
        else
            zi(iasupm-1+ii)=zi(iasup1-1+ii)+nbn2
        endif
181      continue
18      continue
        do 19,i=1,nbsm2
        i1= i+nbsm1
        call jeveuo(jexnum(ma2//'.SUPMAIL', i), 'L', iasup2)
        call jelira(jexnum(ma2//'.SUPMAIL', i), 'LONMAX', n)
        call jenuno(jexnum(ma2//'.SUPMAIL', i), nosma)
        call jeexin(jexnom(mag//'.SUPMAIL', nosma), iret)
        if (iret .gt. 0) then
            call utmess('F', 'SOUSTRUC_2', sk=nosma)
        endif
        call jecroc(jexnom(mag//'.SUPMAIL', nosma))
        call jeecra(jexnum(mag//'.SUPMAIL', i1), 'LONMAX', n)
        call jeveuo(jexnum(mag//'.SUPMAIL', i1), 'E', iasupm)
        do 191,ii=1,n
        if (zi(iasup2-1+ii) .le. nbn2) then
            zi(iasupm-1+ii)=zi(iasup2-1+ii)+nbn1
        else
            zi(iasupm-1+ii)=zi(iasup2-1+ii)+nbn1+nbl1
        endif
191      continue
19      continue
    endif
!
!
!     --OBJET .NOMMAI:
!     ----------------
    ico=0
    if (nbma .gt. 0) then
        call jecreo(mag//'.NOMMAI', 'G N K8')
        call jeecra(mag//'.NOMMAI', 'NOMMAX', nbma)
        do 21,i=1,nbm1
        call jenuno(jexnum(ma1//'.NOMMAI', i), noma)
        call jecroc(jexnom(mag//'.NOMMAI', noma))
        ico=ico+1
21      continue
        do 22,i=1,nbm2
        call jenuno(jexnum(ma2//'.NOMMAI', i), noma)
        if (ico .eq. 0) then
            iret=0
        else
            call jenonu(jexnom(mag//'.NOMMAI', noma), iret)
        endif
        if (iret .gt. 0) then
            call utmess('F', 'SOUSTRUC_3', sk=noma)
        endif
        call jecroc(jexnom(mag//'.NOMMAI', noma))
22      continue
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
        if (nbm1 .gt. 0) call jelira(ma1//'.CONNEX', 'LONT', l1)
        if (nbm2 .gt. 0) call jelira(ma2//'.CONNEX', 'LONT', l2)
        l3= l1+l2
        call jeecra(mag//'.CONNEX', 'LONT', l3)
        do 25,i=1,nbm1
        call jeveuo(jexnum(ma1//'.CONNEX', i), 'L', iacon1)
        call jelira(jexnum(ma1//'.CONNEX', i), 'LONMAX', n)
        call jeecra(jexnum(mag//'.CONNEX', i), 'LONMAX', n)
        call jeveuo(jexnum(mag//'.CONNEX', i), 'E', iaconx)
        do 251,ii=1,n
        zi(iaconx-1+ii)=zi(iacon1-1+ii)
251      continue
25      continue
        do 26,i=1,nbm2
        i1= i+nbm1
        call jeveuo(jexnum(ma2//'.CONNEX', i), 'L', iacon2)
        call jelira(jexnum(ma2//'.CONNEX', i), 'LONMAX', n)
        call jeecra(jexnum(mag//'.CONNEX', i1), 'LONMAX', n)
        call jeveuo(jexnum(mag//'.CONNEX', i1), 'E', iaconx)
        do 261,ii=1,n
        zi(iaconx-1+ii)=zi(iacon2-1+ii)+nbn1
261      continue
26      continue
    endif
!
!
!     --OBJET .TYPMAIL:
!     -----------------
    if (nbma .gt. 0) then
        call wkvect(mag//'.TYPMAIL', 'G V I', nbma, ibid)
        do 27,i=1,nbm1
        call jeveuo(ma1//'.TYPMAIL', 'L', iatyma)
        iatyp1=iatyma-1+i
        call jeveuo(mag//'.TYPMAIL', 'E', iatyma)
        iatypx=iatyma-1+i
        zi(iatypx)=zi(iatyp1)
27      continue
        do 28,i=1,nbm2
        i1=i+nbm1
        call jeveuo(ma2//'.TYPMAIL', 'L', iatyma)
        iatyp2=iatyma-1+i
        call jeveuo(mag//'.TYPMAIL', 'E', iatyma)
        iatypx=iatyma-1+i1
        zi(iatypx)=zi(iatyp2)
28      continue
    endif
!
!
!     --OBJET .GROUPEMA:
!     -----------------
    call jeexin(ma1//'.GROUPEMA', iret1)
    call jeexin(ma2//'.GROUPEMA', iret2)
    nbgm1 = 0
    nbgm2 = 0
    if (iret1 .gt. 0) call jelira(ma1//'.GROUPEMA', 'NUTIOC', nbgm1)
    if (iret2 .gt. 0) call jelira(ma2//'.GROUPEMA', 'NUTIOC', nbgm2)
    nbgma = nbgm1 + nbgm2
    if (nbgma .gt. 0) then
        call jecreo(mag//'.PTRNOMMAI', 'G N K24')
        call jeecra(mag//'.PTRNOMMAI', 'NOMMAX', nbgma)
        call jecrec(mag//'.GROUPEMA', 'G V I', 'NO '//mag//'.PTRNOMMAI', 'DISPERSE', 'VARIABLE',&
                    nbgma)
        do 31,i=1,nbgm1
        call jeveuo(jexnum(ma1//'.GROUPEMA', i), 'L', iagma1)
        call jelira(jexnum(ma1//'.GROUPEMA', i), 'LONUTI', n)
        call jenuno(jexnum(ma1//'.GROUPEMA', i), nogma)
        call jecroc(jexnom(mag//'.GROUPEMA', nogma))
        call jeecra(jexnum(mag//'.GROUPEMA', i), 'LONMAX', max(1, n))
        call jeecra(jexnum(mag//'.GROUPEMA', i), 'LONUTI', n)
        call jeveuo(jexnum(mag//'.GROUPEMA', i), 'E', iagmax)
        do 311, ii=1,n
        zi(iagmax-1+ii)=zi(iagma1-1+ii)
311      continue
31      continue
        icompt = 0
        do 32,i=1,nbgm2
        call jeveuo(jexnum(ma2//'.GROUPEMA', i), 'L', iagma2)
        call jelira(jexnum(ma2//'.GROUPEMA', i), 'LONUTI', n)
        call jenuno(jexnum(ma2//'.GROUPEMA', i), nogma)
        call jeexin(jexnom(mag//'.GROUPEMA', nogma), iret)
        if (iret .gt. 0) then
            call utmess('A', 'SOUSTRUC_4', sk=nogma)
            goto 32
        endif
        icompt = icompt + 1
        i1 = nbgm1 + icompt
        call jecroc(jexnom(mag//'.GROUPEMA', nogma))
        call jeecra(jexnum(mag//'.GROUPEMA', i1), 'LONMAX', max(1, n))
        call jeecra(jexnum(mag//'.GROUPEMA', i1), 'LONUTI', n)
        call jeveuo(jexnum(mag//'.GROUPEMA', i1), 'E', iagmax)
        do 321, ii=1,n
        zi(iagmax-1+ii)=zi(iagma2-1+ii)+nbm1
321      continue
32      continue
    endif
!
!
!     --OBJET .GROUPENO:
!     -----------------
    call jeexin(ma1//'.GROUPENO', iret1)
    call jeexin(ma2//'.GROUPENO', iret2)
    nbgn1 = 0
    nbgn2 = 0
    if (iret1 .gt. 0) call jelira(ma1//'.GROUPENO', 'NUTIOC', nbgn1)
    if (iret2 .gt. 0) call jelira(ma2//'.GROUPENO', 'NUTIOC', nbgn2)
    nbgno = nbgn1 + nbgn2
    if (nbgno .gt. 0) then
        call jecreo(mag//'.PTRNOMNOE', 'G N K24')
        call jeecra(mag//'.PTRNOMNOE', 'NOMMAX', nbgno)
        call jecrec(mag//'.GROUPENO', 'G V I', 'NO '//mag//'.PTRNOMNOE', 'DISPERSE', 'VARIABLE',&
                    nbgno)
        do 33,i=1,nbgn1
        call jeveuo(jexnum(ma1//'.GROUPENO', i), 'L', iagno1)
        call jelira(jexnum(ma1//'.GROUPENO', i), 'LONUTI', n)
        call jenuno(jexnum(ma1//'.GROUPENO', i), nogma)
        call jecroc(jexnom(mag//'.GROUPENO', nogma))
        call jeecra(jexnum(mag//'.GROUPENO', i), 'LONMAX', max(1, n))
        call jeecra(jexnum(mag//'.GROUPENO', i), 'LONUTI', n)
        call jeveuo(jexnum(mag//'.GROUPENO', i), 'E', iagnox)
        do 331, ii=1,n
        zi(iagnox-1+ii)=zi(iagno1-1+ii)
331      continue
33      continue
        icompt = 0
        do 34,i=1,nbgn2
        call jeveuo(jexnum(ma2//'.GROUPENO', i), 'L', iagno2)
        call jelira(jexnum(ma2//'.GROUPENO', i), 'LONUTI', n)
        call jenuno(jexnum(ma2//'.GROUPENO', i), nogno)
        call jeexin(jexnom(mag//'.GROUPENO', nogno), iret)
        if (iret .gt. 0) then
            call utmess('A', 'SOUSTRUC_5', sk=nogno)
            goto 34
        endif
        icompt = icompt + 1
        i1 = nbgn1 + icompt
        call jecroc(jexnom(mag//'.GROUPENO', nogno))
        call jeecra(jexnum(mag//'.GROUPENO', i1), 'LONMAX', max(1, n))
        call jeecra(jexnum(mag//'.GROUPENO', i1), 'LONUTI', n)
        call jeveuo(jexnum(mag//'.GROUPENO', i1), 'E', iagnox)
        do 341, ii=1,n
        zi(iagnox-1+ii)=zi(iagno2-1+ii)+nbn1
341      continue
34      continue
    endif
!
!
!     --OBJET .COORDO_2 (V):
!     ----------------------
    call wkvect(mag//'.COORDO_2', 'V V R', 3*nbno, iacoo2)
    call jeveuo(ma1//'.COORDO    .VALE', 'L', iacoor)
    do 41, i=1,3*nbn1
    zr(iacoo2-1+i)=zr(iacoor-1+i)
    41 end do
    call jeveuo(ma2//'.COORDO    .VALE', 'L', iacoor)
    do 42, i=1,3*nbn2
    zr(iacoo2-1+3*nbn1+i)=zr(iacoor-1+i)
    42 end do
!
!
!     --OBJET .NOEUD_CONF ET .NOMNOE_2 (V):
!     -------------------------------------
    call wkvect(mag//'.NOEUD_CONF', 'V V I', nbno, iancnf)
    call wkvect(mag//'.NOMNOE_2', 'V V K8', nbno, ianon2)
    do 43, i=1,nbno
    zi(iancnf-1+i)=i
    43 end do
    do 44, i=1,nbn1
    call jenuno(jexnum(ma1//'.NOMNOE', i), nono)
    zk8(ianon2-1+i)=nono
    44 end do
    do 45, i=1,nbn2
    call jenuno(jexnum(ma2//'.NOMNOE', i), nono)
    zk8(ianon2-1+nbn1+i)=nono
    call jenonu(jexnom(ma1//'.NOMNOE', nono), itrou)
    if (itrou .gt. 0) then
        zi(iancnf-1+nbn1+i)=itrou
    endif
    45 end do
!
!
!     --ON VERIFIE QUE LES NOEUDS CONFONDUS NE SONT PAS TROP DISTANTS:
!     ----------------------------------------------------------------
    drefe=0.0d0
    do 51, i=1,nbno
    x=zr(iacoo2-1+3*(i-1)+1)-zr(iacoo2-1+1)
    y=zr(iacoo2-1+3*(i-1)+2)-zr(iacoo2-1+2)
    z=zr(iacoo2-1+3*(i-1)+3)-zr(iacoo2-1+3)
    drefe= max(drefe,sqrt(x**2+y**2+z**2))
    51 end do
    do 52, i=1,nbno
    j=zi(iancnf-1+i)
    if (j .ne. i) then
        x=zr(iacoo2-1+3*(i-1)+1)-zr(iacoo2-1+3*(j-1)+1)
        y=zr(iacoo2-1+3*(i-1)+2)-zr(iacoo2-1+3*(j-1)+2)
        z=zr(iacoo2-1+3*(i-1)+3)-zr(iacoo2-1+3*(j-1)+3)
        dij= sqrt(x**2+y**2+z**2)
        if (dij .gt. 1.0d-6*drefe) then
            valk(1) = zk8(ianon2-1+i)
            valk(2) = ma1
            valk(3) = ma2
            call utmess('A', 'SOUSTRUC_6', nk=3, valk=valk)
        endif
    endif
    52 end do
!
!
!     --ON "TERMINE" LE MAILLAGE:
!     ---------------------------
    call ssdmte(mag)
!
    call jedema()
end subroutine
