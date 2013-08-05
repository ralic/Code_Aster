subroutine xpoco1(dirma, nbma, dirno, nbno, ma1,&
                  ma2, jnivgr)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
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
    character(len=8) :: ma1, ma2
    integer :: nbma, dirma(nbma), nbno, dirno(nbno), jnivgr
!
!   COPIE DANS LE MAILLAGE MA2 DES MAILLES ET DES NOEUDS DU MAILLAGE MA1
!   CONTENUS DANS LES TABLEAUX D'INDIRECTION DIRMA ET DIRNO
!
!   IN
!       DIRMA : TABLEAU DE CORRESPONDANCE DES NUMEROS DE MAILLES
!       DIRNO : TABLEAU DE CORRESPONDANCE DES NUMEROS DE NOEUDS
!       NBMA  : LONGUEUR DE DIRMA
!       NBNO  : LONGUEUR DE DIRNO
!       MA1   : NOM DU MAILLAGE SAIN
!      JNIVGR : ADRESSE DU VECTEUR DE REMPLISSAGE DES GROUP_MA DE MAXFEM
!
!   OUT
!       MA2   : NOM DU MAILLAGE FISSURE
!
!
!
    integer :: jtypm1, jtypm2, i, j, ino1, iret, nbgn, iagno
    integer :: iacoo1, iacoo2, iacon1, n, iacon2
    integer :: ino2, nbgm2, i1, i2, iagma1, iagma2, n1, n2, ima
    character(len=8) :: noma2, nono2, k8bid
    character(len=24) :: nogma
!
    call jemarq()
!
!     RECUP DES .TYPMAIL, .COORDO DU MAILLAGE 1 ET 2
    call jeveuo(ma1//'.TYPMAIL', 'L', jtypm1)
    call jeveuo(ma2//'.TYPMAIL', 'E', jtypm2)
    call jeveuo(ma1//'.COORDO    .VALE', 'L', iacoo1)
    call jeveuo(ma2//'.COORDO    .VALE', 'E', iacoo2)
!
    call jeexin(ma2//'.GROUPENO', iret)
    nbgn = 0
    if (iret .gt. 0) call jelira(ma2//'.GROUPENO', 'NUTIOC', nbgn, k8bid)
!
    call jeexin(ma2//'.GROUPEMA', iret)
    nbgm2 = 0
    if (iret .gt. 0) call jelira(ma2//'.GROUPEMA', 'NUTIOC', nbgm2, k8bid)
!
!     ---------------------------------------------------------------
!     COPIE DES VECTEURS
!     ---------------------------------------------------------------
!
!     .NOMMAI ET .TYPMAIL
    do 100 i = 1, nbma
        if (dirma(i) .ne. 0) then
            call jenuno(jexnum(ma1//'.NOMMAI', dirma(i)), noma2)
            call jecroc(jexnom(ma2//'.NOMMAI', noma2))
            zi(jtypm2-1+dirma(i)) = zi(jtypm1-1+i)
        endif
100  end do
!
!     .NOMNOE
    do 200 i = 1, nbno
        if (dirno(i) .ne. 0) then
            call jenuno(jexnum(ma1//'.NOMNOE', dirno(i)), nono2)
            call jecroc(jexnom(ma2//'.NOMNOE', nono2))
        endif
200  end do
!
!     .COORDO
    do 300 i = 1, nbno
        if (dirno(i) .ne. 0) then
            do 310 j = 1, 3
                zr(iacoo2-1+3*(dirno(i)-1)+j)=zr(iacoo1-1+3*(i-1)+j)
310          continue
        endif
300  end do
!
!     .CONNEX
    do 400 i = 1, nbma
        if (dirma(i) .ne. 0) then
            call jeveuo(jexnum(ma1//'.CONNEX', i), 'L', iacon1)
            call jelira(jexnum(ma1//'.CONNEX', i), 'LONMAX', n, k8bid)
            call jeecra(jexnum(ma2//'.CONNEX', dirma(i)), 'LONMAX', n, k8bid)
            call jeveuo(jexnum(ma2//'.CONNEX', dirma(i)), 'E', iacon2)
            do 410 j = 1, n
                ino1 = zi(iacon1-1+j)
                ino2 = dirno(ino1)
                zi(iacon2-1+j)=ino2
410          continue
        endif
400  end do
!
!     .GROUPENO
    do 500 i = 1, nbgn
        call jeveuo(jexnum(ma2//'.GROUPENO', i), 'E', iagno)
        call jelira(jexnum(ma2//'.GROUPENO', i), 'LONUTI', n, k8bid)
        do 501 j = 1, n
            if (dirno(zi(iagno-1+j)) .ne. 0) then
                zi(iagno-1+j)=dirno(zi(iagno-1+j))
            endif
501      continue
500  end do
!
!     .GROUPEMA
    do 600 i2 = 1, nbgm2
        call jenuno(jexnum(ma2//'.GROUPEMA', i2), nogma)
        call jenonu(jexnom(ma1//'.GROUPEMA', nogma), i1)
        call jeveuo(jexnum(ma1//'.GROUPEMA', i1), 'L', iagma1)
        call jelira(jexnum(ma1//'.GROUPEMA', i1), 'LONUTI', n1, k8bid)
        call jeveuo(jexnum(ma2//'.GROUPEMA', i2), 'E', iagma2)
        call jelira(jexnum(ma2//'.GROUPEMA', i2), 'LONUTI', n2, k8bid)
        do 610 i = 1, n1
            ima = zi(iagma1-1+i)
            if (dirma(ima) .ne. 0) then
!           NIVEAU DE REMPLISSAGE DU GROUP_MA
                zi(jnivgr-1+i2) = zi(jnivgr-1+i2) + 1
                zi(iagma2-1+zi(jnivgr-1+i2)) = dirma(ima)
            endif
610      continue
        ASSERT(zi(jnivgr-1+i2).le.n2)
600  end do
!
    call jedema()
end subroutine
