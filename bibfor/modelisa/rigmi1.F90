subroutine rigmi1(noma, nogr, ifreq, nfreq, ifmis,&
                  rigma, rigma2, rigto)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/irmiim.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: ifmis
    integer :: ifreq, nfreq
    character(len=8) :: noma
    character(len=24) :: nogr
    real(kind=8) :: rigma(*), rigma2(*), rigto(*)
!      REAL*8       FREQ, RIGMA(*), RIGTO(*)
!     ------------------------------------------------------------------
!
    character(len=8) :: nommai
    character(len=24) :: mlgnma, magrma, manoma, tabrig
!
!
!-----------------------------------------------------------------------
    integer :: i1,  ifr, ii, ij, im, in
    integer :: inoe,  iret, jrig, ldgm, ldnm
    integer :: nb, nbmode, nbno, noemax
    real(kind=8) :: r1, r2, r3
    integer, pointer :: noeud(:) => null()
    integer, pointer :: parno(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    ifr = iunifi('RESULTAT')
!
    magrma = noma//'.GROUPEMA'
    manoma = noma//'.CONNEX'
    mlgnma = noma//'.NOMMAI'
    noemax = 0
!
!
    call jelira(jexnom(magrma, nogr), 'LONUTI', nb)
    call jeveuo(jexnom(magrma, nogr), 'L', ldgm)
    do 22 in = 0, nb-1
        call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
        inoe = zi(ldnm)
        noemax = max(noemax,inoe)
22  end do
!
!        TABLEAU DE PARTICIPATION DES NOEUDS DE L INTERFACE
!
    AS_ALLOCATE(vi=parno, size=noemax)
!
    call jelira(jexnom(magrma, nogr), 'LONUTI', nb)
    call jeveuo(jexnom(magrma, nogr), 'L', ldgm)
    do 23 in = 0, nb-1
        call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
        inoe = zi(ldnm)
        parno(inoe) = parno(inoe) + 1
23  end do
!
    nbno = 0
    do 25 ij = 1, noemax
        if (parno(ij) .eq. 0) goto 25
        nbno = nbno + 1
25  end do
!
    AS_ALLOCATE(vi=noeud, size=nbno)
    ii = 0
    do 26 ij = 1, noemax
        if (parno(ij) .eq. 0) goto 26
        ii = ii + 1
        noeud(ii) = ij
26  end do
!
!     LECTURE DES RIGIDITES ELEMENTAIRES
!
    tabrig = '&&ACEARM.RIGM'
    call jeexin(tabrig, iret)
    if (iret .eq. 0) call irmiim(ifmis, ifreq, nfreq, nbno, tabrig)
    call jeveuo(tabrig, 'L', jrig)
    nbmode = 3*nbno
    im = 0
    i1 = 0
!      CALL JELIRA(JEXNOM(MAGRMA,NOGR),'LONUTI',NB,K8B)
!      CALL JEVEUO(JEXNOM(MAGRMA,NOGR),'L',LDGM)
    do 33 in = 0, nb-1
        im = zi(ldgm+in)
        call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
        do 37 ii = 1, nbno
            if (zi(ldnm) .eq. noeud(ii)) i1 = ii
37      continue
        rigma(3*in+1) = zr(jrig+(3*i1-3)*nbmode+3*i1-3)
        rigma(3*in+2) = zr(jrig+(3*i1-2)*nbmode+3*i1-2)
        rigma(3*in+3) = zr(jrig+(3*i1-1)*nbmode+3*i1-1)
33  end do
!
    do 34 in = 0, nb-1
        im = zi(ldgm+in)
        call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
        do 38 ii = 1, nbno
            if (zi(ldnm) .eq. noeud(ii)) i1 = ii
38      continue
        r1 = rigma(3*in+1)
        r2 = rigma(3*in+2)
        r3 = rigma(3*in+3)
!
        rigto(3*(im-1)+1) = r1 + rigto(3*(im-1)+1)
        rigto(3*(im-1)+2) = r2 + rigto(3*(im-1)+2)
        rigto(3*(im-1)+3) = r3 + rigto(3*(im-1)+3)
!
        r1 = rigto(3*(im-1)+1) + rigma2(3*(i1-1)+1)
        r2 = rigto(3*(im-1)+2) + rigma2(3*(i1-1)+2)
        r3 = rigto(3*(im-1)+3) + rigma2(3*(i1-1)+3)
!
        rigma(3*in+1) = r1
        rigma(3*in+2) = r2
        rigma(3*in+3) = r3
        call jenuno(jexnum(mlgnma, im), nommai)
        write(ifr,1000) nommai,r1,r2,r3
34  end do
!
    1000 format(2x,'_F ( MAILLE=''',a8,''',',1x,'CARA= ''K_T_D_N'' , ',&
     &      /7x,'VALE=(',1x,3(1x,1pe12.5,','),1x,'),',&
     &      /'   ),')
!
    AS_DEALLOCATE(vi=parno)
    AS_DEALLOCATE(vi=noeud)
!
    call jedema()
end subroutine
