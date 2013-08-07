subroutine rigmi2(noma, nogr, ifreq, nfreq, ifmis,&
                  rigma, rigma2, rigto)
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
#include "asterfort/r8inir.h"
#include "asterfort/wkvect.h"
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
    integer :: i1, i2, idno, ifr, ii, ij, im
    integer :: in, inoe, iparno, iret, isopa, isoto
    integer :: jrig, ldgm, ldnm, nb, nbmode, nbno, noemax
!
    real(kind=8) :: r1, r2, r3
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
        inoe = zi(ldnm+1)
        noemax = max(noemax,inoe)
22  end do
!
!        TABLEAU DE PARTICIPATION DES NOEUDS DE L INTERFACE
!
    call wkvect('&&RIGMI2.PARNO', 'V V I', noemax, iparno)
!
    do 23 in = 0, nb-1
        call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
        inoe = zi(ldnm)
        zi(iparno+inoe-1) = zi(iparno+inoe-1) + 1
        inoe = zi(ldnm+1)
        zi(iparno+inoe-1) = zi(iparno+inoe-1) + 1
23  end do
!
    nbno = 0
    do 25 ij = 1, noemax
        if (zi(iparno+ij-1) .eq. 0) goto 25
        nbno = nbno + 1
25  end do
!
    call wkvect('&&RIGMI2.NOEUD', 'V V I', nbno, idno)
    ii = 0
    do 26 ij = 1, noemax
        if (zi(iparno+ij-1) .eq. 0) goto 26
        ii = ii + 1
        zi(idno+ii-1) = ij
26  end do
!
!     LECTURE DES RIGIDITES ELEMENTAIRES
!
    tabrig = '&&ACEARM.RIGM'
    call jeexin(tabrig, iret)
    if (iret .eq. 0) call irmiim(ifmis, ifreq, nfreq, nbno, tabrig)
    call jeveuo(tabrig, 'L', jrig)
    nbmode = 3*nbno
    call wkvect('&&RIGMI2.SOMTOT', 'V V R', nbmode, isoto)
    call wkvect('&&RIGMI2.SOMPAR', 'V V R', nbmode, isopa)
    do 28 i1 = 1, nbno
        do 28 i2 = 1, nbno
            if (i1 .ne. i2) then
                zr(isoto+3*i1-3) = zr(isoto+3*i1-3) + zr(jrig+(3*i2-3) *nbmode+3*i1-3)
                zr(isoto+3*i1-2) = zr(isoto+3*i1-2) + zr(jrig+(3*i2-2) *nbmode+3*i1-2)
                zr(isoto+3*i1-1) = zr(isoto+3*i1-1) + zr(jrig+(3*i2-1) *nbmode+3*i1-1)
            endif
28      continue
!
    do 33 in = 0, nb-1
        im = zi(ldgm+in)
        call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
        do 37 ii = 1, nbno
            if (zi(ldnm) .eq. zi(idno+ii-1)) i1 = ii
            if (zi(ldnm+1) .eq. zi(idno+ii-1)) i2 = ii
37      continue
        zr(isopa+3*i1-3) = zr(isopa+3*i1-3) + zr(jrig+(3*i2-3)* nbmode+3*i1-3)
        zr(isopa+3*i2-3) = zr(isopa+3*i2-3) + zr(jrig+(3*i2-3)* nbmode+3*i1-3)
        zr(isopa+3*i1-2) = zr(isopa+3*i1-2) + zr(jrig+(3*i2-2)* nbmode+3*i1-2)
        zr(isopa+3*i2-2) = zr(isopa+3*i2-2) + zr(jrig+(3*i2-2)* nbmode+3*i1-2)
        zr(isopa+3*i1-1) = zr(isopa+3*i1-1) + zr(jrig+(3*i2-1)* nbmode+3*i1-1)
        zr(isopa+3*i2-1) = zr(isopa+3*i2-1) + zr(jrig+(3*i2-1)* nbmode+3*i1-1)
33  end do
!
    do 34 in = 0, nb-1
        im = zi(ldgm+in)
        call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
        do 38 ii = 1, nbno
            if (zi(ldnm) .eq. zi(idno+ii-1)) i1 = ii
            if (zi(ldnm+1) .eq. zi(idno+ii-1)) i2 = ii
38      continue
        rigma(3*in+1) = 0.5d0*zr(&
                        jrig+(3*i2-3)*nbmode+3*i1-3)* (zr(isoto+3*i1-3)/zr(isopa+3*i1-3) + zr(iso&
                        &to+3*i2-3)/zr( isopa+3*i2-3)+0.d0&
                        )
        rigma(3*in+2) = 0.5d0*zr(&
                        jrig+(3*i2-2)*nbmode+3*i1-2)* (zr(isoto+3*i1-2)/zr(isopa+3*i1-2) + zr(iso&
                        &to+3*i2-2)/zr( isopa+3*i2-2)+0.d0&
                        )
        rigma(3*in+3) = 0.5d0*zr(&
                        jrig+(3*i2-1)*nbmode+3*i1-1)* (zr(isoto+3*i1-1)/zr(isopa+3*i1-1) + zr(iso&
                        &to+3*i2-1)/zr( isopa+3*i2-1)+0.d0&
                        )
34  end do
!
    call r8inir(3*nbno, 0.d0, rigma2, 1)
    do 35 in = 0, nb-1
        im = zi(ldgm+in)
        call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
        do 39 ii = 1, nbno
            if (zi(ldnm) .eq. zi(idno+ii-1)) i1 = ii
            if (zi(ldnm+1) .eq. zi(idno+ii-1)) i2 = ii
39      continue
        r1 = rigma(3*in+1)
        r2 = rigma(3*in+2)
        r3 = rigma(3*in+3)
!
        rigto(3*(im-1)+1) = r1 + rigto(3*(im-1)+1)
        rigto(3*(im-1)+2) = r2 + rigto(3*(im-1)+2)
        rigto(3*(im-1)+3) = r3 + rigto(3*(im-1)+3)
!
        r1 = rigto(3*(im-1)+1)
        r2 = rigto(3*(im-1)+2)
        r3 = rigto(3*(im-1)+3)
!
        rigma(3*in+1) = r1
        rigma(3*in+2) = r2
        rigma(3*in+3) = r3
        rigma2(3*(i1-1)+1) = r1 + rigma2(3*(i1-1)+1)
        rigma2(3*(i1-1)+2) = r2 + rigma2(3*(i1-1)+2)
        rigma2(3*(i1-1)+3) = r3 + rigma2(3*(i1-1)+3)
        rigma2(3*(i2-1)+1) = r1 + rigma2(3*(i2-1)+1)
        rigma2(3*(i2-1)+2) = r2 + rigma2(3*(i2-1)+2)
        rigma2(3*(i2-1)+3) = r3 + rigma2(3*(i2-1)+3)
        call jenuno(jexnum(mlgnma, im), nommai)
        write(ifr,1000) nommai,-r1,-r2,-r3
35  end do
!
    1000 format(2x,'_F ( MAILLE=''',A8,''',',1x,'CARA= ''K_T_D_L'' , ',&
     &      /7x,'VALE=(',1x,3(1x,1pe12.5,','),1x,'),',&
     &      /'   ),')
!
    call jedetr('&&RIGMI2.PARNO')
    call jedetr('&&RIGMI2.NOEUD')
    call jedetr('&&RIGMI2.SOMTOT')
    call jedetr('&&RIGMI2.SOMPAR')
!
    call jedema()
end subroutine
