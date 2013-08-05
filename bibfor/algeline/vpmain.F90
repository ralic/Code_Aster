subroutine vpmain(modele, mate, cara, xmastr, nbpara)
! aslint: disable=
    implicit   none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/pemica.h"
#include "asterfort/rcmfmc.h"
    character(len=*) :: modele, mate, cara
    real(kind=8) :: xmastr
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
!     OPERATEUR   NORM_MODE
!     CALCUL DE LA MASSE DU MODELE
!     ------------------------------------------------------------------
!
    integer :: mxvale, ibid, nb, i, iorig, icage, nbpara
    parameter (mxvale=16)
    real(kind=8) :: rbi3(3), zmas(mxvale)
    character(len=8) :: lpain(15), lpaout(5)
    character(len=19) :: chelem
    character(len=24) :: lchin(15), lchout(1), mateco
    character(len=24) :: chgeom, chcara(18), ligrmo, compor
    logical :: lret
!
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call megeom(modele, chgeom)
    ASSERT(chgeom.ne.' ')
!
    call mecara(cara, lret, chcara)
!
    ligrmo = modele(1:8)//'.MODELE'
!
    if (mate .ne. '        ') then
        call rcmfmc(mate, mateco)
    else
        mateco = ' '
    endif
!     POUR LES MULTIFIBRES ON SE SERT DE COMPOR
    compor = mate(1:8)//'.COMPOR'
!
!     --- CALCUL DE L'OPTION ---
    chelem = '&&PEMAIN.MASS_INER'
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PMATERC'
    lchin(2) = mateco
    lpain(3) = 'PCAORIE'
    lchin(3) = chcara(1)
    lpain(4) = 'PCADISM'
    lchin(4) = chcara(3)
    lpain(5) = 'PCAGNPO'
    lchin(5) = chcara(6)
    lpain(6) = 'PCACOQU'
    lchin(6) = chcara(7)
    lpain(7) = 'PCASECT'
    lchin(7) = chcara(8)
    lpain(8) = 'PCAARPO'
    lchin(8) = chcara(9)
    lpain(9) = 'PCAGNBA'
    lchin(9) = chcara(11)
    lpain(10) = 'PCAGEPO'
    lchin(10) = chcara(5)
    lpain(11) = 'PNBSP_I'
    lchin(11) = chcara(16)
    lpain(12) = 'PFIBRES'
    lchin(12) = chcara(17)
    lpain(13) = 'PCOMPOR'
    lchin(13) = compor
    lpain(14) = 'PCINFDI'
    lchin(14) = chcara(15)
    nb = 14
    lpaout(1) = 'PMASSINE'
    lchout(1) = chelem
!
    call calcul('S', 'MASS_INER', ligrmo, nb, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
    do 33 i = 1, mxvale
        zmas(i)=0.d0
33  end do
!
    iorig=0
    icage=0
!
    call pemica(chelem, mxvale, zmas, 0, ibid,&
                rbi3, iorig, icage)
    xmastr=zmas(1)
!
    call detrsd('CHAM_ELEM', '&&PEMAIN.MASS_INER')
!
    call jedema()
end subroutine
