subroutine memame(option , model_    , mate_, cara_elem_, time,&
                  compor_, matr_elem_, base)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/dbgcal.h"
#include "asterfort/dismoi.h"
#include "asterfort/detrsd.h"
#include "asterfort/exixfe.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecham.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
#include "asterfort/redetr.h"
#include "asterfort/vrcins.h"
#include "asterfort/xajcin.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: option
    character(len=*), intent(in) :: model_
    character(len=*), intent(in) :: mate_
    character(len=*), intent(in) :: cara_elem_
    real(kind=8), intent(in) :: time
    character(len=*), intent(in) :: compor_
    character(len=*), intent(in) :: matr_elem_
    character(len=1), intent(in) :: base
!
! --------------------------------------------------------------------------------------------------
!
! Compute MASS_* elementary matrix
!
! --------------------------------------------------------------------------------------------------
!
! In  option           : name of option
! In  model            : name of the model
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  time             : current time
! In  base             : JEVEUX base to create matr_elem
! In  matr_elem        : name of matr_elem result
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_in_maxi = 30
    integer, parameter :: nbout = 2
    character(len=8) :: lpain(nb_in_maxi), lpaout(nbout)
    character(len=19) :: lchin(nb_in_maxi), lchout(nbout)
    character(len=2) :: codret
    character(len=19) :: chvarc, matr_elem
    character(len=24) :: ligrmo
    character(len=24), pointer :: v_matr_rerr(:) => null()
    character(len=24) :: chgeom, chcara(18), chharm
    integer :: nbout2, nbin, nh, iret, icode, nb_subs_stat
    aster_logical :: l_xfem
    character(len=24) :: cara_elem, mate
    character(len=8) :: model
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    model     = model_
    cara_elem = cara_elem_
    mate      = mate_
    matr_elem = matr_elem_
    nh        = 0
    ligrmo    = model(1:8)//'.MODELE'
    chvarc    = '&&MEMAME.VARC'
    call exixfe(model, iret)
    l_xfem = (iret .ne. 0)
    call dismoi('NB_SS_ACTI', model, 'MODELE', repi=nb_subs_stat)
!
! - Init fields
!
    call inical(nb_in_maxi, lpain, lchin, nbout, lpaout,&
                lchout    )   
!
! - Create fields (geometry, elem. characteristics, Fourier)
!
    call mecham(option, model , cara_elem, nh, chgeom,&
                chcara, chharm, icode)
!
! - Construct command variables fields
!
    call vrcins(model , mate, cara_elem, time, chvarc,&
                codret)
!
! - Prepare MATR_ELEM
!
    call jeexin(matr_elem(1:19)//'.RELR', iret)
    if (iret .eq. 0) then
        call memare(base, matr_elem, model, mate, cara_elem, option)
    else
        call jedetr(matr_elem(1:19)//'.RELR')
    endif
    call jeveuo(matr_elem(1:19)//'.RERR', 'E', vk24 = v_matr_rerr)
    if (nb_subs_stat .gt. 0) then
        v_matr_rerr(3) = 'OUI_SOUS_STRUC'
    endif
    if (icode .eq. 1) goto 10
!
! - Input fields
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom(1:19)
    lpain(2) = 'PMATERC'
    lchin(2) = mate(1:19)
    lpain(3) = 'PCAORIE'
    lchin(3) = chcara(1)(1:19)
    lpain(4) = 'PCADISM'
    lchin(4) = chcara(3)(1:19)
    lpain(5) = 'PCAGNPO'
    lchin(5) = chcara(6)(1:19)
    lpain(6) = 'PCACOQU'
    lchin(6) = chcara(7)(1:19)
    lpain(7) = 'PCASECT'
    lchin(7) = chcara(8)(1:19)
    lpain(8) = 'PVARCPR'
    lchin(8) = chvarc(1:19)
    lpain(9) = 'PCAARPO'
    lchin(9) = chcara(9)(1:19)
    lpain(10) = 'PCACABL'
    lchin(10) = chcara(10)(1:19)
    lpain(11) = 'PCAGEPO'
    lchin(11) = chcara(5)(1:19)
    lpain(12) = 'PABSCUR'
    lchin(12) = chgeom(1:8)//'.ABSC_CURV'
    lpain(13) = 'PCAGNBA'
    lchin(13) = chcara(11)(1:19)
    lpain(14) = 'PCAPOUF'
    lchin(14) = chcara(13)(1:19)
    lpain(15) = 'PCOMPOR'
    lchin(15) = compor_(1:19)
    lpain(16) = 'PNBSP_I'
    lchin(16) = chcara(16)(1:19)
    lpain(17) = 'PFIBRES'
    lchin(17) = chcara(17)(1:19)
    lpain(18) = 'PCINFDI'
    lchin(18) = chcara(15)(1:19)
    nbin      = 18
    if (l_xfem) then
        call xajcin(model, option, nb_in_maxi, lchin, lpain,&
                    nbin)
    endif
!
! - Output fields
!
    lpaout(1) = 'PMATUUR'
    lchout(1) = matr_elem(1:15)//'.M01'
    lpaout(2) = 'PMATUNS'
    lchout(2) = matr_elem(1:15)//'.M02'
    if (option .eq. 'MASS_MECA') then
        nbout2 = 2
    else
        nbout2 = 1
    endif
!
! - Compute
!
    call calcul('S', option, ligrmo, nbin, lchin,&
                lpain, nbout2, lchout, lpaout, base,&
                'OUI')
!
! - Add RESU_ELEM in MATR_ELEM
!
    call reajre(matr_elem_, lchout(1), base)
    if (nbout2 .eq. 2) then
        call reajre(matr_elem_, lchout(2), base)
    endif
!
 10 continue
!
! - Cleaning
!
    call redetr(matr_elem_)
    call detrsd('CHAMP_GD', chvarc)
!
    call jedema()
end subroutine
