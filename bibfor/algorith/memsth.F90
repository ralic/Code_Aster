subroutine memsth(model_    , cara_elem_, mate_, chtime, memass, base,&
                  varc_curr_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/exixfe.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
#include "asterfort/xajcin.h"
#include "asterfort/inical.h"
#include "asterfort/gcnco2.h"
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
!
    character(len=*), intent(in) :: model_
    character(len=*), intent(in) :: cara_elem_
    character(len=*), intent(in) :: mate_
    character(len=24), intent(in) :: chtime
    character(len=19), intent(in) :: memass
    character(len=1), intent(in) :: base
    character(len=19), optional, intent(in) :: varc_curr_
!
! --------------------------------------------------------------------------------------------------
!
! Thermic
! 
! Mass matrix
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of the model
! In  mate             : name of material characteristics (field)
! In  chtime           : time (<CARTE>)
! In  cara_elem        : name of elementary characteristics (field)
! In  varc_curr        : command variable for current time
! In  base             : JEVEUX base to create matr_elem
! In  matr_elem        : name of matr_elem result
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_in_maxi = 16
    integer, parameter :: nbout = 1
    character(len=8) :: lpain(nb_in_maxi), lpaout(nbout)
    character(len=19) :: lchin(nb_in_maxi), lchout(nbout)
    character(len=16) :: option
    character(len=24) :: ligrmo, cara_elem, mate
    character(len=19) :: resu_elem, varc_curr
    character(len=24) :: chgeom, chcara(18)
    integer :: iret, nbin
    aster_logical :: l_xfem
    character(len=8) :: newnom, model
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
    resu_elem = memass(1:8)//'.0000000'
    ligrmo    = model(1:8)//'.MODELE'
    option    = 'MASS_THER'
    call exixfe(model, iret)
    l_xfem = (iret .ne. 0)
    if (present(varc_curr_)) then
        varc_curr = varc_curr_
    else
        varc_curr = ' '
    endif
!
! - Prepare MATR_ELEM
!
    call jeexin(memass(1:19)//'.RELR', iret)
    if (iret .eq. 0) then
        call memare(base, memass, model, mate, cara_elem, 'MASS_THER')
    else
        call jedetr(memass(1:19)//'.RELR')
    endif
!
! - Generate new RESU_ELEM name
!
    newnom = resu_elem(10:16)
    call gcnco2(newnom)
    resu_elem(10:16) = newnom(2:8)
!
! - Init fields
!
    call inical(nb_in_maxi, lpain, lchin, nbout, lpaout,&
                lchout    )    
!
! - Geometry field
!
    call megeom(model, chgeom)
!
! - Elementary characteristics field
!
    call mecara(cara_elem, chcara)
!
! - Input fields
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom(1:19)
    lpain(2) = 'PMATERC'
    lchin(2) = mate(1:19)
    lpain(3) = 'PCACOQU'
    lchin(3) = chcara(7)(1:19)
    lpain(4) = 'PTEMPSR'
    lchin(4) = chtime(1:19)
    lpain(5) = 'PVARCPR'
    lchin(5) = varc_curr(1:19)
    nbin     = 7
    if (l_xfem) then
        call xajcin(model, option, nb_in_maxi, lchin, lpain,&
                    nbin)
    endif
!
! - Output fields
!
    lpaout(1) = 'PMATTTR'
    lchout(1) = resu_elem
!
! - Compute
!
    call calcul('S'  , option, ligrmo, nbin  , lchin,&
                lpain, nbout , lchout, lpaout, base ,&
                'OUI')
!
! - Add RESU_ELEM in MATR_ELEM
!
    call reajre(memass, lchout(1), base)
!
    call jedema()
end subroutine
