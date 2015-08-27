subroutine nmrefe(model  , compor, mate  , cara_elem, nume_dof,&
                  ds_conv, valinc, veelem, veasse)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assmiv.h"
#include "asterfort/calcul.h"
#include "asterfort/dbgcal.h"
#include "asterfort/detrsd.h"
#include "asterfort/exixfe.h"
#include "asterfort/inical.h"
#include "asterfort/mecact.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/nmchex.h"
#include "asterfort/reajre.h"
#include "asterfort/xajcin.h"
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: compor
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(in) :: nume_dof
    type(NL_DS_Conv), intent(in) :: ds_conv
    character(len=19), intent(in) :: valinc(*)
    character(len=19), intent(in) :: veelem(*)
    character(len=19), intent(in) :: veasse(*)
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Computation
!
! Compute reference vector for RESI_REFE_RELA
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  compor           : name of comportment definition (field)
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  nume_dof         : name of numbering (NUME_DDL)
! In  ds_conv          : datastructure for convergence management
! In  valinc           : hat variable for algorithm fields
! In  veelem           : hat variable for elementary vectors
! In  veasse           : hat variable for vectors
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nbout = 1
    integer, parameter :: nb_in_maxi = 26
    character(len=8) :: lpaout(nbout), lpain(nb_in_maxi)
    character(len=19) :: lchout(nbout), lchin(nb_in_maxi)
!
    character(len=19) :: vect_elem, vect_asse, disp_prev
    character(len=19) :: ligrmo, resu_elem, chrefe
    character(len=24) :: chgeom, chcara(18)
    integer :: i_refe, nb_refe, nb_in_prep, ier
    character(len=8), pointer :: list_cmp(:) => null()
    real(kind=8), pointer :: list_vale(:) => null()
    character(len=16) :: option
    aster_logical :: l_xfem
!
! --------------------------------------------------------------------------------------------------
!
    chrefe  = '&&NMREFE.SIGERE'
    resu_elem = '&&NMREFE.VEREFE'
    ligrmo = model(1:8) // '.MODELE'
    option = 'REFE_FORC_NODA'
    call exixfe(model, ier)
    l_xfem = ier.ne.0
!
! - Get names of fields
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', disp_prev)
    call nmchex(veelem, 'VEELEM', 'CNREFE', vect_elem)
    call nmchex(veasse, 'VEASSE', 'CNREFE', vect_asse)
!
! - Get parameters from convergence datastructure
!
    nb_refe       = ds_conv%nb_refe
    AS_ALLOCATE(vk8 = list_cmp, size = nb_refe)
    AS_ALLOCATE(vr  = list_vale, size = nb_refe)
    do i_refe = 1, nb_refe
        list_cmp(i_refe)  = ds_conv%list_refe(i_refe)%cmp_name
        list_vale(i_refe) = ds_conv%list_refe(i_refe)%user_para
    end do
!
! - Init fields
!
    call inical(nb_in_maxi, lpain, lchin, nbout, lpaout,&
                lchout)
!
! - Field for reference values
!
    call mecact('V', chrefe, 'MODELE', ligrmo, 'PREC',&
                ncmp=nb_refe, lnomcmp=list_cmp, vr=list_vale)
!
! - Geometry field
!
    call megeom(model, chgeom)
!
! - Elementary characteristics fields
!
    call mecara(cara_elem, chcara)
!
! - Preparation of VECT_ELEM
!
    call detrsd('VECT_ELEM', vect_elem)
    call memare('V', vect_elem, model, ' ', ' ', 'CHAR_MECA')
!
! - Input fields
!
    lpain(1)  = 'PGEOMER'
    lchin(1)  = chgeom(1:19)
    lpain(2)  = 'PREFCO'
    lchin(2)  = chrefe
    lpain(3)  = 'PCAORIE'
    lchin(3)  = chcara(1)(1:19)
    lpain(4)  = 'PCOMPOR'
    lchin(4)  = compor(1:19)
    lpain(5)  = 'PMATERC'
    lchin(5)  = mate(1:19)
    lpain(6)  = 'PDEPLMR'
    lchin(6)  = disp_prev
    lpain(7)  = 'PCACOQU'
    lchin(7)  = chcara(7)(1:19)
    lpain(8)  = 'PCAGEPO'
    lchin(8)  = chcara(5)(1:19)
    lpain(9)  = 'PNBSP_I'
    lchin(9)  = chcara(1) (1:8)//'.CANBSP'
    lpain(10) = 'PCAMASS'
    lchin(10) = chcara(12) (1:19)
    lpain(11) = 'PCAGNBA'
    lchin(11) = chcara(11)(1:19)
    lpain(12) = 'PCINFDI'
    lchin(12) = chcara(15)(1:19)
    nb_in_prep = 12
!
! - XFEM fields
!
    if (l_xfem) then
        call xajcin(model, 'REFE_FORC_NODA', nb_in_maxi, lchin, lpain,&
                    nb_in_prep)
    endif
!
! - Output fields
!
    lpaout(1) = 'PVECTUR'
    lchout(1) = resu_elem
!
! - Computation
!
    call calcul('S', option, ligrmo , nb_in_prep, lchin,&
                lpain, nbout, lchout, lpaout, 'V',&
                'OUI')
!
! - Copying output field
!
    call reajre(vect_elem, lchout(1), 'V')
!
! - Assembly
!
    call assmiv('V', vect_asse, 1, vect_elem, [1.d0],&
                nume_dof, ' ', 'ZERO', 1)
!
    AS_DEALLOCATE(vk8 = list_cmp)
    AS_DEALLOCATE(vr  = list_vale)
!
end subroutine
