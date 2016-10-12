subroutine load_neum_evcd(stop      , inst_curr , load_name, i_load  , ligrel_calc,&
                          nb_in_maxi, nb_in_prep, lpain    , lchin   , base       ,&
                          resu_elem , vect_elem)
!
implicit none
!
#include "asterfort/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsinch.h"
#include "asterfort/utmess.h"
#include "asterfort/copisd.h"
#include "asterfort/exisd.h"
#include "asterfort/reajre.h"
#include "asterfort/gcnco2.h"
#include "asterfort/corich.h"
#include "asterfort/load_neum_comp.h"
#include "asterfort/load_neum_prep.h"
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
! person_in_charge: jacques.pellet at edf.fr
!
    character(len=1), intent(in) :: stop
    real(kind=8), intent(in) :: inst_curr
    character(len=8), intent(in) :: load_name
    integer, intent(in) :: i_load
    character(len=19), intent(in) :: ligrel_calc
    integer, intent(in) :: nb_in_maxi
    character(len=*), intent(inout) :: lpain(nb_in_maxi)
    character(len=*), intent(inout) :: lchin(nb_in_maxi)
    integer, intent(in) :: nb_in_prep
    character(len=19), intent(inout) :: resu_elem
    character(len=19), intent(in) :: vect_elem
    character(len=1), intent(in) :: base
!
! --------------------------------------------------------------------------------------------------
!
! Compute Neumann loads
! 
! EVOL_CHAR - Dead and fixed loads
!
! --------------------------------------------------------------------------------------------------
!
! In  stop           : COMPORTEMENT DE CALCUL
! In  inst_curr      : current time
! In  i_load       : index of current load
! In  load_name      : name of current load
! In  ligrel_calc    : LIGREL to compute
! In  nb_in_maxi     : maximum number of input fields
! In  nb_in_prep     : number of input fields before specific ones
! IO  lpain          : list of input parameters
! IO  lchin          : list of input fields
! IO  resu_elem      : name of resu_elem
! In  vect_elem      : name of vect_elem
! In  base           : JEVEUX base to create vect_elem
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ier, nb_cham, iexist, ibid
    integer :: load_nume_evol
    character(len=8) :: evol_char, newnom
    character(len=16) :: type_sd, option
    character(len=19) :: load_name_evol, iden_direct
    character(len=4) :: load_type
    character(len=24) :: object
    character(len=8), pointer :: p_object(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    load_name_evol = '&&NMDEPR'
!
! - Only scalar loadings, dead and fixed loads
!
    load_nume_evol = 1
    load_type      = 'Dead'
!
! - Get evol_char
!
    object = load_name//'.CHME.EVOL.CHAR'
    call jeexin(object, ier)
    if (ier .eq. 0) then
        goto 99
    endif
    call jeveuo(object, 'L', vk8 = p_object)
    evol_char = p_object(1)
!
! - Check
!
    call dismoi('NB_CHAMP_UTI', evol_char, 'RESULTAT', repi=nb_cham)
    ASSERT(nb_cham.gt.0)
    call gettco(evol_char, type_sd)
    ASSERT(type_sd .eq. 'EVOL_CHAR')
!
! - Get volumic forces (CHAR_MECA_FR2D2D / CHAR_MECA_FR3D3D)
!
    option = ' '
    call rsinch(evol_char, 'FVOL_3D', 'INST', inst_curr, load_name_evol,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        option = 'CHAR_MECA_FR3D3D'
        goto 10
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        call utmess('F', 'CHARGES3_2', sk=evol_char, sr=inst_curr)
    endif
    call rsinch(evol_char, 'FVOL_2D', 'INST', inst_curr, load_name_evol,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        option = 'CHAR_MECA_FR2D2D'
        goto 10
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        call utmess('F', 'CHARGES3_3', sk=evol_char, sr=inst_curr)
    endif
 10 continue
!
! - Compute volumic forces (CHAR_MECA_FR2D2D / CHAR_MECA_FR3D3D)
!
    if (option .eq. 'CHAR_MECA_FR3D3D' .or. option .eq. 'CHAR_MECA_FR2D2D') then
        if (option .eq. 'CHAR_MECA_FR3D3D') then
            iden_direct = '.F3D3D'
        endif
        if (option .eq. 'CHAR_MECA_FR2D2D') then
            iden_direct = '.F2D2D'
        endif
        call load_neum_comp(stop       , i_load    , load_name , load_nume_evol, load_type,&
                            ligrel_calc, nb_in_maxi, nb_in_prep, lpain         , lchin    ,&
                            base       , resu_elem , vect_elem , iden_direct = iden_direct,&
                            name_inputz = load_name_evol)
    endif
!
! - Get surfacic forces (CHAR_MECA_FR2D3D / CHAR_MECA_FR1D2D)
!
    call rsinch(evol_char, 'FSUR_3D', 'INST', inst_curr, load_name_evol,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        if (option .eq. 'CHAR_MECA_FR2D2D') then
            call utmess('F', 'CHARGES3_4', sk=evol_char, sr=inst_curr)
        endif
        option = 'CHAR_MECA_FR2D3D'
        goto 20
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        call utmess('F', 'CHARGES3_5', sk=evol_char, sr=inst_curr)
    endif
!
    call rsinch(evol_char, 'FSUR_2D', 'INST', inst_curr, load_name_evol,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        if (option .eq. 'CHAR_MECA_FR3D3D') then
            call utmess('F', 'CHARGES3_6', sk=evol_char, sr=inst_curr)
        endif
        option = 'CHAR_MECA_FR1D2D'
        goto 20
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        call utmess('F', 'CHARGES3_7', sk=evol_char, sr=inst_curr)
    endif
 20 continue
!
! - Compute surfacic forces (CHAR_MECA_FR2D3D / CHAR_MECA_FR1D2D)
!
    if (option .eq. 'CHAR_MECA_FR2D3D' .or. option .eq. 'CHAR_MECA_FR1D2D') then
        if (option .eq. 'CHAR_MECA_FR2D3D') then
            iden_direct = '.F2D3D'
        endif
        if (option .eq. 'CHAR_MECA_FR1D2D') then
            iden_direct = '.F1D2D'
        endif
        call load_neum_comp(stop       , i_load    , load_name , load_nume_evol, load_type,&
                            ligrel_calc, nb_in_maxi, nb_in_prep, lpain         , lchin    ,&
                            base       , resu_elem , vect_elem , iden_direct = iden_direct,&
                            name_inputz = load_name_evol)
    endif
!
! - Get pressure (CHAR_MECA_PRES_R)
!
    call rsinch(evol_char, 'PRES', 'INST', inst_curr, load_name_evol,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        option = 'CHAR_MECA_PRES_R'
        goto 30
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        call utmess('F', 'CHARGES3_8', sk=evol_char, sr=inst_curr)
    endif
 30 continue
!
! - Compute pressure (CHAR_MECA_PRES_R)
!
    if (option .eq. 'CHAR_MECA_PRES_R') then
        iden_direct = '.PRESS'
        call load_neum_comp(stop       , i_load    , load_name , load_nume_evol, load_type,&
                            ligrel_calc, nb_in_maxi, nb_in_prep, lpain         , lchin    ,&
                            base       , resu_elem , vect_elem , iden_direct = iden_direct,&
                            name_inputz = load_name_evol)
    endif
!
! - Get nodal force (VECT_ASSE)
!
    call rsinch(evol_char, 'FORC_NODA', 'INST', inst_curr, load_name_evol,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        option = 'Copy_Load'
        goto 40
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        call utmess('F', 'CHARGES3_8', sk=evol_char, sr=inst_curr)
    endif
 40 continue
!
! - Compute nodal force (VECT_ASSE)
!
    if (option .eq. 'Copy_Load') then
        newnom = resu_elem(10:16)
        call gcnco2(newnom)
        resu_elem(10:16) = newnom(2:8)
        call corich('E', resu_elem, i_load, ibid)
        call copisd('CHAMP_GD', base, load_name_evol, resu_elem)
        call exisd('CHAMP_GD', resu_elem, iexist)
        ASSERT((iexist.gt.0).or.(stop.eq.'C'))
        call reajre(vect_elem, resu_elem, base)
    endif
!
 99 continue
!
    call jedema()
end subroutine
