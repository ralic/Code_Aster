subroutine calcCalcMeca(nb_option   , list_option    , &
                        list_load   , model          , mate       , cara_elem,& 
                        l_elem_nonl , ds_constitutive, varc_refe  ,&
                        hval_incr   , hval_algo      ,&
                        merigi      , vediri         , vefint     , veforc,&
                        vevarc_prev , vevarc_curr    , nume_harm  ,&
                        nb_obje_maxi, obje_name      , obje_sdname, nb_obje)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/exixfe.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/knindi.h"
#include "asterfort/nmchex.h"
#include "asterfort/merimo.h"
#include "asterfort/medime.h"
#include "asterfort/vebtla.h"
#include "asterfort/copisd.h"
#include "asterfort/vefnme.h"
#include "asterfort/nmvcpr.h"
#include "asterfort/utmess.h"
#include "asterfort/nmvcd2.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1504
!
    integer, intent(in) :: nb_option
    character(len=16), intent(in) :: list_option(:)
    character(len=19), intent(in) :: list_load
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    aster_logical, intent(in) :: l_elem_nonl
    type(NL_DS_Constitutive), intent(in) :: ds_constitutive
    character(len=24), intent(in) :: varc_refe
    character(len=19), intent(in) :: hval_incr(:)
    character(len=19), intent(in) :: hval_algo(:)
    character(len=19), intent(in) :: merigi
    character(len=19), intent(in) :: vediri
    character(len=19), intent(in) :: vefint
    character(len=19), intent(in) :: veforc
    character(len=19), intent(in) :: vevarc_prev
    character(len=19), intent(in) :: vevarc_curr
    integer, intent(in) :: nume_harm
    integer, intent(in) :: nb_obje_maxi
    character(len=16), intent(inout) :: obje_name(nb_obje_maxi)
    character(len=24), intent(inout) :: obje_sdname(nb_obje_maxi)
    integer, intent(out) ::  nb_obje
!
! --------------------------------------------------------------------------------------------------
!
! Command CALCUL
!
! Compute for mechanics
!
! --------------------------------------------------------------------------------------------------
!
! In  nb_option        : number of options to compute
! In  list_option      : list of options to compute
! In  list_load        : name of datastructure for list of loads
! In  model            : name of model
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  l_elem_nonl      : .true. if all elements can compute non-linear options
! In  ds_constitutive  : datastructure for constitutive laws management
! In  varc_refe        : name of reference command variables vector
! In  hval_incr        : hat-variable for incremental values fields
! In  hval_algo        : hat-variable for algorithms fields
! In  merigi           : name of elementary for tangent matrix
! In  vediri           : name of elementary for reaction (Lagrange) vector
! In  vefint           : name of elementary for internal forces vector (RAPH_MECA)
! In  veforc           : name of elementary for forces vector (FORC_NODA)
! In  vevarc_prev      : name of elementary for external state variables at beginning of step
! In  vevarc_curr      : name of elementary for external state variables at end of step
! In  nb_obje_maxi     : maximum number of new objects to add
! IO  obje_name        : name of new objects to add
! IO  obje_sdname      : datastructure name of new objects to add
! Out nb_obje          : number of new objects to add
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_matr, l_nonl, l_varc_prev, l_varc_curr, l_forc_noda
    aster_logical :: l_lagr
    character(len=16) :: option
    character(len=19) :: varc_curr, disp_curr, sigm_curr, vari_curr, k19bla = ' '
    character(len=19) :: vari_prev, disp_prev, sigm_prev
    integer :: iterat, ixfem, nb_subs_stat
    aster_logical :: tabret(0:10), l_meta_zirc, l_meta_acier, l_xfem, l_macr_elem
    integer :: fonact(100)
    real(kind=8) :: partps(3)
    character(len=19) :: vefori(2), ligrmo
!
! --------------------------------------------------------------------------------------------------
!
    fonact(:) = 0
    partps(:) = 0.d0
    nb_obje   = 0
!
! - Get LIGREL
!
    call dismoi('NOM_LIGREL', model, 'MODELE', repk=ligrmo)
!
! - Specific functionnalities
!
    call exixfe(model, ixfem)
    l_xfem      = ixfem .ne. 0
    call dismoi('NB_SS_ACTI', model, 'MODELE', repi=nb_subs_stat)
    l_macr_elem = nb_subs_stat .gt. 0
!
! - Name of variables
!
    call nmchex(hval_incr, 'VALINC', 'COMPLU', varc_curr)
    call nmchex(hval_incr, 'VALINC', 'DEPPLU', disp_curr)
    call nmchex(hval_incr, 'VALINC', 'SIGPLU', sigm_curr)
    call nmchex(hval_incr, 'VALINC', 'VARPLU', vari_curr)
    call nmchex(hval_incr, 'VALINC', 'DEPMOI', disp_prev)
    call nmchex(hval_incr, 'VALINC', 'SIGMOI', sigm_prev)
    call nmchex(hval_incr, 'VALINC', 'VARMOI', vari_prev)
!
! - What we are computing
!
    l_matr      = (knindi(16, 'MATR_TANG_ELEM', list_option, nb_option) .gt. 0)
    l_nonl      = (knindi(16, 'MATR_TANG_ELEM', list_option, nb_option) .gt. 0).or.&
                  (knindi(16, 'COMPORTEMENT'  , list_option, nb_option) .gt. 0).or.&
                  (knindi(16, 'FORC_INTE_ELEM', list_option, nb_option) .gt. 0)
    l_forc_noda = (knindi(16, 'FORC_NODA_ELEM'  , list_option, nb_option) .gt. 0)
    l_varc_prev = (knindi(16, 'FORC_VARC_ELEM_M', list_option, nb_option) .gt. 0)
    l_varc_curr = (knindi(16, 'FORC_VARC_ELEM_P', list_option, nb_option) .gt. 0)
    l_lagr      = l_matr
!
! - Some checks
!
    if (l_nonl) then
        if (.not.l_elem_nonl) then
            call utmess('F', 'CALCUL1_8')
        endif
        if (l_xfem) then
            call utmess('F', 'CALCUL1_10')
        endif
        if (l_macr_elem) then
            call utmess('F', 'CALCUL1_11')
        endif
        if (disp_prev .eq. ' ' .or. sigm_prev .eq. ' ' .or. vari_prev .eq. ' ') then
            call utmess('F', 'CALCUL1_12')
        endif
    endif
!
    if (l_forc_noda) then
        if (disp_prev .eq. ' ' .or. sigm_prev .eq. ' ') then
            call utmess('F', 'CALCUL1_13')
        endif
    endif
!
    if (l_varc_prev .or. l_varc_curr) then
        call nmvcd2('M_ZIRC' , mate, l_meta_zirc)
        call nmvcd2('M_ACIER', mate, l_meta_acier)
        if ((l_meta_zirc .or. l_meta_acier) .and. (.not.l_elem_nonl)) then
            call utmess('F', 'CALCUL1_9')
        endif
    endif
!
! - How we are computing
!
    option = ' '
    if (l_matr) then
        option = 'FULL_MECA'
    else
        option = 'RAPH_MECA'
    endif
!
! - Physical dof computation
!
    if (l_nonl) then
        iterat = 1
        call merimo('G', model, cara_elem, mate, varc_refe,&
                    ds_constitutive, iterat, fonact, k19bla,&
                    hval_incr, hval_algo, merigi, vefint, option,&
                    tabret)
    endif
!
! - Lagrange dof computation
!
    if (l_lagr) then
        call medime('G', 'CUMU', model, list_load, merigi)
        call vebtla('G', model, mate, cara_elem, disp_curr,&
                    list_load, vediri)
    endif
!
! - Nodal forces
!
    if (l_forc_noda) then
        option    = 'FORC_NODA'
        vefori(1) = veforc
        vefori(2) = ' '
        if (.not. l_nonl) then
            call copisd('CHAMP_GD', 'V', sigm_prev, sigm_curr)
        endif
        call vefnme(option, 'G', model, mate, cara_elem,&
                    ds_constitutive%compor, partps, 0, ligrmo, varc_curr,&
                    sigm_curr, ' ', disp_curr, ' ', vefori)
    endif
!
! - State variables
!
    if (l_varc_prev) then
        call nmvcpr(model, mate       , cara_elem, varc_refe     , ds_constitutive%compor   ,&
                    hval_incr, base_ = 'G', vect_elem_prev_ = vevarc_prev,&
                    nume_harm_ = nume_harm)
    endif
    if (l_varc_curr) then
        call nmvcpr(model, mate       , cara_elem, varc_refe     , ds_constitutive%compor   ,&
                    hval_incr, base_ = 'G', vect_elem_curr_ = vevarc_curr,&
                    nume_harm_ = nume_harm)
    endif
!
! - New objects in table
!
    nb_obje = 0
    if (l_lagr) then
        nb_obje = nb_obje + 1
        ASSERT(nb_obje.le.nb_obje_maxi)
        obje_name(nb_obje) = 'FORC_DIRI_ELEM'
        obje_sdname(nb_obje) = vediri
    endif
    if (l_nonl) then
        nb_obje = nb_obje + 1
        ASSERT(nb_obje.le.nb_obje_maxi)
        obje_name(nb_obje)   = 'FORC_INTE_ELEM'
        obje_sdname(nb_obje) = vefint
        nb_obje = nb_obje + 1
        ASSERT(nb_obje.le.nb_obje_maxi)
        obje_name(nb_obje)   = 'SIEF_ELGA'
        obje_sdname(nb_obje) = sigm_curr
        nb_obje = nb_obje + 1
        ASSERT(nb_obje.le.nb_obje_maxi)
        obje_name(nb_obje)   = 'VARI_ELGA'
        obje_sdname(nb_obje) = vari_curr
        nb_obje = nb_obje + 1
        ASSERT(nb_obje.le.nb_obje_maxi)
        obje_name(nb_obje)   = 'CODE_RETOUR_INTE'
        obje_sdname(nb_obje) = ds_constitutive%comp_error
        if (l_matr) then
            nb_obje = nb_obje + 1
            ASSERT(nb_obje.le.nb_obje_maxi)
            obje_name(nb_obje)   = 'MATR_TANG_ELEM'
            obje_sdname(nb_obje) = merigi
        endif
    endif
    if (l_forc_noda) then
        nb_obje = nb_obje + 1
        ASSERT(nb_obje.le.nb_obje_maxi)
        obje_name(nb_obje)   = 'FORC_NODA_ELEM'
        obje_sdname(nb_obje) = veforc
    endif
    if (l_varc_prev) then
        nb_obje = nb_obje + 1
        ASSERT(nb_obje.le.nb_obje_maxi)
        obje_name(nb_obje)   = 'FORC_VARC_ELEM_M'
        obje_sdname(nb_obje) = vevarc_prev
    endif
    if (l_varc_curr) then
        nb_obje = nb_obje + 1
        ASSERT(nb_obje.le.nb_obje_maxi)
        obje_name(nb_obje)   = 'FORC_VARC_ELEM_P'
        obje_sdname(nb_obje) = vevarc_curr
    endif
!
end subroutine
