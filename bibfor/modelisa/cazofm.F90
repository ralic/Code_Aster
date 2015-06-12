subroutine cazofm(sdcont, keywf, cont_form, cont_nbzone)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/deprecated_algom.h"
#include "asterfort/assert.h"
#include "asterfort/cazouu.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
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
    character(len=8), intent(in) :: sdcont
    integer, intent(in) :: cont_form
    integer, intent(in) :: cont_nbzone
    character(len=16), intent(in) :: keywf
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Get method of contact
!
! --------------------------------------------------------------------------------------------------
!
! In  keywf       : factor keyword to read
! In  sdcont      : name of contact concept (DEFI_CONTACT)
! In  cont_form   : formulation of contact
! In  cont_nbzone : number of zones of contact
!
! --------------------------------------------------------------------------------------------------
!
    integer :: noc
    character(len=16) :: s_algo_cont, s_algo_frot, s_formul, s_frott
    integer :: algo_cont, algo_frot, izone
    aster_logical :: l_frot, lmunul
    character(len=24) :: sdcont_defi
    character(len=24) :: sdcont_paraci
    integer, pointer :: v_sdcont_paraci(:) => null()
    character(len=16) :: valk(3)
    real(kind=8) :: coefff
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    algo_cont   = 0
    algo_frot   = 0
    l_frot      = .false.
    s_formul    = ' '
    s_frott     = ' '
    s_algo_cont = ' '
    s_algo_frot = ' '
    if (cont_form.eq.1) then
        s_formul = 'DISCRETE'
    else if (cont_form.eq.2) then
        s_formul = 'CONTINUE'
    else if (cont_form.eq.3) then
        s_formul = 'XFEM'
    else
        ASSERT(.false.)
    endif
!
! - Datastructure for contact definition
!
    sdcont_defi   = sdcont(1:8)//'.CONTACT'
    sdcont_paraci = sdcont_defi(1:16)//'.PARACI'
    call jeveuo(sdcont_paraci, 'E', vi=v_sdcont_paraci)
!
! - Formulation
!
    v_sdcont_paraci(4) = cont_form
!
! - Friction ?
!
    call getvtx(' ', 'FROTTEMENT', scal=s_frott)
    l_frot = s_frott.eq.'COULOMB'
!
! - Get methods
!
    call getvtx(keywf, 'ALGO_CONT', iocc=1, scal=s_algo_cont)
    valk(1) = s_formul
    valk(2) = s_algo_cont
    if (l_frot) then
        call getvtx(keywf, 'ALGO_FROT', iocc=1, scal=s_algo_frot, nbret=noc)
        valk(3) = s_algo_frot
    endif
!
    if (cont_form .eq. 1) then
        call cazouu(keywf, cont_nbzone, 'ALGO_CONT')
        if (l_frot) then
            call cazouu(keywf, cont_nbzone, 'ALGO_FROT')
            if (s_algo_frot .eq. 'PENALISATION') then
                algo_frot = 1
                if (s_algo_cont .eq. 'PENALISATION') then
                    algo_cont = 4
                else if (s_algo_cont .eq. 'LAGRANGIEN') then
                    algo_cont = 5
                else
                    call utmess('F', 'CONTACT3_3', nk=3, valk=valk)
                endif
            else if (s_algo_frot .eq. 'LAGRANGIEN') then
                algo_frot = 2
                call deprecated_algom('LAGR_FROT')
                if (s_algo_cont .eq. 'LAGRANGIEN') then
                    algo_cont = 5
                else
                    call utmess('F', 'CONTACT3_3', nk=3, valk=valk)
                endif
            else
                ASSERT(.false.)
            endif
        else
            algo_frot = 0
            if (s_algo_cont .eq. 'GCP') then
                algo_cont = 2
            else if (s_algo_cont .eq. 'CONTRAINTE') then
                algo_cont = 1
            else if (s_algo_cont .eq. 'PENALISATION') then
                algo_cont = 4
            else if (s_algo_cont .eq. 'LAGRANGIEN') then
                algo_cont = 5
                call deprecated_algom('LAGR_CONT')
            else
                ASSERT(.false.)
            endif
        endif
    else if (cont_form.eq.2) then
        algo_cont = 6
        if (l_frot) then
            lmunul = .false.
            do izone = 1, cont_nbzone
                call getvr8(keywf, 'COULOMB', iocc=izone, scal=coefff)
                lmunul = lmunul.or.(coefff.ne.0.d0)
            end do
            if (.not.lmunul) then
                call utmess('A', 'CONTACT3_1')
                l_frot = .false.
            endif
        endif
        if (l_frot) then
            algo_frot = 6
        else
            algo_frot = 0
        endif
    else if (cont_form.eq.3) then
        algo_cont = 7
        if (l_frot) then
            algo_frot = 7
        else
            algo_frot = 0
        endif
    else
        ASSERT(.false.)
    endif
!
! - Save methods
!
    v_sdcont_paraci(17) = algo_cont
    v_sdcont_paraci(18) = algo_frot
!
    call jedema()
!
end subroutine
