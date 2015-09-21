function ischar_iden(v_load_info, i_load, nb_load, load_type_1, load_type_2)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
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
    aster_logical :: ischar_iden
    integer, intent(in), pointer :: v_load_info(:)
    integer, intent(in) :: i_load
    integer, intent(in) :: nb_load
    character(len=4), intent(in) :: load_type_1
    character(len=4), intent(in) :: load_type_2
!
! --------------------------------------------------------------------------------------------------
!
! List of loads - Utility
!
! Return type of load - Identification
!
! --------------------------------------------------------------------------------------------------
!
! In  v_load_info    : vector of loads info
! In  i_load         : index in list of loads
! In  nb_load        : total number of loads
! In  load_type_1    : first level of type
!                'DIRI' - DIRICHLET
!                'NEUM' - NEUMANN
! In  load_type_2    : second level of type
! -> For Dirichlet loads
!                'DUAL' - AFFE_CHAR_MECA
!                'ELIM' - AFFE_CHAR_CINE
!                'DIDI' - Differential
!                'SUIV' - Undead load
!                '    ' - All types
! -> For Neumann loads
!                'ONDE' - ONDE PLANE
!                'SIGM' - SIGMA_INTERNE
!                'LAPL' - FORCE DE LAPLACE
!                'TARD' - ELEMENTS TARDIFS
!                'SUIV' - Undead load
!                '    ' - All types
!
! --------------------------------------------------------------------------------------------------
!
    integer :: load_nume_diri, load_nume_neum
    aster_logical :: ldiri, lelim, ldual, ldidi, lneum
    aster_logical :: londe, llapl, lsigm, lelem, lsuiv
!
! --------------------------------------------------------------------------------------------------
!
    ischar_iden = .false.
    lelim  = .false.
    ldual  = .false.
    ldiri  = .false.
    ldidi  = .false.
    lneum  = .false.
    londe  = .false.
    llapl  = .false.
    lsigm  = .false.
    lsuiv  = .false.
    lelem  = .false.
!
    load_nume_diri = v_load_info(i_load+1)
    load_nume_neum = v_load_info(i_load+nb_load+1)
    if (load_nume_diri .eq. -1) then
        ldiri = .true.
        lelim = .true.
    else if (load_nume_diri.eq.-2) then
        ldiri = .true.
        lelim = .true.
    else if (load_nume_diri.eq.-3) then
        ldiri = .true.
        lelim = .true.
    else if (load_nume_diri.eq.1) then
        ldiri = .true.
        ldual = .true.
    else if (load_nume_diri.eq.2) then
        ldiri = .true.
        ldual = .true.
    else if (load_nume_diri.eq.3) then
        ldiri = .true.
        ldual = .true.
    else if (load_nume_diri.eq.4) then
        ldiri = .true.
        ldual = .true.
        lsuiv = .true.
    else if (load_nume_diri.eq.5) then
        ldiri = .true.
        ldual = .true.
    else if (load_nume_diri.eq.6) then
        ldiri = .true.
        ldual = .true.
    else if (load_nume_diri.eq.0) then
        if (load_nume_neum .eq. 1) then
            lneum = .true.
        else if (load_nume_neum.eq.2) then
            lneum = .true.
        else if (load_nume_neum.eq.3) then
            lneum = .true.
        else if (load_nume_neum.eq.4) then
            lneum = .true.
            lsuiv = .true.
        else if (load_nume_neum.eq.5) then
            lneum = .true.
        else if (load_nume_neum.eq.6) then
            lneum = .true.
            londe = .true.
        else if (load_nume_neum.eq.55) then
            lneum = .true.
            lsigm = .true.
        else if (load_nume_neum.eq.10) then
            lneum = .true.
            lelem = .true.
        else if (load_nume_neum.eq.20) then
            lneum = .true.
        else if (load_nume_neum.eq.0) then
            if (v_load_info(2*nb_load+3) .ne. 0) then
                lneum = .true.
                llapl = .true.
            endif
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
    if (ldiri) then
        if (v_load_info(3*nb_load+i_load+3) .eq. 1) then
            ldidi = .true.
        endif
    endif
    if (load_type_1 .eq. 'DIRI') then
        if (ldiri) then
            if (load_type_2 .eq. 'DUAL') then
                ischar_iden = ldual
            else if (load_type_2.eq.'ELIM') then
                ischar_iden = lelim
            else if (load_type_2.eq.'DIDI') then
                ischar_iden = ldidi
            else if (load_type_2.eq.'SUIV') then
                ischar_iden = lsuiv
            else if (load_type_2.eq.'    ') then
                ischar_iden = ldiri
            else
                write(6,*) 'SOUTYP: ',load_type_2
                ASSERT(.false.)
            endif
        else if (lneum) then
            ischar_iden = .false.
        endif
    else if (load_type_1.eq.'NEUM') then
        if (lneum) then
            if (load_type_2 .eq. 'ONDE') then
                ischar_iden = londe
            else if (load_type_2.eq.'SIGM') then
                ischar_iden = lsigm
            else if (load_type_2.eq.'LAPL') then
                ischar_iden = llapl
            else if (load_type_2.eq.'TARD') then
                ischar_iden = lelem
            else if (load_type_2.eq.'SUIV') then
                ischar_iden = lsuiv
            else if (load_type_2.eq.'    ') then
                ischar_iden = lneum
            else
                write(6,*) 'SOUTYP: ',load_type_2
                ASSERT(.false.)
            endif
        else if (ldiri) then
            ischar_iden = .false.
        endif
    else
        write(6,*) 'TYPCHA: ',load_type_1
        ASSERT(.false.)
    endif
end function
