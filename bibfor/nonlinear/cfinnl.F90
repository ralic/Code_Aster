subroutine cfinnl(ds_contact, l_pair, nbliac, llf,&
                  llf1      , llf2)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisl.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    aster_logical, intent(in) :: l_pair
    integer, intent(out) :: nbliac
    integer, intent(out) :: llf
    integer, intent(out) :: llf1
    integer, intent(out) :: llf2
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Discrete methods - Get total number of initial links
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  l_pair           : .true. if new pairing occurred
! OUT NBLIAC           : NOMBRE DE LIAISONS ACTIVES
! OUT LLF              : NOMBRE DE LIAISON DE FROTTEMENT (DEUX DIRECTIONS)
! OUT LLF1             : NOMBRE DE LIAISON DE FROTTEMENT (1ERE DIRECTION )
! OUT LLF2             : NOMBRE DE LIAISON DE FROTTEMENT (2EME DIRECTION )
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_pena_frot, l_lagr_cont
!
! --------------------------------------------------------------------------------------------------
!
    nbliac = 0
    llf    = 0
    llf1   = 0
    llf2   = 0
!
! - Get contact parameters
!
    l_pena_frot = cfdisl(ds_contact%sdcont_defi,'FROT_PENA')
    l_lagr_cont = cfdisl(ds_contact%sdcont_defi,'CONT_LAGR')
!
! - Keep old links if possible (no new pairing)
!
    if (l_lagr_cont) then
        if (l_pair) then
            nbliac = 0
            llf    = 0
            llf1   = 0
            llf2   = 0
        else
            nbliac = cfdisd(ds_contact%sdcont_solv,'NBLIAC' )
            if (l_pena_frot) then
                llf = 0
                llf1 = 0
                llf2 = 0
            else
                llf = cfdisd(ds_contact%sdcont_solv,'LLF' )
                llf1 = cfdisd(ds_contact%sdcont_solv,'LLF1' )
                llf2 = cfdisd(ds_contact%sdcont_solv,'LLF2' )
            endif
        endif
    else
        nbliac = 0
        llf    = 0
        llf1   = 0
        llf2   = 0
    endif
!
end subroutine
