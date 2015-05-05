function diadap(sddisc, i_adapt)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utdidt.h"
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
! person_in_charge: samuel.geniaut at edf.fr
!
    aster_logical :: diadap
    integer, intent(in) :: i_adapt
    character(len=19), intent(in) :: sddisc
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! ACCES AU DECLENCHEUR DE L'ADAPTATION DU PAS DE TEMPS
!
! --------------------------------------------------------------------------------------------------
!
! In  sddisc           : datastructure for time discretization
! IN  I_ADAPT : NUMERO DE LA METHODE D ADAPTATION TRAITEE
! OUT DIADAP : .TRUE. SI ON DOIT ADAPTER LE PAS DE TEMPS
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbinse, nbok
    character(len=19) :: event_name
!
! --------------------------------------------------------------------------------------------------
!
!
! - Event name
!
    call utdidt('L', sddisc, 'ADAP', 'NOM_EVEN', index_= i_adapt,&
                valk_ = event_name)
!
    if (event_name .eq. 'AUCUN') then
        diadap = .false.
    else if (event_name.eq.'TOUT_INST') then
        diadap = .true.
    else if (event_name.eq.'SEUIL_SANS_FORMU') then
!
! ----- RECUP DU SEUIL SUR LE NB DE SUCCES CONSECUTIFS
!
        call utdidt('L', sddisc, 'ADAP', 'NB_INCR_SEUIL', index_= i_adapt,&
                    vali_ = nbinse)
!
! ----- RECUP DU NB DE SUCCES CONSECUTIFS
!
        call utdidt('L', sddisc, 'ADAP', 'NB_EVEN_OK', index_= i_adapt,&
                    vali_ = nbok)
        if (nbok .lt. nbinse) then
            diadap = .false.
        else
!         ICI NBOK EST NORMALEMENT EGAL A NBINSE
!         MAIS NBOK PEUT ETRE AUSSI SUPERIEUR A NBINSE SI ON UTILISE
!         LA METHODE CONTINUE
            diadap = .true.
!         REMISE A ZERO DE NBOK
            nbok = 0
            call utdidt('E', sddisc, 'ADAP', 'NB_EVEN_OK', index_= i_adapt,&
                        vali_ = nbok)
        endif
    else if (event_name.eq.'SEUIL_AVEC_FORMU') then
        ASSERT(.false.)
    endif
!
end function
