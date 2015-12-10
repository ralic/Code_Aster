subroutine nmevin(sddisc, ds_contact, i_echec, i_echec_acti)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfdisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(in) :: sddisc
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(in) :: i_echec
    integer, intent(out) :: i_echec_acti
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - EVENEMENTS)
!
! GESTION DE L'EVENEMENT INTERPENETRATION
!
! ----------------------------------------------------------------------
!
! In  sddisc           : datastructure for time discretization TEMPORELLE
! In  ds_contact       : datastructure for contact management
! IN  IECHEC : OCCURRENCE DE L'ECHEC
! OUT IEVDAC : VAUT IECHEC SI EVENEMENT DECLENCHE
!                   0 SINON
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nbliai
    integer :: iliai
    character(len=24) :: jeuite
    integer :: jjeuit
    real(kind=8) :: jeufin, pnmaxi
    aster_logical :: levent
    real(kind=8) :: pene_maxi
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... INTERPENETRATION'
    endif
!
! --- INITIALISATIONS
!
    call utdidt('L', sddisc, 'ECHE', 'PENE_MAXI', index_ = i_echec,&
                valr_ = pene_maxi)
    i_echec_acti = 0
    levent = .false.
    pnmaxi = 0.d0
!
! - Get parameters of contact
!
    nbliai = cfdisd(ds_contact%sdcont_solv,'NBLIAI')
!
! - Access to contact datastructures
!
    jeuite = ds_contact%sdcont_solv(1:14)//'.JEUITE'
    call jeveuo(jeuite, 'L', jjeuit)
!
! --- DETECTION PENETRATION
!
    do iliai = 1, nbliai
        jeufin = zr(jjeuit+3*(iliai-1)+1-1)
        if (jeufin .le. 0.d0) then
            if (abs(jeufin) .gt. pene_maxi) then
                if (abs(jeufin) .gt. pnmaxi) then
                    pnmaxi = abs(jeufin)
                endif
                levent = .true.
            endif
        endif
    end do
!
! --- ACTIVATION EVENEMENT
!
    if (levent) then
        i_echec_acti = i_echec
    endif
!
    call jedema()
end subroutine
