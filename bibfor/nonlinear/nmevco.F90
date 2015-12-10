subroutine nmevco(sddisc, nume_inst, ds_contact, i_echec, i_echec_acti)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/diinst.h"
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
    integer, intent(in) :: nume_inst
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(in) :: i_echec
    integer, intent(out) :: i_echec_acti
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - EVENEMENTS)
!
! DETECTION DE L'EVENEMENT COLLISION - CAS DISCRET
!
! ----------------------------------------------------------------------
!
! In  sddisc           : datastructure for time discretization TEMPORELLE
! In  ds_contact       : datastructure for contact management
! IN  NUMINS : NUMERO D'INSTANT
! IN  IECHEC : OCCURRENCE DE L'ECHEC
! OUT IEVDAC : VAUT IECHEC SI EVENEMENT DECLENCHE
!                   0 SINON
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nbliai, ip
    integer :: iliai
    character(len=24) :: numlia, ctevco
    integer :: jnumli, jctevc
    real(kind=8) :: etacin, etacfi, etacol
    real(kind=8) :: fincol, subdur
    real(kind=8) :: instam, instap
    aster_logical :: levent
    integer :: zeven
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... COLLISION'
    endif
!
! - Initializations
!
    i_echec_acti = 0
    levent       = .false.
!
! - Get contact parameters
!
    nbliai = cfdisd(ds_contact%sdcont_solv,'NBLIAI')
    zeven = cfmmvd('ZEVEN')
!
! - Get time dicretization parameters
!
    ASSERT(nume_inst.gt.0)
    instap = diinst(sddisc,nume_inst)
    instam = diinst(sddisc,nume_inst-1)
!
! - Get event parameters
!
    call utdidt('L', sddisc, 'ECHE', 'SUBD_DUREE', index_ = i_echec,&
                valr_ = subdur)
!
! - Access to contact datastructures
!
    numlia = ds_contact%sdcont_solv(1:14)//'.NUMLIA'
    ctevco = ds_contact%sdcont_solv(1:14)//'.EVENCO'
    call jeveuo(numlia, 'L', jnumli)
    call jeveuo(ctevco, 'E', jctevc)
!
! --- STATUT DE LA COLLISION
!
    do iliai = 1, nbliai
        ip = zi(jnumli+4*(iliai-1)+1-1)
        etacin = zr(jctevc+zeven*(ip-1)+1-1)
        etacfi = zr(jctevc+zeven*(ip-1)+2-1)
        etacol = zr(jctevc+zeven*(ip-1)+3-1)
        fincol = zr(jctevc+zeven*(ip-1)+4-1)
!
! ----- COLLISION EN COURS CE N'EST PAS UN EVENEMENT
!
        if (etacol .eq. 1.d0) then
            if (instap .gt. fincol) then
                etacol = 0.d0
                fincol = 0.d0
            endif
        else if (etacol.eq.0.d0) then
!
! ------- COLLISION QUI S'ACTIVE: C'EST UN EVENEMENT
!
            if ((etacin.eq.1.d0) .or. (etacfi.eq.1.d0)) then
                etacol = 1.d0
                fincol = instam+subdur
                levent = .true.
            endif
        endif
        zr(jctevc+zeven*(ip-1)+3-1) = etacol
        zr(jctevc+zeven*(ip-1)+4-1) = fincol
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
