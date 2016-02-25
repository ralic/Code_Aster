subroutine cfgeom(iter_newt, mesh     , ds_measure, ds_contact,&
                  disp_curr, time_curr)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfappa.h"
#include "asterfort/cfimp4.h"
#include "asterfort/cfsvfr.h"
#include "asterfort/cfsvmu.h"
#include "asterfort/geomco.h"
#include "asterfort/infdbg.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
#include "asterfort/reajeu.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    integer, intent(in) :: iter_newt
    character(len=8), intent(in) :: mesh
    type(NL_DS_Measure), intent(inout) :: ds_measure
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: disp_curr
    real(kind=8), intent(in) :: time_curr
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Discrete methods - Pairing
!
! --------------------------------------------------------------------------------------------------
!
! In  iter_newt        : index of current Newton iteration
! In  mesh             : name of mesh
! IO  ds_measure       : datastructure for measure and statistics management
! In  ds_contact       : datastructure for contact management
! In  disp_curr        : current displacements
! In  time_curr        : current time
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: l_pair
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
!
! - Is pairing ?
!
    l_pair      = ds_contact%l_pair
!
! - Save before pairing
!
    if (l_pair) then
        call cfsvmu(ds_contact, .false._1)
        call cfsvfr(ds_contact, .false._1)
    endif
!
! - Print
!
    if (niv .ge. 2) then
        if (l_pair) then
            write (ifm,*) '<CONTACT> ... REACTUALISATION DE L''APPARIEMENT'
        else
            write (ifm,*) '<CONTACT> ... PAS DE REACTUALISATION DE L''APPARIEMENT'
        endif
    endif
!
! - Pairing or not pairing ?
!
    if (l_pair) then
!
        call nmtime(ds_measure, 'Init'  , 'Contact_Geometry')
        call nmtime(ds_measure, 'Launch', 'Contact_Geometry')
!
! ----- Update geometry
!
        call geomco(mesh, ds_contact, disp_curr)
!
! ----- Pairing
!
        call cfappa(mesh, ds_contact, time_curr)
        call nmtime(ds_measure, 'Stop', 'Contact_Geometry')
        call nmrinc(ds_measure, 'Contact_Geometry')
!
    else
!
! ----- Update gaps (for step cut)
!
        if (iter_newt .eq. 0) then
            call reajeu(ds_contact)
        endif
    endif
!
! - Debug print
!
    if (niv .ge. 2) then
        call cfimp4(ds_contact, mesh, ifm)
    endif
!
! - Print
!
    if (niv .ge. 2) then
        if (l_pair) then
            write (ifm,*) '<CONTACT> ... FIN DE REACTUALISATION DE L''APPARIEMENT'
        endif
    endif
!
end subroutine
