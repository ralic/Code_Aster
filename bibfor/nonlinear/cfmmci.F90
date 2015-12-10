subroutine cfmmci(ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmco.h"
#include "asterfort/mminfr.h"
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
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE POUR LE CONTACT (TOUTES METHODES)
!
! INITIALISE LES COEFFICIENTS VARIABLES
!
! ----------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
!
!
!
!
    integer :: nb_cont_zone, i_zone
    aster_logical :: l_cont_disc, l_cont_cont, l_cont_xfem
    real(kind=8) :: coefac, coefaf
    real(kind=8) :: coefpn, coefpt
    real(kind=8) :: coefcr, coeffr, coefcp, coeffp
!
! ----------------------------------------------------------------------
!
    l_cont_xfem = cfdisl(ds_contact%sdcont_defi,'FORMUL_XFEM')
    l_cont_cont = cfdisl(ds_contact%sdcont_defi,'FORMUL_CONTINUE')
    l_cont_disc = cfdisl(ds_contact%sdcont_defi,'FORMUL_DISCRETE')
!
! --- INITIALISATIONS
!
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi,'NZOCO')
!
! --- REMPLISSAGE INITIAL
!
    do i_zone = 1, nb_cont_zone
        if (l_cont_xfem) then
            coefcr = mminfr(ds_contact%sdcont_defi, 'COEF_AUGM_CONT', i_zone)
            coeffr = mminfr(ds_contact%sdcont_defi, 'COEF_AUGM_FROT', i_zone)
            coefcp = mminfr(ds_contact%sdcont_defi, 'COEF_PENA_CONT', i_zone)
            coeffp = mminfr(ds_contact%sdcont_defi, 'COEF_PENA_FROT', i_zone)
            call cfmmco(ds_contact, i_zone, 'COEF_AUGM_CONT', 'E', coefcr)
            call cfmmco(ds_contact, i_zone, 'COEF_AUGM_FROT', 'E', coeffr)
            call cfmmco(ds_contact, i_zone, 'COEF_PENA_CONT', 'E', coefcp)
            call cfmmco(ds_contact, i_zone, 'COEF_PENA_FROT', 'E', coeffp)
        else if (l_cont_cont) then
            coefac = mminfr(ds_contact%sdcont_defi, 'COEF_AUGM_CONT', i_zone)
            coefaf = mminfr(ds_contact%sdcont_defi, 'COEF_AUGM_FROT', i_zone)
            call cfmmco(ds_contact, i_zone, 'COEF_AUGM_CONT', 'E', coefac)
            call cfmmco(ds_contact, i_zone, 'COEF_AUGM_FROT', 'E', coefaf)
        else if (l_cont_disc) then
            coefpn = mminfr(ds_contact%sdcont_defi, 'E_N', i_zone)
            coefpt = mminfr(ds_contact%sdcont_defi, 'E_T', i_zone)
            call cfmmco(ds_contact, i_zone, 'E_N', 'E', coefpn)
            call cfmmco(ds_contact, i_zone, 'E_T', 'E', coefpt)
        else
            ASSERT(.false.)
        endif
    end do
!
end subroutine
