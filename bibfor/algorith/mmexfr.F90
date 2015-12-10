subroutine mmexfr(mesh, ds_contact, i_zone, elem_mast_indx, tau1,&
                  tau2)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterc/r8prem.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfnomm.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfr.h"
#include "asterfort/mmnorm.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/utmess.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
#include "blas/dscal.h"
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
! person_in_charge: thomas.de-soza at edf.fr
!
    character(len=8), intent(in) :: mesh
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(in) :: i_zone
    integer, intent(in) :: elem_mast_indx
    real(kind=8), intent(out) :: tau1(3)
    real(kind=8), intent(out) :: tau2(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT - UTILITAIRE)
!
! REDEFINIT LA BASE TANGENTE LOCALE POUR SANS_GROUP_NO_FR
!
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! In  ds_contact       : datastructure for contact management
! IN  IZONE  : NUMERO DE LA ZONE DE CONTACT
! IN  POSMAM : POSITION DE LA MAILLE MAITRE DANS LES SD CONTACT
! OUT TAU1   : PREMIER VECTEUR TANGENT
! OUT TAU2   : SECOND VECTEUR TANGENT
!
    integer :: model_ndim, ndirex
    real(kind=8) :: vdirex(3), norm(3), norme
    real(kind=8) :: tau1fr(3), tau2fr(3)
    real(kind=8) :: extau1, extau2
    character(len=8) :: elem_mast_name
!
! ----------------------------------------------------------------------
!
    model_ndim = cfdisi(ds_contact%sdcont_defi,'NDIM')
!
! --- NOMBRE DE DIRECTIONS A EXCLURE POUR LA ZONE
!
    ndirex = mminfi(ds_contact%sdcont_defi,'EXCL_DIR',i_zone)
!
! --- REDEFINITION DU REPERE SI NECESSAIRE (UNE DIRECTION EXCLUE EN 3D)
!
    if ((model_ndim.eq.3) .and. (ndirex.eq.1)) then
! ----- DIRECTION D'EXCLUSION
        vdirex(1) = mminfr(ds_contact%sdcont_defi,'EXCL_FROT_DIRX',i_zone )
        vdirex(2) = mminfr(ds_contact%sdcont_defi,'EXCL_FROT_DIRY',i_zone )
        vdirex(3) = mminfr(ds_contact%sdcont_defi,'EXCL_FROT_DIRZ',i_zone )
! ----- ON LA PROJETTE SUR LE PLAN TANGENT
        call dcopy(3, tau1, 1, tau1fr, 1)
        call dcopy(3, tau2, 1, tau2fr, 1)
        extau1 = ddot(3,vdirex,1,tau1,1)
        extau2 = ddot(3,vdirex,1,tau2,1)
        call dscal(3, extau1, tau1fr, 1)
        call daxpy(3, extau2, tau2fr, 1, tau1fr,&
                   1)
        call normev(tau1fr, norme)
        if (norme .le. r8prem()) then
            call cfnomm(mesh, ds_contact%sdcont_defi, 'MAIL', elem_mast_indx, elem_mast_name)
            call utmess('F', 'CONTACT3_18', sk=elem_mast_name, si=i_zone, nr=3,&
                        valr=vdirex)
        endif
! ----- ON CALCULE TAU2FR PAR PROD. VECT.
        call mmnorm(model_ndim, tau1, tau2, norm, norme)
        call provec(tau1fr, norm, tau2fr)
! ----- RECOPIE
        call dcopy(3, tau1fr, 1, tau1, 1)
        call dcopy(3, tau2fr, 1, tau2, 1)
    endif
!
end subroutine
