subroutine cfsvmu(ds_contact, lconv)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
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
    aster_logical, intent(in) :: lconv
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES)
!
! SAUVEGARDE DU LAGRANGE DE CONTACT POUR PERMETTRE LE TRANSPORT
! D'UN APPARIEMENT A UN AUTRE
!
! ----------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! IN  LCONV  : SAUVEGARDE T-ON UN ETAT CONVERGE ?
!
    integer :: nnoco
    integer :: iliai, posnoe
    integer :: nbliai
    character(len=19) :: svmu, mu
    integer :: jsvmu, jmu
    character(len=24) :: numlia
    integer :: jnumli
    aster_logical :: lgcp
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LE LAGRANGE DE CONTACT N'EST SAUVEGARDE QU'EN GCP
!
    lgcp = cfdisl(ds_contact%sdcont_defi,'CONT_GCP')
!
    if (lgcp) then
!
! --- ACCES OBJETS
!
        if (lconv) then
            svmu = ds_contact%sdcont_solv(1:14)//'.SVM0'
        else
            svmu = ds_contact%sdcont_solv(1:14)//'.SVMU'
        endif
        call jeveuo(svmu, 'E', jsvmu)
        mu = ds_contact%sdcont_solv(1:14)//'.MU'
        call jeveuo(mu, 'L', jmu)
        numlia = ds_contact%sdcont_solv(1:14)//'.NUMLIA'
        call jeveuo(numlia, 'L', jnumli)
!
! --- INITIALISATIONS
!
        nnoco = cfdisi(ds_contact%sdcont_defi,'NNOCO')
        call jerazo(svmu, nnoco, 1)
!
! --- INFORMATIONS
!
        nbliai = cfdisd(ds_contact%sdcont_solv,'NBLIAI')
!
! --- SAUVEGARDE DU STATUT DE FROTTEMENT
!
        do iliai = 1, nbliai
            posnoe = zi(jnumli-1+4*(iliai-1)+2)
            ASSERT(posnoe.le.nnoco)
            ASSERT(zr(jsvmu-1+posnoe).eq.0.d0)
            zr(jsvmu-1+posnoe) = zr(jmu-1+iliai)
        end do
    endif
!
    call jedema()
end subroutine
