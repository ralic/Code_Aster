subroutine cfrsmu(ds_contact, reapre)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisl.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
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
    aster_logical :: reapre
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES)
!
! RESTAURATION DU LAGRANGE DE CONTACT APRES UN APPARIEMENT
!
! ----------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! IN  REAPRE : S'AGIT-IL DU PREMIER APPARIEMENT DU PAS DE TEMPS ?
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
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
    call infdbg('CONTACT', ifm, niv)
!
! --- LE LAGRANGE DE CONTACT N'EST RESTAURE QU'EN GCP
!
    lgcp = cfdisl(ds_contact%sdcont_defi,'CONT_GCP')
!
    if (.not.lgcp) then
        goto 999
    endif
!
! --- ACCES OBJETS
!
    if (reapre) then
        svmu = ds_contact%sdcont_solv(1:14)//'.SVM0'
    else
        svmu = ds_contact%sdcont_solv(1:14)//'.SVMU'
    endif
    call jeveuo(svmu, 'L', jsvmu)
    mu = ds_contact%sdcont_solv(1:14)//'.MU'
    call jeveuo(mu, 'E', jmu)
    numlia = ds_contact%sdcont_solv(1:14)//'.NUMLIA'
    call jeveuo(numlia, 'L', jnumli)
!
! --- INFORMATIONS
!
    nbliai = cfdisd(ds_contact%sdcont_solv,'NBLIAI')
!
! --- SAUVEGARDE DU STATUT DE FROTTEMENT
!
    do iliai = 1, nbliai
        posnoe = zi(jnumli-1+4*(iliai-1)+2)
        zr(jmu-1+iliai) = zr(jsvmu-1+posnoe)
    end do
!
999 continue
!
    call jedema()
end subroutine
