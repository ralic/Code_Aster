subroutine cfsvfr(ds_contact, lconv)
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
! SAUVEGARDE DU STATUT DE FROTTEMENT POUR PERMETTRE LE TRANSPORT
! D'UN APPARIEMENT A UN AUTRE
!
! ----------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! IN  LCONV  : SAUVEGARDE T-ON UN ETAT CONVERGE ?
!
    aster_logical :: l_lagr_frot
    integer :: nnoco
    integer :: iliac, iliai, posnoe
    integer :: nbliac, llf, llf1, llf2, btotal
    character(len=19) :: statfr, typl, liac
    integer :: jstfr, jtypl, jliac
    character(len=24) :: numlia
    integer :: jnumli
    character(len=8) :: typlia
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LE STATUT DE FROTTEMENT N'EST SAUVEGARDE QU'EN LAGRANGIEN
!
    l_lagr_frot = cfdisl(ds_contact%sdcont_defi,'FROT_LAGR' )
!
    if (l_lagr_frot) then
!
! --- ACCES OBJETS
!
        if (lconv) then
            statfr = ds_contact%sdcont_solv(1:14)//'.STF0'
        else
            statfr = ds_contact%sdcont_solv(1:14)//'.STFR'
        endif
        call jeveuo(statfr, 'E', jstfr)
        typl = ds_contact%sdcont_solv(1:14)//'.TYPL'
        call jeveuo(typl, 'L', jtypl)
        liac = ds_contact%sdcont_solv(1:14)//'.LIAC'
        call jeveuo(liac, 'L', jliac)
        numlia = ds_contact%sdcont_solv(1:14)//'.NUMLIA'
        call jeveuo(numlia, 'L', jnumli)
!
! --- INITIALISATIONS
!
        nnoco = cfdisi(ds_contact%sdcont_defi,'NNOCO')
        call jerazo(statfr, nnoco, 1)
!
! --- INFORMATIONS
!
        nbliac = cfdisd(ds_contact%sdcont_solv,'NBLIAC')
        llf    = cfdisd(ds_contact%sdcont_solv,'LLF' )
        llf1   = cfdisd(ds_contact%sdcont_solv,'LLF1' )
        llf2   = cfdisd(ds_contact%sdcont_solv,'LLF2' )
        btotal = nbliac + llf + llf1 + llf2
!
! --- SAUVEGARDE DU STATUT DE FROTTEMENT
!
        do iliac = 1, btotal
            typlia = zk8(jtypl -1+iliac)
            if (typlia(1:1) .ne. 'F') then
                cycle
            endif
            iliai = zi(jliac -1+iliac)
            posnoe = zi(jnumli-1+4*(iliai-1)+2)
            ASSERT(posnoe.le.nnoco)
            ASSERT(zk8(jstfr-1+posnoe).eq.' ')
            zk8(jstfr-1+posnoe) = typlia
        end do
    endif
!
    call jedema()
!
end subroutine
