subroutine mldeco(ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONSTINUE LAC - POST-TRAITEMENT)
!
! GESTION DE LA DECOUPE
!
! ----------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
!
    character(len=19) :: sdcont_stat, sdcont_zeta
    integer, pointer :: v_sdcont_stat(:) => null()
    integer, pointer :: v_sdcont_zeta(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    sdcont_stat = ds_contact%sdcont_solv(1:14)//'.STAT'
    sdcont_zeta = ds_contact%sdcont_solv(1:14)//'.ZETA'    
    call jeveuo(sdcont_zeta, 'E', vi = v_sdcont_zeta)
    call jeveuo(sdcont_stat, 'L', vi = v_sdcont_stat)   
!
! --- SAUVEGARDE DE L ETAT DE CONTACT EN CAS DE REDECOUPAGE
! 
    v_sdcont_zeta(:)=v_sdcont_stat(:)
    
    call jedema()
!
end subroutine
