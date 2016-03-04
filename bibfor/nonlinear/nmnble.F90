subroutine nmnble(numins, modele, noma, numedd, ds_measure,&
                  sddyna, sddisc, fonact, ds_contact,&
                  valinc, solalg)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisl.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/isfonc.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmbouc.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmctce.h"
#include "asterfort/nmctcl.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
#include "asterfort/r8inir.h"
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
    integer :: fonact(*)
    character(len=8) :: noma
    character(len=24) :: modele
    character(len=24) :: numedd
    type(NL_DS_Contact), intent(in) :: ds_contact
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=19) :: sddyna, sddisc
    character(len=19) :: solalg(*), valinc(*)
    integer :: numins
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGO - BOUCLE CONTACT)
!
! BOUCLE CONTACT: GESTION DES CONTRAINTES ACTIVES
!
! ----------------------------------------------------------------------
!
! IN  MODELE : NOM DU MODELE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NUMINS : NUMERO D'INSTANT
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IO  ds_measure       : datastructure for measure and statistics management
! In  ds_contact       : datastructure for contact management
! IN  SDDYNA : SD DEDIEE A LA DYNAMIQUE
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMEDD : NOM DU NUME_DDL
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
! ----------------------------------------------------------------------
!
    integer :: neq
    character(len=19) :: depmoi, depplu
    character(len=19) :: depdel, ddepla
    character(len=19) :: vitini, accini, vitplu, accplu
    aster_logical :: lallv, leltc, ldyna
    real(kind=8), pointer :: ddepl(:) => null()
    real(kind=8), pointer :: depde(:) => null()
!
! ----------------------------------------------------------------------
!
    leltc = isfonc(fonact,'ELT_CONTACT')
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    if (.not.leltc) goto 999
!
! --- INITIALISATIONS
!
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
!
    lallv = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF')
    if (lallv) goto 999
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(valinc, 'VALINC', 'ACCPLU', accplu)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
    call nmchex(solalg, 'SOLALG', 'DDEPLA', ddepla)
!
! --- INITIALISATION DES CHAMPS DE DEPLACEMENT
!
    call copisd('CHAMP_GD', 'V', depmoi, depplu)
    call jeveuo(depdel(1:19)//'.VALE', 'E', vr=depde)
    call jeveuo(ddepla(1:19)//'.VALE', 'E', vr=ddepl)
    call r8inir(neq, 0.d0, depde, 1)
    call r8inir(neq, 0.d0, ddepl, 1)
!
! --- AFIN QUE LE VECTEUR DES FORCES D'INERTIE NE SOIT PAS MODIFIE AU
! --- COURS DE LA BOUCLE DES CONTRAINTES ACTIVES PAR L'APPEL A OP0070
! --- ON LE DUPLIQUE ET ON UTILISE CETTE COPIE FIXE (VITINI,ACCINI)
!
    if (ldyna) then
        vitini = ds_contact%sdcont_solv(1:14)//'.VITI'
        accini = ds_contact%sdcont_solv(1:14)//'.ACCI'
        call copisd('CHAMP_GD', 'V', vitini, vitplu)
        call copisd('CHAMP_GD', 'V', accini, accplu)
    endif
!
! - Start timer for preparation of contact
!
    call nmtime(ds_measure, 'Launch', 'Cont_Prep')
!
! - Create elements for contact
!
    call nmctcl(modele, noma, ds_contact)
!
! - Create input fields for contact
!
    call nmctce(modele, noma, ds_contact, sddyna, sddisc,&
                numins)
!
! - Stop timer for preparation of contact
!
    call nmtime(ds_measure, 'Stop', 'Cont_Prep')
    call nmrinc(ds_measure, 'Cont_Prep')
!
999 continue
!
end subroutine
