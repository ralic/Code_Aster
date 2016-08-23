subroutine nmevev(sddisc   , nume_inst, valinc, sderro, ds_contact,&
                  loop_name)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmerge.h"
#include "asterfort/nmevel.h"
#include "asterfort/nmlecv.h"
#include "asterfort/nmltev.h"
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
    character(len=24), intent(in) :: sderro
    character(len=19), intent(in) :: sddisc
    character(len=4), intent(in) :: loop_name
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(in) :: nume_inst
    character(len=19), intent(in) :: valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! DETECTION DU PREMIER EVENEMENT DECLENCHE
!
! ----------------------------------------------------------------------
!
! NB: DES QU'UN EVENT-DRIVEN EST SATISFAIT, ON SORT
! ON NE CHERCHE PAS A VERIFIER LES AUTRES EVENEMENTS
!
! In  sddisc           : datastructure for time discretization TEMPORELLE
! In  ds_contact       : datastructure for contact management
! IN  NUMINS : NUMERO D'INSTANT
! IN  SDERRO : SD GESTION DES ERREURS
! IN  NOMBCL : NOM DE LA BOUCLE
!               'NEWT' - BOUCLE DE NEWTON
!               'FIXE' - BOUCLE DE POINT FIXE
! IN  VALINC : VARIABLE CHAPEAU INCREMENTS DES VARIABLES
!
! ----------------------------------------------------------------------
!
    aster_logical :: lsvimx, ldvres, linsta, lresmx
    aster_logical :: conver, lerror, lerrcv
!
! ----------------------------------------------------------------------
!
    call nmerge(sderro, 'SOLV_ITMX', lsvimx)
    call nmerge(sderro, 'DIVE_RESI', ldvres)
    call nmerge(sderro, 'RESI_MAXI',  lresmx)
    call nmerge(sderro, 'CRIT_STAB', linsta)
!
! --- LA BOUCLE COURANTE A-T-ELLE CONVERGE ?
!
    call nmlecv(sderro, loop_name, conver)
!
! --- DECLENCHEMENT DE L'ERREUR A CONVERGENCE
!
    call nmltev(sderro, 'ERRC', loop_name, lerror)
    if (conver .and. lerror) then
        lerrcv = .true.
    else
        lerrcv = .false.
    endif
!
! --- ERREUR IMMEDIATE AU NIVEAU DE LA BOUCLE COURANTE ?
!
    call nmltev(sderro, 'ERRI', loop_name, lerror)
!
! --- PREMIER EVENT DECLENCHE
!
    call nmevel(sddisc, nume_inst , valinc, loop_name, lsvimx,&
                ldvres, lresmx    , linsta, lerrcv   , lerror,&
                conver, ds_contact)
!
end subroutine
