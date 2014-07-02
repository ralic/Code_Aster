subroutine nmevev(sddisc, numins, valinc, sderro, defico,&
                  resoco, nombcl)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmerge.h"
#include "asterfort/nmevel.h"
#include "asterfort/nmlecv.h"
#include "asterfort/nmltev.h"
    character(len=24) :: sderro, defico, resoco
    integer :: numins
    character(len=19) :: sddisc, valinc(*)
    character(len=4) :: nombcl
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
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! IN  SDERRO : SD GESTION DES ERREURS
! IN  NOMBCL : NOM DE LA BOUCLE
!               'NEWT' - BOUCLE DE NEWTON
!               'FIXE' - BOUCLE DE POINT FIXE
! IN  VALINC : VARIABLE CHAPEAU INCREMENTS DES VARIABLES
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
!
!
!
!
    aster_logical :: lsvimx, ldvres, linsta
    aster_logical :: conver, lerror, lerrcv
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- EVENEMENTS DE L'ALGORITHME
!
    call nmerge(sderro, 'SOLV_ITMX', lsvimx)
    call nmerge(sderro, 'DIVE_RESI', ldvres)
    call nmerge(sderro, 'CRIT_STAB', linsta)
!
! --- LA BOUCLE COURANTE A-T-ELLE CONVERGE ?
!
    call nmlecv(sderro, nombcl, conver)
!
! --- DECLENCHEMENT DE L'ERREUR A CONVERGENCE
!
    call nmltev(sderro, 'ERRC', nombcl, lerror)
    if (conver .and. lerror) then
        lerrcv = .true.
    else
        lerrcv = .false.
    endif
!
! --- ERREUR IMMEDIATE AU NIVEAU DE LA BOUCLE COURANTE ?
!
    call nmltev(sderro, 'ERRI', nombcl, lerror)
!
! --- PREMIER EVENT DECLENCHE
!
    call nmevel(sddisc, numins, defico, resoco, valinc,&
                nombcl, lsvimx, ldvres, linsta, lerrcv,&
                lerror, conver)
!
    call jedema()
end subroutine
