subroutine nmresd(fonact, sddyna, ds_measure, solveu, numedd,&
                  instan, maprec, matass    , cndonn, cnpilo,&
                  cncine, solalg, rescvg)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infdbg.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmresg.h"
#include "asterfort/nmreso.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
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
    character(len=19) :: solalg(*)
    character(len=19) :: maprec, matass
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=19) :: solveu, sddyna
    character(len=19) :: cncine, cndonn, cnpilo
    character(len=24) :: numedd
    real(kind=8) :: instan
    integer :: rescvg
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - CALCUL)
!
! RESOLUTION DU SYSTEME LINEAIRE K.dU = F
!
! ----------------------------------------------------------------------
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SDDYNA : SD DYNAMIQUE
! IO  ds_measure       : datastructure for measure and statistics management
! IN  SOLVEU : SOLVEUR
! IN  NUMEDD : NUME_DDL
! IN  INSTAN : INSTANT COURANT
! IN  MAPREC : MATRICE DE PRECONDITIONNEMENT (GCPC)
! IN  MATASS : MATRICE ASSEMBLEE
! IN  CNDONN : CHAM_NO DE CHARGE DONNEE
! IN  CNPILO : CHAM_NO DE CHARGE PILOTEE
! IN  CNCINE : CHAM_NO DE CHARGE CINEMATIQUE
! OUT DEPSOL : SOLUTION DU DU SYSTEME K.DU = DF
!                      DEPSOL(1) EN L'ABSENCE DE PILOTAGE
!                      DEPSOL(1) ET DEPSOL(2) AVEC PILOTAGE
! OUT RESCVG : CODE RETOUR RESOLUTION SYSTEME LINEAIRE
!                -1 : PAS DE RESOLUTION
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : NOMBRE MAXIMUM D'ITERATIONS ATTEINT
!
!
    aster_logical :: lprmo
    character(len=19) :: depso1, depso2
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... RESOLUTION'
    endif
!
! --- EXTRACTION VARIABLES CHAPEAUX
!
    call nmchex(solalg, 'SOLALG', 'DEPSO1', depso1)
    call nmchex(solalg, 'SOLALG', 'DEPSO2', depso2)
!
! --- FONCTIONNALITES ACTIVEES
!
    lprmo = ndynlo(sddyna,'PROJ_MODAL')
!
! --- RESOLUTION GENERALISEE OU PHYSIQUE
!
    call nmtime(ds_measure, 'Init', 'Solve')
    call nmtime(ds_measure, 'Launch', 'Solve')
!
    if (lprmo) then
        call nmresg(numedd, sddyna, instan, cndonn, depso1)
    else
        call nmreso(fonact, cndonn, cnpilo, cncine, solveu,&
                    maprec, matass, depso1, depso2, rescvg)
    endif
!
    call nmtime(ds_measure, 'Stop', 'Solve')
    call nmrinc(ds_measure, 'Solve')
!
end subroutine
