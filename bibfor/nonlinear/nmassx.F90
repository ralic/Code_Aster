subroutine nmassx(modele, numedd, mate, carele, comref,&
                  compor, lischa, carcri, fonact, ds_measure,&
                  sddyna, valinc, solalg, veelem, veasse,&
                  ldccvg, codere, cndonn)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/assvec.h"
#include "asterfort/ndasva.h"
#include "asterfort/ndynre.h"
#include "asterfort/nmasdi.h"
#include "asterfort/nmasfi.h"
#include "asterfort/nmasva.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdiri.h"
#include "asterfort/nmfint.h"
#include "asterfort/nmtime.h"
#include "asterfort/vtaxpy.h"
#include "asterfort/vtzero.h"
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
    integer :: ldccvg
    integer :: fonact(*)
    character(len=19) :: lischa, sddyna
    character(len=24) :: modele, numedd, mate, codere
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=24) :: carele, compor, comref, carcri
    character(len=19) :: solalg(*), valinc(*)
    character(len=19) :: veasse(*), veelem(*)
    character(len=19) :: cndonn
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL DU SECOND MEMBRE POUR LA PREDICTION EN EXPLICITE
!
! ----------------------------------------------------------------------
!
! IN  MODELE : NOM DU MODELE
! IN  NUMEDD : NOM DE LA NUMEROTATION
! IN  MATE   : NOM DU CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VALEURS DE REF DES VARIABLES DE COMMANDE
! IN  COMPOR : CARTE DECRIVANT LE TYPE DE COMPORTEMENT
! IN  LISCHA : SD LISTE DES CHARGES
! IN  CARCRI : CARTE DES CRITERES DE CONVERGENCE LOCAUX
! IO  ds_measure       : datastructure for measure and statistics management
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  SDDYNA : SD DYNAMIQUE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! OUT CNDONN : VECTEUR ASSEMBLE DES FORCES DONNEES
! OUT CODERE : CHAM_ELEM CODE RETOUR INTEGRATION LDC
! OUT LDCCVG : CODE RETOUR INTEGRATION DU COMPORTEMENT
!                0 - OK
!                1 - ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
!                2 - ERREUR DANS LES LDC SUR LA NON VERIFICATION DE
!                    CRITERES PHYSIQUES
!                3 - SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
!
!
!
!
    character(len=19) :: cnffdo, cndfdo, cnfvdo, cnvady
    character(len=19) :: cndumm
    character(len=19) :: cndiri, cnfint, cnvcpr
    character(len=19) :: vediri, vefint
    character(len=19) :: depmoi, vitmoi, accmoi
    integer :: iterat
    integer :: i, nbvec
    real(kind=8) :: coef(8)
    character(len=19) :: vect(8)
    real(kind=8) :: coeequ
!
! ----------------------------------------------------------------------
!
    iterat = 0
    call vtzero(cndonn)
    cndumm = '&&CNCHAR.DUMM'
    cnffdo = '&&CNCHAR.FFDO'
    cndfdo = '&&CNCHAR.DFDO'
    cnfvdo = '&&CNCHAR.FVDO'
    cnvady = '&&CNCHAR.FVDY'
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(veasse, 'VEASSE', 'CNDIRI', cndiri)
    call nmchex(veasse, 'VEASSE', 'CNFINT', cnfint)
    call nmchex(veelem, 'VEELEM', 'CNDIRI', vediri)
    call nmchex(veelem, 'VEELEM', 'CNFINT', vefint)
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'VITMOI', vitmoi)
    call nmchex(valinc, 'VALINC', 'ACCMOI', accmoi)
!
! --- COEFFICIENTS POUR MULTI-PAS
!
    coeequ = ndynre(sddyna,'COEF_MPAS_EQUI_COUR')
!
! - Launch timer
!
    call nmtime(ds_measure, 'Init'  , 'Second_Member')
    call nmtime(ds_measure, 'Launch', 'Second_Member')
!
! --- CALCUL DU VECTEUR DES CHARGEMENTS FIXES        (NEUMANN)
!
    call nmasfi(fonact, sddyna, veasse, cnffdo, cndumm)
!
! --- CALCUL DU VECTEUR DES CHARGEMENTS FIXES        (DIRICHLET)
!
    call nmasdi(fonact, veasse, cndfdo, cndumm)
!
! --- CALCUL DU VECTEUR DES CHARGEMENTS VARIABLES    (NEUMANN)
!
    call nmasva(sddyna, veasse, cnfvdo)
!
! --- CALCUL DU VECTEUR DES CHARGEMENTS VARIABLES DYNAMIQUES (NEUMANN)
!
    call ndasva('PRED', sddyna, veasse, cnvady)
!
! --- SECOND MEMBRE DES VARIABLES DE COMMANDE
!
    call nmchex(veasse, 'VEASSE', 'CNVCPR', cnvcpr)
!
! --- CALCUL DES REACTIONS D'APPUI BT.LAMBDA
!
    call nmdiri(modele, mate, carele, lischa, sddyna,&
                depmoi, vitmoi, accmoi, vediri)
!
! --- ASSEMBLAGE DES REACTIONS D'APPUI
!
    call assvec('V', cndiri, 1, vediri, [1.d0],&
                numedd, ' ', 'ZERO', 1)
!
! - End timer
!
    call nmtime(ds_measure, 'Stop', 'Second_Member')
!
! --- CALCUL DES FORCES INTERIEURES
!
    call nmfint(modele, mate, carele, comref, compor,&
                carcri, fonact, iterat, sddyna, ds_measure,&
                valinc, solalg, ldccvg, codere,&
                vefint)
!
! --- ASSEMBLAGE DES FORCES INTERIEURES
!
    call assvec('V', cnfint, 1, vefint, [1.d0],&
                numedd, ' ', 'ZERO', 1)
!
! --- CHARGEMENTS DONNES
!
    nbvec = 7
    coef(1) = 1.d0
    coef(2) = 1.d0
    coef(3) = -1.d0
    coef(4) = 1.d0
    coef(5) = 1.d0
    coef(6) = -1.d0
    coef(7) = coeequ
    vect(1) = cnffdo
    vect(2) = cnfvdo
    vect(3) = cnfint
    vect(4) = cnvcpr
    vect(5) = cndfdo
    vect(6) = cndiri
    vect(7) = cnvady
!
! --- CHARGEMENT DONNE
!
    if (nbvec .gt. 8) then
        ASSERT(.false.)
    endif
    do i = 1, nbvec
        call vtaxpy(coef(i), vect(i), cndonn)
    end do
!
end subroutine
