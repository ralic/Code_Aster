subroutine ndexpl(modele  , numedd         , numfix  , mate       , carele,&
                  comref  , ds_constitutive, lischa  , ds_algopara, fonact,&
                  ds_print, ds_measure     , sdnume  , sddyna     , sddisc,&
                  sderro  , valinc         , numins  , solalg     , solveu,&
                  matass  , maprec         , ds_inout, meelem     , measse,&
                  veelem  , veasse         , nbiter  )
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/ndxcvg.h"
#include "asterfort/ndxdec.h"
#include "asterfort/ndxdep.h"
#include "asterfort/ndxnpa.h"
#include "asterfort/ndxpre.h"
#include "asterfort/nmchar.h"
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
! aslint: disable=W1504
!
    integer :: numins
    integer :: fonact(*)
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    character(len=24) :: sderro
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=19) :: sdnume, sddyna, sddisc
    type(NL_DS_InOut), intent(in) :: ds_inout
    type(NL_DS_Print), intent(inout) :: ds_print
    character(len=19) :: valinc(*), solalg(*)
    character(len=19) :: meelem(*), veelem(*)
    character(len=19) :: measse(*), veasse(*)
    character(len=19) :: lischa
    character(len=19) :: solveu, maprec, matass
    character(len=24) :: modele, numedd, numfix
    character(len=24) :: comref
    type(NL_DS_Constitutive), intent(in) :: ds_constitutive
    character(len=24) :: mate, carele
    integer :: nbiter
!
! --------------------------------------------------------------------------------------------------
!
! OPERATEUR NON-LINEAIRE MECANIQUE
!
! ALGORITHME DYNAMIQUE EXPLICITE
!
! --------------------------------------------------------------------------------------------------
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARIABLES DE COMMANDE DE REFERENCE
! In  ds_constitutive  : datastructure for constitutive laws management
! IN  LISCHA : L_CHARGES
! In  ds_inout         : datastructure for input/output management
! In  ds_algopara      : datastructure for algorithm parameters
! IN  SOLVEU : SOLVEUR
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IO  ds_measure       : datastructure for measure and statistics management
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  SDDYNA : SD DYNAMIQUE
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  MAPREC : NOM DE LA MATRICE DE PRECONDITIONNEMENT (GCPC)
! OUT NBITER : NOMBRE D'ITERATIONS DE NEWTON
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: lerrit
!
! --------------------------------------------------------------------------------------------------
!

!
! --- INITIALISATION DES CHAMPS D'INCONNUES POUR LE NOUVEAU PAS DE TEMPS
!
    call ndxnpa(modele, mate  , carele, fonact, ds_print,&
                sddisc, sddyna, sdnume, numedd, numins  ,&
                valinc, solalg)
!
! --- CALCUL DES CHARGEMENTS CONSTANTS AU COURS DU PAS DE TEMPS
!
    call nmchar('FIXE', ' '            , modele, numedd  , mate      ,&
                carele, ds_constitutive, lischa, numins  , ds_measure,&
                sddisc, fonact         , comref, ds_inout, valinc    ,&
                solalg, veelem         , measse, veasse  , sddyna)
!
! --- PREDICTION D'UNE DIRECTION DE DESCENTE
!
    call ndxpre(modele  , numedd         , numfix    , mate       , carele,&
                comref  , ds_constitutive, lischa    , ds_algopara, solveu,&
                fonact  , sddisc         , ds_measure, numins     , valinc,&
                solalg  , matass         , maprec    , sddyna     , sderro,&
                ds_inout, meelem         , measse    , veelem     , veasse,&
                lerrit)
!
    if (lerrit) goto 315
!
! --- CALCUL PROPREMENT DIT DE L'INCREMENT DE DEPLACEMENT
!
    call ndxdep(numedd, fonact, numins, sddisc, sddyna,&
                sdnume, valinc, solalg, veasse)
!
! --- ESTIMATION DE LA CONVERGENCE
!
315 continue
    call ndxcvg(sddisc, sderro, valinc)
!
! --- EN L'ABSENCE DE CONVERGENCE ON CHERCHE A SUBDIVISER LE PAS
! --- DE TEMPS SI L'UTILISATEUR A FAIT LA DEMANDE
!
    call ndxdec(ds_print, sddisc, sderro, numins)
!
    nbiter = 1
!
end subroutine
