subroutine nmfcon(modele, numedd, mate, fonact, defico,&
                  resoco, sdstat, sdtime, valinc, solalg,&
                  veelem, veasse)
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
    implicit     none
#include "asterfort/isfonc.h"
#include "asterfort/nmfocc.h"
    integer :: fonact(*)
    character(len=24) :: modele, numedd, mate
    character(len=19) :: veelem(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=24) :: defico, resoco, sdstat, sdtime
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! MISE A JOUR DES EFFORTS DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL
! IN  MATE   : CHAMP MATERIAU
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  DEFICO : SD DEFINITION CONTACT
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  SDSTAT : SD STATISTIQUES
! IN  SDTIME : SD TIMER
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
!
! ----------------------------------------------------------------------
!
    logical(kind=1) :: leltc
!
! ----------------------------------------------------------------------
!
!
! --- FONCTIONNALITES ACTIVEES
!
    leltc = isfonc(fonact,'ELT_CONTACT')
!
! --- CALCUL DU SECOND MEMBRE POUR CONTACT/XFEM
!
    if (leltc) then
        call nmfocc('CORRECTION', modele, mate, numedd, fonact,&
                    defico, resoco, sdstat, sdtime, solalg,&
                    valinc, veelem, veasse)
    endif
!
end subroutine
