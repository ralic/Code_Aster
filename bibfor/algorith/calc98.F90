subroutine calc98(nomres, mailla, numddl)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
!***********************************************************************
!    P. RICHARD                                     DATE 09/07/91
!-----------------------------------------------------------------------
!  BUT: ROUTINE DE CALCUL DE BASE MODALE
!
!    BASE MODALE DE TYPE MIXTE CRAIG-BAMPTON MAC-NEAL OU INCONNUE
!
!
!
!
#include "jeveux.h"
#include "asterfort/crlidd.h"
#include "asterfort/ddlact.h"
#include "asterfort/defint.h"
#include "asterfort/gesdef.h"
    character(len=8) :: nomres, mailla
    character(len=19) :: numddl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!---------  RECUPERATION DES NOEUDS D'INTERFACE       ------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call defint(mailla, nomres)
!
!-------------CREATION TABLEAU DESCRIPTION DES DEFORMES ----------------
!           ET MODIFICATION NUMEROTATION NOEUDS INTERFACES
!
    call crlidd(nomres, mailla)
!
!---------------PRISE EN COMPTE DES MASQUES DE DDL AUX NOEUDS-----------
!             ET DETERMINATION DU NOMBRE DE MODES ET DEFORMEES
!
    call gesdef(nomres, numddl)
!
!
!-----------------DETERMINATION DES DDL INTERFACE ACTIFS FINAUX---------
!
!
    call ddlact(nomres, numddl)
!
end subroutine
