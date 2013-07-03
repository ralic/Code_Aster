subroutine cfdeco(defico, resoco)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/cfsvfr.h"
#include "asterfort/cfsvmu.h"
#include "asterfort/jedupo.h"
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - POST-TRAITEMENT)
!
! SAUVEGARDE DES VECTEURS DE JEU
!
! ----------------------------------------------------------------------
!
! ON SAUVEGARDE LE JEU POUR PERMETTRE LA SUBDIVISION DU PAS DE TEMPS
! EN GEOMETRIE INITIALE (REAC_GEOM='SANS')
!
! RECUPERATION ULTERIEURE EVENTUELLE DANS REAJEU
!
! ON SAUVEGARDE LE LAGRANGE DE CONTACT POUR PERMETTRE SA RESTAURATION
! EN CAS DE DECOUPE EN GCP (ALGO_CONT='GCP')
!
! RECUPERATION ULTERIEURE DANS CFRSMU
!
! ON SAUVEGARDE LE STATUT DE FROTTEMENT POUR PERMETTRE SA RESTAURATION
! EN CAS DE DECOUPE EN LAGRANGIEN (ALGO_FROT='LAGRANGIEN')
!
! RECUPERATION ULTERIEURE DANS CFINAL
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!
! ----------------------------------------------------------------------
!
    character(len=24) :: jeuite, jeusav
!
! ----------------------------------------------------------------------
!
! --- SAUVEGARDE DU JEU POUR REAC_GEOM='SANS'
!
    jeuite = resoco(1:14)//'.JEUITE'
    jeusav = resoco(1:14)//'.JEUSAV'
    call jedupo(jeuite, 'V', jeusav, .false.)
!
! --- SAUVEGARDE DU LAGRANGE DE CONTACT POUR ALGO_CONT='GCP'
!
    call cfsvmu(defico, resoco, .true.)
!
! --- SAUVEGARDE DU STATUT DE FROTTEMENT POUR ALGO_FROT='LAGRANGIEN'
!
    call cfsvfr(defico, resoco, .true.)
!
end subroutine
