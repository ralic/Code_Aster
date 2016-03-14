subroutine cfdeco(ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/cfsvmu.h"
#include "asterfort/jedupo.h"
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
! In  ds_contact       : datastructure for contact management
!
! ----------------------------------------------------------------------
!
    character(len=24) :: jeuite, jeusav
!
! ----------------------------------------------------------------------
!
! --- SAUVEGARDE DU JEU POUR REAC_GEOM='SANS'
!
    jeuite = ds_contact%sdcont_solv(1:14)//'.JEUITE'
    jeusav = ds_contact%sdcont_solv(1:14)//'.JEUSAV'
    call jedupo(jeuite, 'V', jeusav, .false._1)
!
! --- SAUVEGARDE DU LAGRANGE DE CONTACT POUR ALGO_CONT='GCP'
!
    call cfsvmu(ds_contact, .true._1)
!
end subroutine
