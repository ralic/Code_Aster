subroutine cfappa(mesh, ds_contact, instan)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/apcalc.h"
#include "asterfort/cfapre.h"
#include "asterfort/cfpoin.h"
#include "asterfort/infdbg.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in) :: mesh
    type(NL_DS_Contact), intent(in) :: ds_contact
    real(kind=8), intent(in) :: instan
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! ROUTINE D'AIGUILLAGE POUR L'ACTUALISATION GEOMETRIQUE DU CONTACT:
!  APPARIEMENT, PROJECTION, JEUX
!
! ----------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
!
    integer :: ifm, niv
    character(len=19) :: sdappa
    character(len=19) :: newgeo
!
! ----------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... DEBUT DE L''APPARIEMENT'
    endif
!
! --- INITIALISATIONS
!
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
    newgeo = ds_contact%sdcont_solv(1:14)//'.NEWG'
!
! --- REMPLISSAGE DE LA SD APPARIEMENT - POINTS (COORD. ET NOMS)
!
    call cfpoin(mesh, ds_contact, newgeo, sdappa)
!
! - Pairing
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ......... REALISATION DE L''APPARIEMENT'
    endif
    call apcalc(sdappa, mesh, ds_contact%sdcont_defi, newgeo)
!
! --- RECOPIE APPARIEMENT POUR CONTACT
!
    call cfapre(mesh, ds_contact, newgeo, sdappa, instan)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... FIN DE L''APPARIEMENT'
    endif
!
end subroutine
