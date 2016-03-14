subroutine cfapno(noma, newgeo, ds_contact, lctfd,&
                  ndimg, izone, posnoe, numnoe,&
                  coorne, posnom, tau1m, tau2m, iliai)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/apvect.h"
#include "asterfort/cfaddm.h"
#include "asterfort/cfcorn.h"
#include "asterfort/cfnewj.h"
#include "asterfort/cfnumn.h"
#include "asterfort/cftanr.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmnorm.h"
#include "asterfort/utmess.h"
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
    character(len=8), intent(in) :: noma
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: newgeo
    real(kind=8), intent(in) :: coorne(3)
    real(kind=8), intent(out) :: tau1m(3)
    real(kind=8), intent(out) :: tau2m(3)
    integer, intent(in) :: izone
    integer, intent(in) :: ndimg
    integer, intent(in) :: posnom(1)
    integer, intent(in) :: posnoe
    integer, intent(in) :: numnoe
    integer, intent(in) :: iliai
    aster_logical, intent(in) :: lctfd
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! RECOPIE DE LA SD APPARIEMENT - CAS NODAL
!
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NEWGEO : NOUVELLE GEOMETRIE (AVEC DEPLACEMENT GEOMETRIQUE)
! In  ds_contact       : datastructure for contact management
! IN  LCTFD  : FROTTEMENT
! IN  LCTF3D : FROTTEMENT EN 3D
! IN  NDIMG  : DIMENSION DE L'ESPACE
! IN  IZONE  : NUMERO DE LA ZONE DE CONTACT
! IN  POSNOE : INDICES DANS CONTNO DU NOEUD ESCLAVE
! IN  NUMNOE : NUMERO ABSOLU DU NOEUD ESCLAVE
! IN  COORNE : COORDONNEES DU NOEUD ESCLAVE
! IN  POSNOM : INDICES DANS CONTNO DU NOEUD MAITRE
! IN  TAU1M  : PREMIERE TANGENTE SUR LE NOEUD MAITRE
! IN  TAU2M  : SECONDE TANGENTE SUR LE NOEUD MAITRE
! IN  ILIAI  : INDICE DE LA LIAISON COURANTE
!
! ----------------------------------------------------------------------
!
    character(len=19) :: sdappa
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: norm(3), noor
    real(kind=8) :: r8bid, jeu
    real(kind=8) :: coornm(3)
    character(len=8) :: nomnoe
    integer :: numnom(1)
    real(kind=8) :: coefno(9)
!
! ----------------------------------------------------------------------
!
!
! --- NUMERO DU NOEUD MAITRE
!
    call cfnumn(ds_contact%sdcont_defi, 1, posnom(1), numnom(1))
!
! --- LECTURE APPARIEMENT
!
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
!
! --- RECUPERATIONS DES TANGENTES AU NOEUD MAITRE
!
    call apvect(sdappa, 'APPARI_NOEUD_TAU1', posnom(1), tau1m)
    call apvect(sdappa, 'APPARI_NOEUD_TAU2', posnom(1), tau2m)
!
! --- COORDONNNEES DU NOEUD MAITRE
!
    call cfcorn(newgeo, numnom(1), coornm)
!
! --- RE-DEFINITION BASE TANGENTE SUIVANT OPTIONS
!
    call cftanr(noma, ndimg, ds_contact, izone,&
                posnoe, 'NOEU', posnom(1), numnom(1), r8bid,&
                r8bid, tau1m, tau2m, tau1, tau2)
!
! --- CALCUL DE LA NORMALE INTERIEURE
!
    call mmnorm(ndimg, tau1, tau2, norm, noor)
    if (noor .le. r8prem()) then
        call jenuno(jexnum(noma//'.NOMNOE', numnoe), nomnoe)
        call utmess('F', 'CONTACT3_26', sk=nomnoe)
    endif
!
! --- CALCUL DU JEU
!
    call cfnewj(ndimg, coorne, coornm, norm, jeu)
!
! --- COEFFICIENT DE LA RELATION LINEAIRE SUR NOEUD MAITRE
!
    coefno(1) = - 1.d0
!
! --- AJOUT DE LA LIAISON NODALE
!
    call cfaddm(ds_contact, lctfd, posnoe, iliai,&
                ndimg, 1, [posnom], coefno, tau1,&
                tau2, norm, jeu, coornm)
!
end subroutine
