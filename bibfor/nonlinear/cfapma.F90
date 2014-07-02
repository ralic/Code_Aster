subroutine cfapma(noma, newgeo, defico, resoco, lctfd,&
                  lctf3d, ndimg, izone, posnoe, numnoe,&
                  coorne, posmam, ksipr1, ksipr2, tau1m,&
                  tau2m, iliai)
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
#include "asterc/r8prem.h"
#include "asterfort/cfaddm.h"
#include "asterfort/cfcoor.h"
#include "asterfort/cfnewj.h"
#include "asterfort/cfnumm.h"
#include "asterfort/cfposn.h"
#include "asterfort/cfreli.h"
#include "asterfort/cftanr.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmnorm.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=19) :: newgeo
    real(kind=8) :: coorne(3), ksipr1, ksipr2
    real(kind=8) :: tau1m(3), tau2m(3)
    integer :: izone, ndimg
    integer :: posmam
    integer :: posnoe, numnoe
    integer :: iliai
    aster_logical :: lctfd, lctf3d
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! RECOPIE DE LA SD APPARIEMENT - CAS MAIT_ESCL
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NEWGEO : NOUVELLE GEOMETRIE (AVEC DEPLACEMENT GEOMETRIQUE)
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  LCTFD  : FROTTEMENT
! IN  LCTF3D : FROTTEMENT EN 3D
! IN  NDIMG  : DIMENSION DE L'ESPACE
! IN  IZONE  : NUMERO DE LA ZONE DE CONTACT
! IN  POSNOE : INDICES DANS CONTNO DU NOEUD ESCLAVE
! IN  NUMNOE : NUMERO ABSOLU DU NOEUD ESCLAVE
! IN  COORNE : COORDONNEES DU NOEUD ESCLAVE
! IN  POSMAM : INDICES DANS CONTNO DE LA MAILLE MAITRE
! IN  KSIPR1 : COORDONNEE PARAMETRIQUE SUR MAITRE DU POINT ESCLAVE
!              PROJETE
! IN  KSIPR2 : COORDONNEE PARAMETRIQUE SUR MAITRE DU POINT ESCLAVE
!              PROJETE
! IN  TAU1M  : PREMIERE TANGENTE SUR LA MAILLE MAITRE
! IN  TAU2M  : SECONDE TANGENTE SUR LA MAILLE MAITRE
! IN  KSI2   : SECONDE COORD. DE LA PROJECTION
! IN  ILIAI  : INDICE DE LA LIAISON COURANTE
!
!
!
!
    integer :: ifm, niv
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: norm(3), noor
    real(kind=8) :: jeu
    real(kind=8) :: coornp(3)
    character(len=8) :: nomnoe
    integer :: nbnom, nummam
    integer :: posnsm(9)
    real(kind=8) :: coefno(9)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- NUMERO DE LA MAILLE MAITRE
!
    call cfnumm(defico, posmam, nummam)
!
! --- CARACTERISTIQUES DE LA MAILLE MAITRE
!
    call cfposn(defico, posmam, posnsm, nbnom)
!
! --- COORDONNEES PROJECTION DU NOEUD ESCLAVE SUR LA MAILLE MAITRE
!
    call cfcoor(noma, defico, newgeo, posmam, ksipr1,&
                ksipr2, coornp)
!
! --- RE-DEFINITION BASE TANGENTE SUIVANT OPTIONS
!
    call cftanr(noma, ndimg, defico, resoco, izone,&
                posnoe, 'MAIL', posmam, nummam, ksipr1,&
                ksipr2, tau1m, tau2m, tau1, tau2)
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
    call cfnewj(ndimg, coorne, coornp, norm, jeu)
!
! --- COEFFICIENT DE LA RELATION LINEAIRE SUR NOEUD MAITRE
!
    call cfreli(noma, nummam, nbnom, ksipr1, ksipr2,&
                coefno)
!
! --- AJOUT DE LA LIAISON NODALE
!
    call cfaddm(resoco, lctfd, lctf3d, posnoe, iliai,&
                ndimg, nbnom, posnsm, coefno, tau1,&
                tau2, norm, jeu, coornp)
!
    call jedema()
end subroutine
