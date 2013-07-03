subroutine mmapma(noma, defico, resoco, ndimg, izone,&
                  lexfro, typint, aliase, posmae, nummae,&
                  nnomae, posmam, nummam, ksipr1, ksipr2,&
                  tau1m, tau2m, iptm, iptc, norm,&
                  nommam)
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
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
!
#include "asterc/r8prem.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmgaus.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmnumn.h"
#include "asterfort/mmpnoe.h"
#include "asterfort/mmsauv.h"
#include "asterfort/mmtanr.h"
#include "asterfort/u2mesk.h"
    character(len=8) :: noma
    character(len=8) :: aliase
    character(len=24) :: defico, resoco
    real(kind=8) :: ksipr1, ksipr2
    integer :: ndimg
    integer :: posmae, nummae
    integer :: posmam, nummam, nnomae
    integer :: izone, iptm, iptc
    integer :: typint
    real(kind=8) :: tau1m(3), tau2m(3), norm(3)
    character(len=8) :: nommam
    logical :: lexfro
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES CONTINUES - APPARIEMENT)
!
! RECOPIE DE LA SD APPARIEMENT - CAS MAIT_ESCL
!
! ----------------------------------------------------------------------
!
!
! IN  LSSFRO : IL Y A DES NOEUDS DANS SANS_GROUP_NO_FR
! IN  NOMA   : NOM DU MAILLAGE
! IN  ALIASE : NOM D'ALIAS DE L'ELEMENT ESCLAVE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  NDIMG  : DIMENSION DE L'ESPACE
! IN  IZONE  : ZONE DE CONTACT ACTIVE
! IN  LEXFRO : LE POINT D'INTEGRATION DOIT-IL ETRE EXCLUS DU FROTTEMENT?
! IN  POSMAM : POSITION DE LA MAILLE MAITRE DANS LES SD CONTACT
! IN  NUMMAM : NUMERO ABSOLU MAILLE MAITRE QUI RECOIT LA PROJECTION
! IN  POSMAE : POSITION DE LA MAILLE ESCLAVE DANS LES SD CONTACT
! IN  NNOMAE : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  TYPINT : TYPE D'INTEGRATION
! IN  IPTM   : NUMERO DU POINT D'INTEGRATION DANS LA MAILLE
! IN  KSIPR1 : PREMIERE COORDONNEE PARAMETRIQUE PT CONTACT PROJETE
!              SUR MAILLE MAITRE
! IN  KSIPR2 : SECONDE COORDONNEE PARAMETRIQUE PT CONTACT PROJETE
!              SUR MAILLE MAITRE
! IN  TAU1M  : PREMIERE TANGENTE SUR LA MAILLE MAITRE AU POINT ESCLAVE
!              PROJETE
! IN  TAU2M  : SECONDE TANGENTE SUR LA MAILLE MAITRE AU POINT ESCLAVE
!              PROJETE
! OUT NORM   : NORMALE FINALE
! OUT NOMMAM : NOM DE LA MAILLE MAITRE
!
!
!
!
    real(kind=8) :: noor
    real(kind=8) :: ksipc1, ksipc2, wpc
    real(kind=8) :: tau1(3), tau2(3)
    integer :: posnoe, numnoe
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- POSITION DU NOEUD ESCLAVE SI INTEGRATION AUX NOEUDS
!
    call mmpnoe(defico, posmae, aliase, typint, iptm,&
                posnoe)
!
! --- NUMERO ABSOLU DU POINT DE CONTACT
!
    call mmnumn(noma, typint, nummae, nnomae, iptm,&
                numnoe)
!
! --- RE-DEFINITION BASE TANGENTE SUIVANT OPTIONS
!
    call mmtanr(noma, ndimg, defico, resoco, izone,&
                lexfro, posnoe, ksipr1, ksipr2, posmam,&
                nummam, tau1m, tau2m, tau1, tau2)
!
! --- CALCUL DE LA NORMALE
!
    call mmnorm(ndimg, tau1, tau2, norm, noor)
    if (noor .le. r8prem()) then
        call jenuno(jexnum(noma//'.NOMMAI', nummam), nommam)
        call u2mesk('F', 'CONTACT3_24', 1, nommam)
    endif
!
! --- POIDS ET COORDONNEES DU POINT DE CONTACT
!
    call mmgaus(aliase, typint, iptm, ksipc1, ksipc2,&
                wpc)
!
! --- SAUVEGARDE APPARIEMENT
!
    call mmsauv(resoco, izone, iptc, nummam, ksipr1,&
                ksipr2, tau1, tau2, nummae, numnoe,&
                ksipc1, ksipc2, wpc)
!
    call jedema()
end subroutine
