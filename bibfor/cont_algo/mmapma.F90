subroutine mmapma(mesh, ds_contact, model_ndim, i_zone,&
                  lexfro, typint, aliase, posmae, node_mast_nume,&
                  nnomae, elem_mast_indx, elem_mast_nume, ksipr1, ksipr2,&
                  tau1m, tau2m, iptm, iptc, norm,&
                  nommam)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmgaus.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmnumn.h"
#include "asterfort/mmpnoe.h"
#include "asterfort/mmsauv.h"
#include "asterfort/mmtanr.h"
#include "asterfort/utmess.h"
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
    character(len=8) :: mesh
    character(len=8) :: aliase
    type(NL_DS_Contact), intent(in) :: ds_contact
    real(kind=8) :: ksipr1, ksipr2
    integer :: model_ndim
    integer :: posmae, node_mast_nume
    integer :: elem_mast_indx, elem_mast_nume, nnomae
    integer :: i_zone, iptm, iptc
    integer :: typint
    real(kind=8) :: tau1m(3), tau2m(3), norm(3)
    character(len=8) :: nommam
    aster_logical :: lexfro
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES CONTINUES - APPARIEMENT)
!
! RECOPIE DE LA SD APPARIEMENT - CAS MAIT_ESCL
!
! ----------------------------------------------------------------------
!
! IN  LSSFRO : IL Y A DES NOEUDS DANS SANS_GROUP_NO_FR
! IN  NOMA   : NOM DU MAILLAGE
! IN  ALIASE : NOM D'ALIAS DE L'ELEMENT ESCLAVE
! In  ds_contact       : datastructure for contact management
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
    integer :: node_slav_indx, node_slav_nume
!
! ----------------------------------------------------------------------
!
!
! --- POSITION DU NOEUD ESCLAVE SI INTEGRATION AUX NOEUDS
!
    call mmpnoe(ds_contact%sdcont_defi, posmae, aliase, typint, iptm,&
                node_slav_indx)
!
! --- NUMERO ABSOLU DU POINT DE CONTACT
!
    call mmnumn(mesh, typint, node_mast_nume, nnomae, iptm,&
                node_slav_nume)
!
! --- RE-DEFINITION BASE TANGENTE SUIVANT OPTIONS
!
    call mmtanr(mesh, model_ndim, ds_contact, i_zone,&
                lexfro, node_slav_indx, ksipr1, ksipr2, elem_mast_indx,&
                elem_mast_nume, tau1m, tau2m, tau1, tau2)
!
! --- CALCUL DE LA NORMALE
!
    call mmnorm(model_ndim, tau1, tau2, norm, noor)
    if (noor .le. r8prem()) then
        call jenuno(jexnum(mesh//'.NOMMAI', elem_mast_nume), nommam)
        call utmess('F', 'CONTACT3_24', sk=nommam)
    endif
!
! --- POIDS ET COORDONNEES DU POINT DE CONTACT
!
    call mmgaus(aliase, typint, iptm, ksipc1, ksipc2,&
                wpc)
!
! --- SAUVEGARDE APPARIEMENT
!
    call mmsauv(ds_contact, i_zone, iptc, elem_mast_nume, ksipr1,&
                ksipr2, tau1, tau2, node_mast_nume, node_slav_nume,&
                ksipc1, ksipc2, wpc)
!
end subroutine
