subroutine cfmmve(noma, ds_contact, valinc, instan)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/apcalc.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvc.h"
#include "asterfort/cfmmvs.h"
#include "asterfort/cfpoin.h"
#include "asterfort/cfsans.h"
#include "asterfort/cfveri.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedetr.h"
#include "asterfort/mmpoin.h"
#include "asterfort/mmveri.h"
#include "asterfort/nmchex.h"
#include "asterfort/mreacg.h"
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
    character(len=8), intent(in) :: noma
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: valinc(*)
    real(kind=8), intent(in) :: instan
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE VERIF)
!
! ROUTINE PRINCIPALE
!
! ----------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! IN  NOMA   : NOM DU MAILLAGE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  INSTAN : INST VALUE
!
!
!
!
    integer :: ifm, niv
    character(len=19) :: sdappa, newgeo, depplu
    aster_logical :: lctcc, lctcd, lallv
    character(len=24) :: jeux, loca, enti, zone
    integer :: npt
!
! ----------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> MODE VERIF'
    endif
!
! --- TYPE DE CONTACT
!
    lctcc = cfdisl(ds_contact%sdcont_defi,'FORMUL_CONTINUE')
    lctcd = cfdisl(ds_contact%sdcont_defi,'FORMUL_DISCRETE')
    lallv = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF')
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
!
! --- NOM DES SDs
!
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
    newgeo = ds_contact%sdcont_solv(1:14)//'.NEWG'
!
! - Geometry update
!
    if (lallv) then
        call mreacg(noma, ds_contact, field_update_ = depplu)
    endif
!
! --- CREATION SD APPARIEMENT EN MODE ALL VERIF
!
    if (lallv) then
!
! ----- RE-REMPLISSAGE DE LA SD APPARIEMENT - POINTS (COORD. ET NOMS)
!
        if (lctcc) then
            call mmpoin(noma, ds_contact, newgeo, sdappa)
        else if (lctcd) then
            call cfpoin(noma, ds_contact, newgeo, sdappa)
        else
            ASSERT(.false.)
        endif
!
! ----- Pairing
!
        call apcalc(sdappa, noma, ds_contact%sdcont_defi, newgeo)
!
    endif
!
! --- CREATION SD PROVISOIRES POUR VERIF
!
    call cfmmvc(ds_contact%sdcont_defi, jeux, loca, enti, zone,&
                npt)
!
! --- EVALUATION DES POINTS EN MODE VERIF
!
    if (lctcc) then
        call mmveri(noma, ds_contact, newgeo, sdappa,&
                    npt, jeux, loca, enti, zone,&
                    instan)
    else if (lctcd) then
        call cfveri(noma, ds_contact, newgeo, sdappa,&
                    npt, jeux, loca, enti, zone,&
                    instan)
    else
        ASSERT(.false.)
    endif
!
! --- AFFICHAGE DES INTERPENETRATIONS EVENTUELLES
!
    call cfsans(ds_contact%sdcont_defi, npt, jeux, enti, zone)
!
! --- SAUVEGARDE DANS LA SD RESULTAT
!
    call cfmmvs(ds_contact, npt, jeux, loca, zone)
!
! --- NETTOYAGE
!
    call jedetr(jeux)
    call jedetr(loca)
    call jedetr(enti)
    call jedetr(zone)
!
end subroutine
