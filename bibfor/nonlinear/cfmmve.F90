subroutine cfmmve(noma, defico, resoco, valinc, instan)
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
#include "jeveux.h"
#include "asterfort/apcalc.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvc.h"
#include "asterfort/cfmmvs.h"
#include "asterfort/cfpoin.h"
#include "asterfort/cfsans.h"
#include "asterfort/cfveri.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/mmpoin.h"
#include "asterfort/mmveri.h"
#include "asterfort/nmchex.h"
#include "asterfort/vtgpld.h"
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=19) :: valinc(*)
    real(kind=8) :: instan
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE VERIF)
!
! ROUTINE PRINCIPALE
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NOMA   : NOM DU MAILLAGE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  INSTAN : INST VALUE
!
!
!
!
    integer :: ifm, niv
    character(len=19) :: sdappa, oldgeo, newgeo, depplu
    logical(kind=1) :: lctcc, lctcd, lallv
    character(len=24) :: jeux, loca, enti, zone
    integer :: npt
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> MODE VERIF'
    endif
!
! --- TYPE DE CONTACT
!
    lctcc = cfdisl(defico,'FORMUL_CONTINUE')
    lctcd = cfdisl(defico,'FORMUL_DISCRETE')
    lallv = cfdisl(defico,'ALL_VERIF')
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    oldgeo = noma(1:8)//'.COORDO'
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
!
! --- NOM DES SDs
!
    sdappa = resoco(1:14)//'.APPA'
    newgeo = resoco(1:14)//'.NEWG'
!
! --- MAJ GEOMETRIE
!
    if (lallv) then
        call vtgpld('CUMU', oldgeo, 1.d0, depplu, 'V',&
                    newgeo)
    endif
!
! --- CREATION SD APPARIEMENT EN MODE ALL VERIF
!
    if (lallv) then
!
! ----- RE-REMPLISSAGE DE LA SD APPARIEMENT - POINTS (COORD. ET NOMS)
!
        if (lctcc) then
            call mmpoin(noma, defico, newgeo, sdappa)
        else if (lctcd) then
            call cfpoin(noma, defico, newgeo, sdappa)
        else
            ASSERT(.false.)
        endif
!
! ----- REALISATION DE L'APPARIEMENT
!
        call apcalc(sdappa)
!
    endif
!
! --- CREATION SD PROVISOIRES POUR VERIF
!
    call cfmmvc(defico, jeux, loca, enti, zone,&
                npt)
!
! --- EVALUATION DES POINTS EN MODE VERIF
!
    if (lctcc) then
        call mmveri(noma, defico, resoco, newgeo, sdappa,&
                    npt, jeux, loca, enti, zone, instan)
    else if (lctcd) then
        call cfveri(noma, defico, resoco, newgeo, sdappa,&
                    npt, jeux, loca, enti, zone, instan)
    else
        ASSERT(.false.)
    endif
!
! --- AFFICHAGE DES INTERPENETRATIONS EVENTUELLES
!
    call cfsans(defico, npt, jeux, enti, zone)
!
! --- SAUVEGARDE DANS LA SD RESULTAT
!
    call cfmmvs(defico, resoco, npt, jeux, loca, zone)
!
! --- NETTOYAGE
!
    call jedetr(jeux)
    call jedetr(loca)
    call jedetr(enti)
    call jedetr(zone)
!
    call jedema()
!
end subroutine
