subroutine cfpoin(noma, defico, newgeo, sdappa)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/apzoni.h"
#include "asterfort/assert.h"
#include "asterfort/cfcorn.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmex.h"
#include "asterfort/cfnumn.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
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
    character(len=8), intent(in) :: noma
    character(len=19), intent(in) :: sdappa
    character(len=24), intent(in) :: defico
    character(len=19), intent(in) :: newgeo
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - SD APPARIEMENT)
!
! REMPLISSAGE DE LA SD APPARIEMENT - POINTS (COORD. ET NOMS)
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=24) :: appoin, apinfp
    integer :: jpoin, jinfp
    character(len=24) :: apnoms
    integer :: jpnoms
    integer :: suppok
    integer :: ip, inoe, izone
    integer :: nbpt, nbnoe
    integer :: posnoe(1), numnoe(1)
    integer :: jdecne
    real(kind=8) :: coorpt(3)
    character(len=8) :: nomnoe
    character(len=16) :: nompt
    integer :: nzoco
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ......... PREPARATION DE ' //&
        'L''APPARIEMENT'
    endif
!
! --- ACCES SDAPPA
!
    appoin = sdappa(1:19)//'.POIN'
    call jeveuo(appoin, 'E', jpoin)
    apinfp = sdappa(1:19)//'.INFP'
    call jeveuo(apinfp, 'E', jinfp)
    apnoms = sdappa(1:19)//'.NOMS'
    call jeveuo(apnoms, 'E', jpnoms)
!
! --- INITIALISATIONS
!
    nzoco = cfdisi(defico,'NZOCO')
!
! --- BOUCLE SUR LES ZONES
!
    ip = 1
    do izone = 1, nzoco
!
! ----- INFORMATION SUR LA ZONE
!
        call apzoni(sdappa, izone, 'NBPT', nbpt)
        call apzoni(sdappa, izone, 'NBNOE', nbnoe)
        call apzoni(sdappa, izone, 'JDECNE', jdecne)
!
! ----- POINTS DE CONTACT = NOEUDS ESCLAVES
!
        ASSERT(nbpt.eq.nbnoe)
!
! ----- BOUCLE SUR LES POINTS
!
        do inoe = 1, nbnoe
!
! ------- NUMERO ABSOLU DU NOEUD
!
            posnoe(1) = jdecne + inoe
            call cfnumn(defico, 1, posnoe(1), numnoe(1))
!
! ------- COORDONNEES DU NOEUD
!
            call cfcorn(newgeo, numnoe(1), coorpt)
            zr(jpoin + 3*(ip-1)+1-1) = coorpt(1)
            zr(jpoin + 3*(ip-1)+2-1) = coorpt(2)
            zr(jpoin + 3*(ip-1)+3-1) = coorpt(3)
!
! ------- NOEUD EXCLU ?
!
            call cfmmex(defico, 'CONT', izone, numnoe(1), suppok)
            zi(jinfp+ip-1) = suppok
!
! ------- NOM DU POINT
!
            call jenuno(jexnum(noma//'.NOMNOE', numnoe(1)), nomnoe)
            nompt = 'NOEUD   '//nomnoe
            zk16(jpnoms+ip-1) = nompt
!
! ------- POINT SUIVANT
!
            ip = ip + 1
        end do
    end do
!
    call jedema()
!
end subroutine
