subroutine cfappa(noma, defico, resoco)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/apcalc.h"
#include "asterfort/cfapre.h"
#include "asterfort/cfpoin.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    character(len=8) :: noma
    character(len=24) :: defico, resoco
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
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!
!
!
!
    integer :: ifm, niv
    character(len=19) :: sdappa
    character(len=19) :: newgeo
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... DEBUT DE L''APPARIEMENT'
    endif
!
! --- INITIALISATIONS
!
    sdappa = resoco(1:14)//'.APPA'
    newgeo = resoco(1:14)//'.NEWG'
!
! --- REMPLISSAGE DE LA SD APPARIEMENT - POINTS (COORD. ET NOMS)
!
    call cfpoin(noma, defico, newgeo, sdappa)
!
! --- REALISATION DE L'APPARIEMENT
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ......... REALISATION DE ' //&
        'L''APPARIEMENT'
    endif
    call apcalc(sdappa)
!
! --- RECOPIE APPARIEMENT POUR CONTACT
!
    call cfapre(noma, defico, resoco, newgeo, sdappa)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... FIN DE L''APPARIEMENT'
    endif
!
    call jedema()
end subroutine
