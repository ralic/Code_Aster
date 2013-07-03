subroutine cfcrje(defico, resoco)
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
#include "asterfort/cfdisi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - JEUX)
!
! CREATION DES SD POUR LES JEUX
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!
!
!
!
    integer :: ifm, niv
    integer :: ntpc
    character(len=24) :: jeuite, jeusav, jeux
    integer :: jjeuit, jjeusa, jjeux
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... CREATION DES SD POUR LES'//&
        ' JEUX'
    endif
!
! --- INFORMATIONS
!
    ntpc = cfdisi(defico,'NTPC' )
!
! --- JEUX ACTUALISES AU COURS DES ITERATIONS
!
    jeuite = resoco(1:14)//'.JEUITE'
    call wkvect(jeuite, 'V V R', 3*ntpc, jjeuit)
!
! --- JEUX AU DEBUT DU PAS DE TEMPS (SI REDECOUPAGE CF CFDECO/REAJEU)
!
    jeusav = resoco(1:14)//'.JEUSAV'
    call wkvect(jeusav, 'V V R', 3*ntpc, jjeusa)
!
! --- JEUX
!     1: JEU NORMAL SANS CORRECTION DU CONTACT
!     2: JEU TANGENT 1 DEPUIS LE DEBUT DU PAS DE TEMPS
!     3: JEU TANGENT 2 DEPUIS LE DEBUT DU PAS DE TEMPS
!
    jeux = resoco(1:14)//'.JEUX'
    call wkvect(jeux, 'V V R', 3*ntpc, jjeux)
!
    call jedema()
end subroutine
