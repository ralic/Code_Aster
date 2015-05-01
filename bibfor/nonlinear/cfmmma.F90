subroutine cfmmma(defico, resoco)
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
    implicit none
#include "jeveux.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES)
!
! CREATION SD DE RESOLUTION
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
!
!
!
!
    integer :: ifm, niv
    integer :: ntpc, ntnoec, nzoco
    character(len=24) :: jeusup, ctevco, ctevpe
    integer :: jjsup, jctevc, jctevp
    integer :: zeven
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... CREATION DES SD POUR LES '//&
        ' FORMULATIONS MAILLEES'
    endif
!
! --- INITIALISATIONS
!
    ntpc = cfdisi(defico,'NTPC' )
    ntnoec = cfdisi(defico,'NTNOEC')
    nzoco = cfdisi(defico,'NZOCO' )
    zeven = cfmmvd('ZEVEN')
!
! --- JEU SUPPLEMENTAIRE (DIST_*)
!
    jeusup = resoco(1:14)//'.JSUPCO'
    call wkvect(jeusup, 'V V R', ntpc, jjsup)
!
! --- INFORMATIONS POUR L'EVENT-DRIVEN - COLLISION
!
    ctevco = resoco(1:14)//'.EVENCO'
    call wkvect(ctevco, 'V V R', zeven*ntpc, jctevc)
!
! --- INFORMATIONS POUR L'EVENT-DRIVEN - INTERPENETRATION
!
    ctevpe = resoco(1:14)//'.EVENPE'
    call wkvect(ctevpe, 'V V R', 3*nzoco, jctevp)
!
! --- AFFICHAGE
!
    call utmess('I', 'MECANONLINE6_5', si=ntnoec)
!
    call jedema()
end subroutine
