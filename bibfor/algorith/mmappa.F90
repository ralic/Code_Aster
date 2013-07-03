subroutine mmappa(loptin, noma, numedd, defico, resoco)
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
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mmapre.h"
#include "asterfort/mmpoin.h"
    logical :: loptin
    character(len=8) :: noma
    character(len=24) :: numedd
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT)
!
! ROUTINE PRINCIPALE POUR L'APPARIEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  LOPTIN : VAUT .TRUE. SI ACTIVATION DES OPTIONS *_INIT
! IN  NOMA   : NOM DU MAILLAGE
! IN  NUMEDD : NOM DU NUME_DDL
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!
! ----------------------------------------------------------------------
!
    character(len=19) :: sdappa
    character(len=19) :: newgeo
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... APPARIEMENT'
    endif
!
! --- ACCES SD CONTACT
!
    sdappa = resoco(1:14)//'.APPA'
    newgeo = resoco(1:14)//'.NEWG'
!
! --- REMPLISSAGE DE LA SD APPARIEMENT - POINTS (COORD. ET NOMS)
!
    call mmpoin(noma, defico, newgeo, sdappa)
!
! --- REALISATION DE L'APPARIEMENT
!
    call apcalc(sdappa)
!
! --- RECOPIE DE L'APPARIEMENT
!
    call mmapre(loptin, noma, numedd, defico, resoco,&
                sdappa)
!
    call jedema()
end subroutine
