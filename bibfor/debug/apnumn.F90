subroutine apnumn(defico, posno, numno)
!
    implicit      none
!
#include "asterfort/cfnumn.h"
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
    character(len=24), intent(in) :: defico
    integer, intent(in) :: posno(1)
    integer, intent(out) :: numno(1)
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! NUMERO ABSOLU DU NOEUD
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DEFINITION DU CONTACT
! IN  POSNO  : POSITION DU NOEUD DANS LES SD
! OUT NUMNO  : NUMERO ABSOLU DU NOEUD DANS LE MAILLAGE
!
!
! ----------------------------------------------------------------------
!
! --- REPONSE
!
    call cfnumn(defico, 1, posno, numno)
!
end subroutine
