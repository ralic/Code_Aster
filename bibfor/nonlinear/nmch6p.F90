subroutine nmch6p(measse)
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
    implicit     none
#include "asterfort/nmcha0.h"
    character(len=19) :: measse(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! CREATION DES VARIABLES CHAPEAUX - MEASSE
!
! ----------------------------------------------------------------------
!
!
! OUT MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
!
! ----------------------------------------------------------------------
!
    character(len=19) :: masse, amort, rigid, sstru
!
    data amort ,masse     /'&&NMCH6P.AMORT','&&NMCH6P.MASSE'/
    data rigid ,sstru     /'&&NMCH6P.RIGID','&&NMCH6P.SSRASS'/
!
! ----------------------------------------------------------------------
!
    call nmcha0('MEASSE', 'ALLINI', ' ', measse)
    call nmcha0('MEASSE', 'MERIGI', rigid, measse)
    call nmcha0('MEASSE', 'MEMASS', masse, measse)
    call nmcha0('MEASSE', 'MEAMOR', amort, measse)
    call nmcha0('MEASSE', 'MESSTR', sstru, measse)
!
end subroutine
