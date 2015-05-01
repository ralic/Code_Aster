subroutine nmch3p(meelem)
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
    character(len=19) :: meelem(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! CREATION DES VARIABLES CHAPEAUX - MEELEM
!
! ----------------------------------------------------------------------
!
!
! OUT MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
!
! ----------------------------------------------------------------------
!
    character(len=19) :: memass, meamor, merigi, mediri, mesuiv
    character(len=19) :: messtr, megeom
    character(len=19) :: meeltc, meeltf
!
    data memass,mediri    /'&&NMCH3P.MEMASS','&&NMCH3P.MEDIRI'/
    data merigi,meamor    /'&&NMCH3P.MERIGI','&&NMCH3P.MEAMOR'/
    data mesuiv,messtr    /'&&NMCH3P.MATGME','&&NMCH3P.SSRELE'/
    data megeom           /'&&NMCH3P.MEGEOM'/
    data meeltc,meeltf    /'&&NMCH3P.MEELTC','&&NMCH3P.MEELTF'/
!
! ----------------------------------------------------------------------
!
    call nmcha0('MEELEM', 'ALLINI', ' ', meelem)
    call nmcha0('MEELEM', 'MERIGI', merigi, meelem)
    call nmcha0('MEELEM', 'MEDIRI', mediri, meelem)
    call nmcha0('MEELEM', 'MEMASS', memass, meelem)
    call nmcha0('MEELEM', 'MEAMOR', meamor, meelem)
    call nmcha0('MEELEM', 'MESUIV', mesuiv, meelem)
    call nmcha0('MEELEM', 'MESSTR', messtr, meelem)
    call nmcha0('MEELEM', 'MEGEOM', megeom, meelem)
    call nmcha0('MEELEM', 'MEELTC', meeltc, meelem)
    call nmcha0('MEELEM', 'MEELTF', meeltf, meelem)
!
end subroutine
