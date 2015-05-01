subroutine dydome(nomo, mate, carele)
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
!
    implicit      none
#include "asterfort/ledome.h"
    character(len=8) :: nomo
    character(len=24) :: mate, carele
!
! ----------------------------------------------------------------------
!
! DYNA_LINE_HARM
!
! LECTURE DONNEES MECANIQUES
!
! ----------------------------------------------------------------------
!
!
! OUT NOMO   : MODELE
! OUT MATE   : MATERIAU CODE
! OUT CARELE : CARACTERISTIQUES ELEMENTAIRES
!
! ----------------------------------------------------------------------
!
    character(len=8) :: materi
!
! ----------------------------------------------------------------------
!
!
! --- LECTURE DONNNEES MECANIQUES
!
    call ledome('NN', nomo, materi, mate, carele)
!
end subroutine
