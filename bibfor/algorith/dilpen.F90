subroutine dilpen(imate, rpena)
! ======================================================================
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
! ======================================================================
    implicit none
#include "asterfort/rcvalb.h"
    integer :: imate
    real(kind=8) :: rpena
! ======================================================================
! --- BUT : RECUPERATION DU COEFFICIENT DE PENALISATION ----------------
! ======================================================================
    real(kind=8) :: val(1)
    integer :: icodre(1), kpg, spt
    character(len=16) :: ncra
    character(len=8) :: fami, poum
! ======================================================================
! --- DEFINITION DES DONNEES INITIALES ---------------------------------
! ======================================================================
    data ncra  / 'PENA_LAGR' /
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    val(1) = 0.0d0
    call rcvalb(fami, kpg, spt, poum, imate,&
                ' ', 'NON_LOCAL', 0, ' ', [0.0d0],&
                1, ncra, val, icodre, 0, nan='NON')
    rpena = val(1)
! ======================================================================
end subroutine
