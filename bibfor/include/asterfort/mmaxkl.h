!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine mmaxkl(latabl, modele, thetai, mate, compor,&
                      symech, chfond, nnoff, basloc, courb, &
                      ndeg, liss, pair, &
                      ndimte, nbprup, noprup, fiss, lonvec, &
                      ivec, resuco, lmelas, lncas, lord, &
                      milieu, connex, lischa, coor, iadnoe)
        integer :: lonvec
        character(len=8) :: latabl
        character(len=8) :: modele
        character(len=8) :: thetai
        character(len=24) :: mate
        character(len=24) :: compor
        character(len=8) :: symech
        character(len=24) :: chfond
        integer :: nnoff
        character(len=24) :: basloc
        character(len=24) :: courb
        integer :: ndeg
        character(len=24) :: liss
        aster_logical :: pair
        integer :: ndimte
        integer :: nbprup
        character(len=16) :: noprup(*)
        character(len=8) :: fiss
        integer :: ivec
        character(len=8) :: resuco
        aster_logical :: lmelas
        aster_logical :: lncas
        aster_logical :: lord(lonvec)
        aster_logical :: milieu
        aster_logical :: connex
        character(len=19) :: lischa              
        integer :: coor
        integer :: iadnoe
    end subroutine mmaxkl
end interface
