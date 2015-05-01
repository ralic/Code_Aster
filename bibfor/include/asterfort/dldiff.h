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
    subroutine dldiff(result, force1, lcrea, lamort, neq,&
                      imat, masse, rigid, amort, dep0,&
                      vit0, acc0, fexte, famor, fliai,&
                      t0, nchar, nveca, liad, lifo,&
                      modele, mate, carele, charge, infoch,&
                      fomult, numedd, nume, solveu, numrep)
        character(len=8) :: result
        character(len=19) :: force1
        aster_logical :: lcrea
        aster_logical :: lamort
        integer :: neq
        integer :: imat(*)
        character(len=8) :: masse
        character(len=8) :: rigid
        character(len=8) :: amort
        real(kind=8) :: dep0(*)
        real(kind=8) :: vit0(*)
        real(kind=8) :: acc0(*)
        real(kind=8) :: fexte(*)
        real(kind=8) :: famor(*)
        real(kind=8) :: fliai(*)
        real(kind=8) :: t0
        integer :: nchar
        integer :: nveca
        integer :: liad(*)
        character(len=24) :: lifo(*)
        character(len=24) :: modele
        character(len=24) :: mate
        character(len=24) :: carele
        character(len=24) :: charge
        character(len=24) :: infoch
        character(len=24) :: fomult
        character(len=24) :: numedd
        integer :: nume
        character(len=19) :: solveu
        integer :: numrep
    end subroutine dldiff
end interface
