!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine xprvir(fiss, covir, bavir, vitvir, angvir,&
                      numvir, numfon, nvit, nbeta, nbptff,&
                      radimp, radtor, damax, noma, locdom)
        character(len=8) :: fiss
        character(len=19) :: covir
        character(len=19) :: bavir
        character(len=19) :: vitvir
        character(len=19) :: angvir
        character(len=19) :: numvir
        integer :: numfon
        character(len=24) :: nvit
        character(len=24) :: nbeta
        integer :: nbptff
        real(kind=8) :: radimp
        real(kind=8) :: radtor
        real(kind=8) :: damax
        character(len=8) :: noma
        aster_logical :: locdom
    end subroutine xprvir
end interface
