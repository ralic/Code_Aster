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
    subroutine mnlcor(imat, numdrv, matdrv, xcdl, parcho,&
                      adime, ninc, nd, nchoc, h,&
                      hf, itemax, epscor, xvect, cor, info)
        integer :: imat(2)
        character(len=14) :: numdrv
        character(len=19) :: matdrv
        character(len=14) :: xcdl
        character(len=14) :: parcho
        character(len=14) :: adime
        integer :: ninc
        integer :: nd
        integer :: nchoc
        integer :: h
        integer :: hf
        integer :: itemax
        integer :: info
        real(kind=8) :: epscor
        character(len=14) :: xvect
        aster_logical :: cor
    end subroutine mnlcor
end interface 
