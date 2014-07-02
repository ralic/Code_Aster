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
    subroutine dltini(lcrea, nume, result, depini, vitini,&
                      accini, fexini, famini, fliini, neq,&
                      numedd, inchac, baseno)
        aster_logical :: lcrea
        integer :: nume
        character(len=8) :: result
        real(kind=8) :: depini(*)
        real(kind=8) :: vitini(*)
        real(kind=8) :: accini(*)
        real(kind=8) :: fexini(*)
        real(kind=8) :: famini(*)
        real(kind=8) :: fliini(*)
        integer :: neq
        character(len=24) :: numedd
        integer :: inchac
        character(len=8) :: baseno
    end subroutine dltini
end interface
