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
    subroutine affono(valr, valk, desc, prnm, nbcomp,&
                      fonree, nomn, ino, nsurch, forimp,&
                      valfor, valfof, motcle, verif, nbec)
        integer :: nbcomp
        real(kind=8) :: valr(1)
        character(len=8) :: valk(1)
        integer :: desc
        integer :: prnm(1)
        character(len=4) :: fonree
        character(len=8) :: nomn
        integer :: ino
        integer :: nsurch
        integer :: forimp(nbcomp)
        real(kind=8) :: valfor(nbcomp)
        character(len=8) :: valfof(nbcomp)
        character(len=16) :: motcle(nbcomp)
        aster_logical :: verif
        integer :: nbec
    end subroutine affono
end interface
