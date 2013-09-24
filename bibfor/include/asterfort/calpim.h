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
interface
    subroutine calpim(graexc, excmod, napexc, nbmode, tymmec,&
                      mtrmas, numer, nbddl, noexit, cpexit,&
                      nvasex, vecass)
        character(len=16) :: graexc
        character(len=4) :: excmod
        integer :: napexc
        integer :: nbmode
        character(len=8) :: tymmec
        character(len=8) :: mtrmas
        character(len=8) :: numer
        integer :: nbddl
        character(len=8) :: noexit(*)
        character(len=8) :: cpexit(*)
        integer :: nvasex
        character(len=8) :: vecass(*)
    end subroutine calpim
end interface
