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
    subroutine cmlqma(nbmato, nbma, nbno, lima, typema,&
                      conniz, connoz, nofils, nbtyma, nomast,&
                      reftyp, nbref, impmai)
        integer :: nbma
        integer :: nbmato
        integer :: nbno
        integer :: lima(nbma)
        integer :: typema(*)
        character(len=*) :: conniz
        character(len=*) :: connoz
        integer :: nofils(12, *)
        integer :: nbtyma
        character(len=8) :: nomast(*)
        integer :: reftyp(*)
        integer :: nbref(*)
        integer :: impmai(*)
    end subroutine cmlqma
end interface
