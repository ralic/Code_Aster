function exisdg(dg, cmp)
! aslint: disable=
    implicit none
    logical(kind=1) :: exisdg
    integer :: dg(*), cmp
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
!     INDIQUE L'EXISTENCE D'1 CMP DANS UN DESCRIPTEUR-GRANDEUR DG
!     ------------------------------------------------------------------
!     EXTERNAL:
!     ---------
    integer :: iand, lshift
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: iec, reste, code
!
! DEB-------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    iec = (cmp-1)/30 + 1
    reste = cmp - 30* (iec-1)
    code = lshift(1,reste)
    exisdg = iand(dg(iec),code) .eq. code
!
end function
