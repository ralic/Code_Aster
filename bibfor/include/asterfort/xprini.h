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
    subroutine xprini(model, noma, cnxinv, grille, fispre,&
                      fiss, cnsln, cnslt, cnsgls, noesom,&
                      noresi, vcn, grlr, lcmin)
        character(len=8) :: model
        character(len=8) :: noma
        character(len=19) :: cnxinv
        logical(kind=1) :: grille
        character(len=8) :: fispre
        character(len=8) :: fiss
        character(len=19) :: cnsln
        character(len=19) :: cnslt
        character(len=19) :: cnsgls
        character(len=19) :: noesom
        character(len=19) :: noresi
        character(len=24) :: vcn
        character(len=24) :: grlr
        real(kind=8) :: lcmin
    end subroutine xprini
end interface
