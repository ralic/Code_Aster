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
    subroutine utites(label1, label2, type, nbref, refi,&
                      refr, refc, vali, valr, valc,&
                      epsi, crit, ific, llab, ssigne)
        integer :: nbref
        character(*) :: label1
        character(*) :: label2
        character(*) :: type
        integer :: refi(nbref)
        real(kind=8) :: refr(nbref)
        complex(kind=8) :: refc(nbref)
        integer :: vali
        real(kind=8) :: valr
        complex(kind=8) :: valc
        real(kind=8) :: epsi
        character(*) :: crit
        integer :: ific
        logical :: llab
        character(*) :: ssigne
    end subroutine utites
end interface
