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
    subroutine deffen(base, nuor, imodi, nbmr, nbm,&
                      iaxe, long, nbnfen, nofe, discfe,&
                      nbp1, nbp2, discff, defm)
        integer :: nbp2
        integer :: nbp1
        integer :: nbnfen
        integer :: nbm
        integer :: nbmr
        character(len=19) :: base
        integer :: nuor(nbm)
        integer :: imodi
        integer :: iaxe
        real(kind=8) :: long
        integer :: nofe(nbnfen)
        real(kind=8) :: discfe(nbnfen)
        real(kind=8) :: discff(nbp1+nbp2)
        real(kind=8) :: defm(nbp1+nbp2, nbmr)
    end subroutine deffen
end interface
