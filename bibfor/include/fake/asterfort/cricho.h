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
    subroutine cricho(nbmode, riggen, nbchoc, parcho, noecho,&
                      info, fimpo, rfimpo, trloc, soupl,&
                      indic, neq, bmodal, seuil, marig,&
                      nbnli)
        integer :: nbnli
        integer :: neq
        integer :: nbmode
        real(kind=8) :: riggen(*)
        integer :: nbchoc
        real(kind=8) :: parcho(nbnli, *)
        character(len=8) :: noecho(nbnli, *)
        integer :: info
        real(kind=8) :: fimpo(neq)
        real(kind=8) :: rfimpo(neq)
        real(kind=8) :: trloc(nbmode)
        real(kind=8) :: soupl(nbmode)
        integer :: indic(nbmode)
        real(kind=8) :: bmodal(neq, nbmode)
        real(kind=8) :: seuil
        character(len=19) :: marig
    end subroutine cricho
end interface
