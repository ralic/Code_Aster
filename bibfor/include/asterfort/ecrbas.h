!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine ecrbas(nbsauv, nbnl, nbmode, depgen, vitgen,&
                      accgen, temps, jordre, ptemp, depbut,&
                      vitbut, forbut, redepg, revitg, reaccg,&
                      retemp, reordr, reptem, redepb, revitb,&
                      reforb)
        integer :: nbmode
        integer :: nbnl
        integer :: nbsauv
        real(kind=8) :: depgen(nbmode, *)
        real(kind=8) :: vitgen(nbmode, *)
        real(kind=8) :: accgen(nbmode, *)
        real(kind=8) :: temps(*)
        integer :: jordre(*)
        real(kind=8) :: ptemp(*)
        real(kind=8) :: depbut(nbnl, 3, *)
        real(kind=8) :: vitbut(nbnl, 3, *)
        real(kind=8) :: forbut(nbnl, 3, *)
        real(kind=8) :: redepg(*)
        real(kind=8) :: revitg(*)
        real(kind=8) :: reaccg(*)
        real(kind=8) :: retemp(*)
        integer :: reordr(*)
        real(kind=8) :: reptem(*)
        real(kind=8) :: redepb(*)
        real(kind=8) :: revitb(*)
        real(kind=8) :: reforb(*)
    end subroutine ecrbas
end interface
