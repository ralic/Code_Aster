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
    subroutine ecrgen(iordre, nbmode, tc, dt, depg,&
                      vitg, accg, depgen, vitgen, accgen,&
                      temps, jordre, ptemps)
        integer :: nbmode
        integer :: iordre
        real(kind=8) :: tc
        real(kind=8) :: dt
        real(kind=8) :: depg(*)
        real(kind=8) :: vitg(*)
        real(kind=8) :: accg(*)
        real(kind=8) :: depgen(nbmode, *)
        real(kind=8) :: vitgen(nbmode, *)
        real(kind=8) :: accgen(nbmode, *)
        real(kind=8) :: temps(*)
        integer :: jordre(*)
        real(kind=8) :: ptemps(*)
    end subroutine ecrgen
end interface
