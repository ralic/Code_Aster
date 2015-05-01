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
#include "asterf_types.h"
!
interface 
    subroutine erhmb2(perman, ino, nbs, ndim, theta,&
                      instpm, jac, nx, ny, tx,&
                      ty, nbcmp, geom, ivois, sielnp,&
                      sielnm, adsip, iagd, tbref2, iade2,&
                      iava2, ncmpm2, iaptm2, iade3, iava3,&
                      ncmpm3, iaptm3, tm2h1b)
        integer :: ndim
        aster_logical :: perman
        integer :: ino
        integer :: nbs
        real(kind=8) :: theta
        real(kind=8) :: instpm(2)
        real(kind=8) :: jac(3)
        real(kind=8) :: nx(3)
        real(kind=8) :: ny(3)
        real(kind=8) :: tx(3)
        real(kind=8) :: ty(3)
        integer :: nbcmp
        real(kind=8) :: geom(ndim, *)
        integer :: ivois
        real(kind=8) :: sielnp(140)
        real(kind=8) :: sielnm(140)
        integer :: adsip
        integer :: iagd
        integer :: tbref2(12)
        integer :: iade2
        integer :: iava2
        integer :: ncmpm2
        integer :: iaptm2
        integer :: iade3
        integer :: iava3
        integer :: ncmpm3
        integer :: iaptm3
        real(kind=8) :: tm2h1b(3)
    end subroutine erhmb2
end interface 
