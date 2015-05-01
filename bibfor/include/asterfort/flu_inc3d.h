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
    subroutine flu_inc3d(e0i, e1i, e2i, ve1i, ve2i,&
                         k0, k1, h1, h2, vk0,&
                         vk1, vh1, vh2, depst, delta,&
                         e0f, e1f, e2f, dsigma, ve1f,&
                         ve2f, dissip)
        real(kind=8) :: e0i
        real(kind=8) :: e1i
        real(kind=8) :: e2i
        real(kind=8) :: ve1i
        real(kind=8) :: ve2i
        real(kind=8) :: k0
        real(kind=8) :: k1
        real(kind=8) :: h1
        real(kind=8) :: h2
        real(kind=8) :: vk0
        real(kind=8) :: vk1
        real(kind=8) :: vh1
        real(kind=8) :: vh2
        real(kind=8) :: depst
        real(kind=8) :: delta
        real(kind=8) :: e0f
        real(kind=8) :: e1f
        real(kind=8) :: e2f
        real(kind=8) :: dsigma
        real(kind=8) :: ve1f
        real(kind=8) :: ve2f
        real(kind=8) :: dissip
    end subroutine flu_inc3d
end interface 
