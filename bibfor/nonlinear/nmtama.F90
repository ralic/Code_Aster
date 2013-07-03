subroutine nmtama(fami, kpg, ksp, imate, instam,&
                  instap, matm, mat)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
!
#include "asterfort/rcvalb.h"
#include "asterfort/u2mess.h"
    integer :: kpg, ksp, imate
    character(len=*) :: fami
    real(kind=8) :: instam, instap, matm(3), mat(14)
!
! ----------------------------------------------------------------------
! TAHERI :  LECTURE DES CARACTERISTIQUES DU MATERIAU
! ----------------------------------------------------------------------
! IN   FAMI  FAMILLE DU POINT DE GAUSS
! IN  KPG    POINT DE GAUSS
! IN   KSP   SOUS-POINT DE GAUSS
! IN  IMATE  ADRESSE DU MATERIAU CODE
! IN  INSTAM INSTANT -
! IN  INSTAP INSTANT +
! OUT MATM   CARACTERISTIQUES (ELASTIQUES) EN T-
! OUT MAT    CARACTERISTIQUES (ELASTIQUES, PLASTIQUES, VISQUEUSES) EN T+
!             1 = TROISK            (ELASTICITE)
!             2 = DEUXMU            (ELASTICITE)
!             3 = ALPHA             (THERMIQUE)
!             4 = R_0               (ECROUISSAGE)
!             5 = ALPHA             (ECROUISSAGE)
!             6 = M                 (ECROUISSAGE)
!             7 = A                 (ECROUISSAGE)
!             8 = B                 (ECROUISSAGE)
!             9 = C1                (ECROUISSAGE)
!            10 = C_INF             (ECROUISSAGE)
!            11 = S                 (ECROUISSAGE)
!            12 = 1/N               (VISCOSITE)
!            13 = K/(DT)**1/N       (VISCOSITE)
!            14 = UN_SUR_M          (VISCOSITE)
! ----------------------------------------------------------------------
!
    logical :: visco
    character(len=8) :: nom(14)
    integer :: ok(14)
    real(kind=8) :: e, nu
!
    data nom / 'E','NU','ALPHA',&
     &           'R_0','ALPHA','M','A','B','C1','C_INF','S',&
     &           'N','UN_SUR_K','UN_SUR_M' /
!
!
! - LECTURE DES CARACTERISTIQUES ELASTIQUES DU MATERIAU (T- ET T+)
!
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                2, nom(1), matm(1), ok(1), 2)
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                1, nom(3), matm(3), ok(3), 0)
    if (ok(3) .ne. 0) matm(3) = 0.d0
    e = matm(1)
    nu = matm(2)
    matm(1) = e/(1.d0-2.d0*nu)
    matm(2) = e/(1.d0+nu)
!
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                2, nom(1), mat(1), ok(1), 2)
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                1, nom(3), mat(3), ok(3), 0)
    if (ok(3) .ne. 0) mat(3) = 0.d0
    e = mat(1)
    nu = mat(2)
    mat(1) = e/(1.d0-2.d0*nu)
    mat(2) = e/(1.d0+nu)
!
!
! - LECTURE DES CARACTERISTIQUES D'ECROUISSAGE (T+)
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'TAHERI', 0, ' ', 0.d0,&
                8, nom(4), mat(4), ok(4), 2)
    mat(7) = mat(7) * (2.d0/3.d0)**mat(5)
!
! LECTURE DES CARACTERISTIQUES DE VISCOSITE (TEMPS +)
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'LEMAITRE', 0, ' ', 0.d0,&
                3, nom(12), mat(12), ok(12), 0)
    visco = ok(12).eq.0
!
    if (visco) then
        if (mat(12) .eq. 0.d0) call u2mess('F', 'ALGORITH8_32')
        mat(12) = 1.d0 / mat(12)
!
        if (mat(13) .eq. 0.d0) call u2mess('F', 'ALGORITH8_33')
        mat(13) = 1.d0 / mat(13) / (instap-instam)**mat(12)
!
        if (ok(14) .ne. 0) mat(14) = 0.d0
!
    else
        mat(12) = 1.d0
        mat(13) = 0.d0
        mat(14) = 1.d0
    endif
!
!
end subroutine
