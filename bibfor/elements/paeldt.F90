subroutine paeldt(kpg, ksp, fami, poum, icdmat,&
                  materi, em, ep, nup, depsth,&
                  tmoins, tplus, trefer)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterfort/rcvalb.h"
#include "asterfort/verift.h"
!
    integer :: kpg, ksp, icdmat
    character(len=1) :: poum
    character(len=4) :: fami
    character(len=8) :: materi
    real(kind=8) :: em, ep, num, nup, depsth
    real(kind=8), intent(out), optional :: tmoins, tplus, trefer
! --------------------------------------------------------------------------------------------------
!
!        CALCUL DES PARAMETRES ELASTIQUES ET DE LA
!        DEFORMATION THERMIQUE POUR UN SOUS-POINT DONNE
!
! --------------------------------------------------------------------------------------------------
    integer :: icodre(2)
    real(kind=8) :: valres(2), tpl, tms, tref
    character(len=16) :: nomres(2)
!
    nomres(1) = 'E'
    nomres(2) = 'NU'
!
    if ( poum.eq. 'T' ) then
        call rcvalb(fami, kpg, ksp, '-', icdmat,&
                    materi, 'ELAS', 0, ' ', [0.d0],&
                    2, nomres, valres, icodre, 1)
        em  = valres(1)
        num = valres(2)
!
        call rcvalb(fami, kpg, ksp, '+', icdmat,&
                    materi, 'ELAS', 0, ' ', [0.d0],&
                    2, nomres, valres, icodre, 1)
        ep  = valres(1)
        nup = valres(2)
!
        call verift(fami, kpg, ksp, 'T', icdmat,&
                    materi, epsth_= depsth,&
                    temp_prev_=tms, temp_curr_=tpl, temp_refe_=tref)
!
    else
        call rcvalb(fami, kpg, ksp, poum, icdmat,&
                    materi, 'ELAS', 0, ' ', [0.d0],&
                    2, nomres, valres, icodre, 1)
        ep  = valres(1)
        nup = valres(2)
        em  = valres(1)
        call verift(fami, kpg, ksp, poum, icdmat,&
                    materi, epsth_=depsth,&
                    temp_prev_=tms, temp_curr_=tpl, temp_refe_=tref)
    endif
!
    if (present(tmoins)) then
        tmoins = tms
    endif
    if (present(tplus)) then
        tplus = tpl
    endif
    if (present(trefer)) then
        trefer = tref
    endif
end subroutine
