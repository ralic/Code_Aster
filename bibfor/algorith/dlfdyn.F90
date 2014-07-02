subroutine dlfdyn(rigid, amort, lamort, neq, d0,&
                  v0, f, f0)
    implicit none
#include "asterf_types.h"
#include "asterfort/mrmult.h"
#include "blas/daxpy.h"
    real(kind=8) :: d0(*), v0(*), f(*), f0(*)
    integer :: rigid, amort, neq
    aster_logical :: lamort
!
!**********************************************************************
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   BUT :      CALCUL DU VECTEUR FORCE DYNAMIQUE
!
!              F  = F   -  K D0 - C V0
!  ======
!
!   INPUT:
!   ---> RIGID   : POINTEUR DE LA MATRICE RIGIDITE
!   ---> AMORT   : POINTEUR DE LA MATRICE AMORTISSEMENT
!   ---> LAMORT  : VARIABLE LOGIQUE
!                     .TRUE. SI IL Y A UNE MATRICE AMORTISSEMENT
!                     .FALSE. SINON
!   ---> NEQ   : NOMBRE D'EQUATIONS (D.D.L. ACTIFS)
!   ---> D0    : VECTEUR DEPLACEMENT INITIAL (NEQ)
!   ---> V0    : VECTEUR VITESSE INITIAL  (NEQ)
!   ---> F0    : VECTEUR REEL DE TRAVAIL  (NEQ)
!
!   VAR   :
!   <--> F     : VECTEUR FORCE EXTERIEURE ENTREE (NEQ)
!                VECTEUR FORCE DYNAMIQUE SORTIE (NEQ)
!
!----------------------------------------------------------------------
    real(kind=8) :: mun
!
    mun = -1.d0
    call mrmult('ZERO', rigid, d0, f0, 1,&
                .true._1)
    call daxpy(neq, mun, f0, 1, f,&
               1)
    if (lamort) then
        call mrmult('ZERO', amort, v0, f0, 1,&
                    .true._1)
        call daxpy(neq, mun, f0, 1, f,&
                   1)
    endif
end subroutine
