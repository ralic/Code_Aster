subroutine lcquma(mat, fami, kpg, ksp, poum)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterfort/rcvalb.h"
    integer,intent(in) :: mat, kpg, ksp
    character(len=1),intent(in) :: poum
    character(len=*),intent(in) :: fami
! --------------------------------------------------------------------------------------------------
!   ENDOMMAGEMENT FRAGILE A GRADIENT DE VARIABLE INTERNE ENDO_SCALAIRE
!            LECTURE DES PARAMETRES DU CRITERE QUADRATIQUE
! --------------------------------------------------------------------------------------------------
! IN  MAT    ADRESSE DU MATERIAU
! IN  FAMI   FAMILLE DE POINTS D'INTEGRATION (SI 'NONE', PAS DE TEMP.)
! IN  KPG    NUMERO DU POINT D'INTEGRATION
! IN  KSP    NUMERO DU SOUS-POINT
! IN  POUM   LECTURE DES PARAMETRES EN DEBUT '-' OU FIN '+' DU PAS
! --------------------------------------------------------------------------------------------------
    integer         :: iok(7)
    real(kind=8)    :: valer(7), coef, cc, cv, e, nu,rdum(1)
    character(len=8):: nomdum(1)
    character(len=8):: nomer(7)
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: lambda, deuxmu, troisk, rigmin, pc, pr, epsth
    common /lcee/ lambda,deuxmu,troisk,rigmin,pc,pr,epsth
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: pk, pm, pp, pq
    common /lces/ pk,pm,pp,pq
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: pct, pch, pcs
    common /lcmqu/ pch,pct,pcs
! --------------------------------------------------------------------------------------------------
    data nomer /'K','M','P','Q','COEF_RIG','C_VOLU','C_COMP'/
! --------------------------------------------------------------------------------------------------

    call rcvalb(fami, kpg, ksp, poum, mat,' ', 'ENDO_SCALAIRE', 0, nomdum, rdum, &
                7, nomer, valer, iok, 2)
!
    pk     = valer(1)
    pm     = valer(2)
    pp     = valer(3)
    pq     = valer(4)
    rigmin = valer(5)
    cv     = valer(6)
    cc     = valer(7)
!
    coef   = troisk/(2*deuxmu)
    nu     = (2*coef-1)/(4*coef+1)
    e      = deuxmu*(1+nu)
!
    pcs    = 0.5d0*e / ( (1-2*nu)*cc + sqrt(coef*cv*(1-2*nu)**2 + (1+nu)**2) )**2
    pch    = cv*coef*pcs
    pct    = cc*sqrt(pcs)
!
end subroutine
