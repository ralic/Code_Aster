subroutine lcrofs(y, dp, s, ds)
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
    implicit none
#include "asterfort/rcfonc.h"
    real(kind=8) :: y, dp, s, ds
!
! *******************************************************
! *     INTEGRATION DE LA LOI DE ROUSSELIER LOCAL       *
! * CALCUL DE DP DE LA FONCTION S(Y) ET DE SA DERIVEE   *
! *******************************************************
!
! IN  Y       : PARAMETRE Y = K*X/SIG1
! OUT DP      : INCREMENT DE DEFORMATION PLASTIQUE CUMULEE
!                 DP = Y*SIG1*EXP(Y)/(FONC*K)
! OUT S       : VALEUR DE LA FONCTION S(Y)=-SIG1*FONC*EXP(-Y)+R
! OUT DS      : DERIVEE DS / DY
! ----------------------------------------------------------------------
!  COMMON LOI DE COMPORTEMENT ROUSSELIER
!
    integer :: itemax, jprolp, jvalep, nbvalp
    real(kind=8) :: prec, young, nu, sigy, sig1, rousd, f0, fcr, acce
    real(kind=8) :: pm, rpm, fonc, fcd, dfcddj, dpmaxi,typoro
    common /lcrou/ prec,young,nu,sigy,sig1,rousd,f0,fcr,acce,&
     &               pm,rpm,fonc,fcd,dfcddj,dpmaxi,typoro,&
     &               itemax, jprolp, jvalep, nbvalp
! ----------------------------------------------------------------------
!  COMMON GRANDES DEFORMATIONS CANO-LORENTZ
!
    integer :: ind1(6), ind2(6)
    real(kind=8) :: kr(6), rac2, rc(6)
    real(kind=8) :: lambda, mu, deuxmu, unk, troisk, cother
    real(kind=8) :: jm, dj, jp, djdf(3, 3)
    real(kind=8) :: etr(6), dvetr(6), eqetr, tretr, detrdf(6, 3, 3)
    real(kind=8) :: dtaude(6, 6)
!
    common /gdclc/&
     &          ind1,ind2,kr,rac2,rc,&
     &          lambda,mu,deuxmu,unk,troisk,cother,&
     &          jm,dj,jp,djdf,&
     &          etr,dvetr,eqetr,tretr,detrdf,&
     &          dtaude
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
    real(kind=8) :: rp, pente, r8bid, aire
!
    dp=y*exp(y)*sig1/(fonc*unk)
!
    call rcfonc('V', 1, jprolp, jvalep, nbvalp,&
                r8bid, r8bid, r8bid, pm+dp, rp,&
                pente, aire, r8bid, r8bid)
!
    s = -sig1*fonc*exp(-y) + rp
    ds = sig1*fonc*exp(-y) + pente*sig1*(1+y)*exp(y)/(unk*fonc)
!
end subroutine
