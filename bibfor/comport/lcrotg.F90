subroutine lcrotg(indice, dp, e, dtaudf)
!
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
#include "asterfort/r8inir.h"
#include "asterfort/rcfonc.h"
    integer :: indice
    real(kind=8) :: dp, e(6), dtaudf(6, 3, 3)
!
! ***************************************************************
! *       INTEGRATION DE LA LOI DE ROUSSELIER LOCAL             *
! * CALCUL DE LA DERIVEE DE TAU PAR RAPPORT A DF = DTAUDF       *
! * TAU = TAU(TAU - DF)                                         *
! * TAU = TAU(E)                                                *
! * E = E(ETR)                                                  *
! * ETR =  ETR(DF)                                              *
! * DTAUDF = DTAU/DE * DE/DETR * DETR/DF + DTAU/DDF *
! ***************************************************************
!
! IN  INDICE : REGIME DE LA SOLUTION (ELASTIQUE, PLASTIQUE, SING)
! IN  DP     : INCREMENT DE DEFORMATION PLASTIQUE
! IN  E      : DEFORMATION ELASTIQUE
! OUT DTAUDF : DERIVEE DE TAU PAR RAPPORT A DF
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
    integer :: ij, kl, k, l, pq, rs
    real(kind=8) :: tre, rp, drdp, aire
    real(kind=8) :: al, al0, al1, al2, al3, al4, be1, be2, a1(6, 6), a2(6)
    real(kind=8) :: a3(6), sum
    real(kind=8) :: ddvetr(6, 6), dtretr(6)
    real(kind=8) :: dedetr(6, 6), dedfcd(6)
! ----------------------------------------------------------------------
!
!
! 1 - CALCUL DES DERIVEES SPECIFIQUES A LA LOI DE ROUSSELIER
!         DE / DETR = DEDETR
!         DE / DJ   = DEDJ
!
    call r8inir(36, 0.d0, dedetr, 1)
    call r8inir(6, 0.d0, dedfcd, 1)
    call rcfonc('V', 1, jprolp, jvalep, nbvalp,&
                p = pm+dp, rp = rp, rprim = drdp, airerp = aire)
    tre = e(1)+e(2)+e(3)
    select case (indice)
!
!
! 4.1 - CAS ELASTIQUE
!
    case (0)
        do ij = 1, 6
            dedetr(ij,ij) = 1.d0
        end do
!
!
!
! 4.2 CAS PLASTIQUE
!
    case (1)
!
! 4.2.1 - DERIVEE DE DVE PAR RAPPORT A DVETR ET DP
! DVE = DVE(DVETR - DP)
! DDVE/DDVETR = A1 ET DDVE/DDP = A2
!
        al = 1.d0-3.d0/2.d0*dp/eqetr
        do ij = 1, 6
            a2(ij) = -1.5d0*dvetr(ij)/eqetr
        end do
        do ij = 1, 6
            do kl = 1, 6
                a1(ij,kl) = 2.d0/3.d0*(1-al)*a2(ij)*a2(kl)
            end do
            a1(ij,ij) = a1(ij,ij) + al
        end do
!
!
! 4.2.2 - DERIVEE DE TRE PAR RAPPORT A TRETR, DP ET FCD
! TRE = TRE(TRETR - DP)
! DTRE/DTRETR = AL1, DTRE/DP = AL2, DTRE/DFCF = BE1
!
        al0 = exp(-unk*tre/sig1)*exp(-cother/sig1)
        al1 = 1.d0/(1.d0+fcd*unk*dp*al0/sig1)
        al2 = fcd*al0*al1
        be1 = dp*al0*al1
!
!
! 4.2.3 - DERIVEE DE DP PAR RAPPORT A DVETR ET TRETR
! DP = DP(DVETR - TRETR)
! DDP/DDVETR = A3, DDP/DTRETR = AL4, DDP/DFCD = BE2
!
        al3 = 1.d0/(3.d0*mu+drdp+fcd*unk*al2*al0)
        al4 = -unk*al2*al3
        do ij = 1, 6
            a3(ij) = -deuxmu*al3*a2(ij)
        end do
        be2 = al3*(sig1*al0-unk*fcd*al0*be1)
!
!
! 4.2.4 - DERIVEE DE E PAR RAPPORT A DVETR, TRETR ET FCD
! ASSEMBLAGE DE 4.1.1 ET 4.1.2 ET 4.1.3
! E = E(DVETR - TRETR)
! DE/DDVETR = DDVETR, DE/DTRETR = DTRETR, DE/DFCD = DEDFCD
!
        do ij = 1, 6
            do kl = 1, 6
                ddvetr(ij,kl) = a1(ij,kl) + (a2(ij)+al2/3.d0*kr(ij))*a3( kl)
            end do
            dtretr(ij) = al1*kr(ij)/3.d0 + al4*(a2(ij)+al2/3.d0*kr(ij))
            dedfcd(ij) = be1/3.d0*kr(ij) + be2*(a2(ij)+al2/3.d0*kr(ij))
        end do
!
!
! 4.2.5 - DERIVEE DE E PAR RAPPORT A ETR = DEDETR
!
        do ij = 1, 6
            do kl = 1, 6
                dedetr(ij,kl)=ddvetr(ij,kl)+(dtretr(ij)-al/3.d0*kr(ij))*&
            kr(kl)
            end do
        end do
!
!
!
! 4.3 CAS SINGULIER
!
    case (2)
!
! 4.3.1 - DERIVEE DE DVE PAR RAPPORT A DVETR, DP ET DFCD
! DVE = DVE(DVETR - DP)=0.D0
!
!
! 4.3.2 - DERIVEE DE TRE PAR RAPPORT A TRETR, DP ET DFCD
! TRE = TRE(TRETR - DP)
! DTRE/DTRETR = AL1, DTRE/DP = AL2 ET DTRE/DFCD = BE1
!
        al0 = exp(-unk*tre/sig1)*exp(-cother/sig1)
        al1 = 1.d0/(1.d0+fcd*unk*dp*al0/sig1)
        al2 = fcd*al0*al1
        be1 = dp*al0*al1
!
!
! 4.3.3 - DERIVEE DE DP PAR RAPPORT A DVETR, TRETR ET DFCD
! DP = DP(DVETR - TRETR)
! DDP/DDVETR = 0.D0, DDP/DTRETR = AL4 ET DDP/DFCD = BE2
!
        al3 = 1.d0/(drdp+al0*al2*fcd*unk)
        al4 = -unk*al2*al3
        be2 = al3*(sig1*al0-unk*fcd*al0*be1)
!
!
! 4.3.4 - DERIVEE DE E PAR RAPPORT A ETR ET FCD
!  DE/DETR = DEDETR, DE/DFCD = DEDFCD
!
        do ij = 1, 6
            do kl = 1, 6
                dedetr(ij,kl)= (al1+al2*al4)/3.d0*kr(ij)*kr(kl)
            end do
            dedfcd(ij) = (be1+al2*be2)/3.d0*kr(ij)
        end do
!
!
!
! 2 - COMPOSITION DES DERIVATIONS :
!     DTAUDF = DTAUDE * (DEDETR * DETRDF + DEDJ * DJDF)
!
    end select
    do ij = 1, 6
        do k = 1, 3
            do l = 1, 3
                sum = 0
                do pq = 1, 6
                    do rs = 1, 6
                        sum = sum + dtaude(ij,pq)*dedetr(pq,rs)* detrdf(rs,k,l)
                    end do
                    sum = sum + dtaude(ij,pq)*dedfcd(pq)*dfcddj*djdf( k,l)
                end do
                dtaudf(ij,k,l) = sum
            end do
        end do
    end do
!
end subroutine
