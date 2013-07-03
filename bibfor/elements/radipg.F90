subroutine radipg(sig1, sig2, npg, nbsig, radia,&
                  cosang, ind, compor, imate, nvi,&
                  vari1, vari2)
    implicit none
#include "asterc/r8prem.h"
#include "asterfort/nmcham.h"
#include "asterfort/norsig.h"
#include "asterfort/radial.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
    integer :: npg, nbsig, ind, nvi, imate
    real(kind=8) :: sig1(*), sig2(*), radia(*), cosang(*)
    real(kind=8) :: vari1(*), vari2(*)
    character(len=16) :: compor
! ----------------------------------------------------------------------
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
!
!     BUT:
!       CALCUL DE L'INDICATEUR LOCAL DE PERTE DE RADIALITE RADIA
!       I = 1- ABS(SIG1:DSIGMA)/(NORME(SIG1)*NORME(DSIGMA)
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   SIG1     : CONTRAINTES INSTANT -
! IN   SIG2     : CONTRAINTES INSTANT +
! IN   NPG      : NOMBRE DE POINT DE GAUSS
! IN   NBSIG    : NOMBRE DE CMP DE CONTR
! IN   IND      : 0 : CALCUL DE RADI_V, 1 : CALCUL DE ERR_RADI
! IN   COMPOR   : COMPORTEMENT
! IN   IMATE    : ADRESSE MATERIAU CODE
! IN   NVI      : NOMBRE DE VARIABLES INTERNES
! IN   VARI1    : VARIABLES INTERNES INSTANT -
! IN   VARI2    : VARIABLES INTERNES INSTANT +
!
!      SORTIE :
!-------------
! OUT  RADIA    : INDICATEUR DE PERTE DE RADIALITE
! OUT  COSANG   : COSINUS DE L'ANGLE
!
! ......................................................................
!
    integer :: mxcmel
    parameter (mxcmel=162)
!
    integer :: i, k, igau, icine, nbvar, memo, visc, iradi, idelta
!
    real(kind=8) :: dsigma(mxcmel), zero, deux, s1dsig, norm, dnorm, matel(20)
    real(kind=8) :: zernor, tensm(6), tensp(6), indm, indp, xm(6), xp(6)
    real(kind=8) :: coef, cinf, c2inf, mat(50)
!
! ----------------------------------------------------------------------
!
    zero = 0.0d0
    deux = 2.0d0
    zernor = 10.0d0*r8prem()
!
    if (ind .eq. 0) then
!
! ----    CALCUL DE DSIGMA = SIG2 - SIG1 :
!         ----------------------------------
        k = 0
        do 10 igau = 1, npg
            do 20 i = 1, nbsig
                k = k + 1
                dsigma(k) = sig2(k) - sig1(k)
!
20          continue
10      continue
!
! ----    CALCUL DE L'INDICATEUR LOCAL DE PERTE DE RADIALITE
! ----    AUX POINTS D'INTEGRATION :
!         ------------------------
        do 50 igau = 1, npg
!
! ----       CALCUL DU PRODUIT SIG1:(SIG2-SIG1) :
!            ----------------------------------------
            s1dsig = zero
            do 30 i = 1, 3
                s1dsig = s1dsig + sig1( i+ (igau-1)*nbsig)* dsigma(i+ ( igau-1)*nbsig)
30          continue
!
            do 40 i = 4, nbsig
                s1dsig = s1dsig + deux*sig1( i+ (igau-1)*nbsig)* dsigma(i+ (igau-1)*nbsig)
40          continue
!
! ----       CALCUL DU SECOND INVARIANT DES TENSEURS DES CONTRAINTES :
!            -------------------------------------------------------
            norm = norsig(sig1(1+ (igau-1)*nbsig),nbsig)
            dnorm = norsig(dsigma(1+ (igau-1)*nbsig),nbsig)
!
! ----       DANS LE CAS OU NORME(SIG1) = 0  OU NORME(DSIGMA) = 0 :
! ----       ON MET L'INDICATEUR A 0 :
!            -----------------------
            if (norm .le. zernor .or. dnorm .le. zernor) then
                radia(igau) = zero
                cosang(igau) = zero
            else if (dnorm.le.1.0d4*r8prem()*norm) then
                radia(igau) = zero
                cosang(igau) = zero
            else
                radia(igau) = 1.d0 - abs(s1dsig)/norm/dnorm
                cosang(igau) = s1dsig/norm/dnorm
            endif
50      continue
!
    else if (ind.eq.1) then
!
        do 51 igau = 1, npg
!
            iradi=0
            call dcopy(nbsig, sig1(1+(igau-1)*nbsig), 1, tensm, 1)
            call dcopy(nbsig, sig2(1+(igau-1)*nbsig), 1, tensp, 1)
            call dscal(nbsig-3, sqrt(2.d0), tensm(4), 1)
            call dscal(nbsig-3, sqrt(2.d0), tensp(4), 1)
!
!           ISOTROPE : LA NORMALE NE DEPEND QUE DE SIG
            if ((compor.eq.'VMIS_ISOT_TRAC') .or. ( compor.eq.'VMIS_ISOT_LINE') .or.&
                ( compor.eq.'VMIS_ISOT_PUIS')) then
                indm=vari1((igau-1)*nvi+2)
                indp=vari2((igau-1)*nvi+2)
                icine=0
                iradi=1
!
!           CINEMATIQUE : LA NORMALE DEPEND DE SIG ET X
                elseif ((compor.eq.'VMIS_ECMI_TRAC') .or.(&
            compor.eq.'VMIS_ECMI_LINE')) then
                call dcopy(nbsig, vari1((igau-1)*nvi+3), 1, xm, 1)
                call dcopy(nbsig, vari2((igau-1)*nvi+3), 1, xp, 1)
                indm=vari1((igau-1)*nvi+2)
                indp=vari2((igau-1)*nvi+2)
                icine=1
                iradi=1
                call dscal(nbsig-3, sqrt(2.d0), xm(4), 1)
                call dscal(nbsig-3, sqrt(2.d0), xp(4), 1)
!
            else if ((compor.eq.'VMIS_CINE_LINE')) then
                call dcopy(nbsig, vari1((igau-1)*nvi+1), 1, xm, 1)
                call dcopy(nbsig, vari2((igau-1)*nvi+1), 1, xp, 1)
                indm=vari1((igau-1)*nvi+7)
                indp=vari2((igau-1)*nvi+7)
                icine=1
                iradi=1
                call dscal(nbsig-3, sqrt(2.d0), xm(4), 1)
                call dscal(nbsig-3, sqrt(2.d0), xp(4), 1)
!
                elseif((compor.eq.'VMIS_CIN1_CHAB') .or. (&
            compor.eq.'VISC_CIN1_CHAB') .or. (&
            compor.eq.'VMIS_CIN2_CHAB') .or. (&
            compor.eq.'VMIS_CIN2_MEMO') .or. (&
            compor.eq.'VISC_CIN2_CHAB') .or. (&
            compor.eq.'VISC_CIN2_MEMO')) then
                call nmcham('RIGI', igau, 1, imate, compor,&
                            matel, mat, nbvar, memo, visc,&
                            idelta, coef)
!              approximation : on supose C constant
                cinf = mat(4)/1.5d0
                indm=vari1((igau-1)*nvi+2)
                indp=vari2((igau-1)*nvi+2)
                call dcopy(nbsig, vari1((igau-1)*nvi+3), 1, xm, 1)
                call dcopy(nbsig, vari2((igau-1)*nvi+3), 1, xp, 1)
                call dscal(nbsig, cinf, xm, 1)
                call dscal(nbsig, cinf, xp, 1)
                if (nbvar .eq. 2) then
                    c2inf = mat(9)/1.5d0
                    call daxpy(nbsig, c2inf, vari1((igau-1)*nvi+9), 1, xm,&
                               1)
                    call daxpy(nbsig, c2inf, vari2((igau-1)*nvi+9), 1, xp,&
                               1)
                endif
                icine=1
                iradi=1
                call dscal(nbsig-3, sqrt(2.d0), xm(4), 1)
                call dscal(nbsig-3, sqrt(2.d0), xp(4), 1)
!
!
            endif
!
!           CALCUL EFFECTUE UNIQUEMENT SI LE COMPORTEMENT LE PERMET
            if (iradi .eq. 1) then
                call radial(nbsig, tensm, tensp, indm, indp,&
                            icine, xm, xp, radia(igau))
                cosang(igau)=sqrt(abs(1.d0-radia(igau)*radia(igau)))
            else
                radia(igau)=0.d0
                cosang(igau)=0.d0
            endif
!
51      continue
!
    endif
end subroutine
