subroutine calcfp(mutrbe, rprim, seuil, dt, dp,&
                  sigm0, epsi0, coefm, fplas, fprim,&
                  dfprim)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    real(kind=8) :: mutrbe, rprim, seuil, dt, dp, sigm0, epsi0, coefm
    real(kind=8) :: fplas, fprim, dfprim
!
!-------------------------------------------------------------------
!  FONCTION FPRIM(X) POUR LOI VISCOPLASTIQUE EN SINH
!  CALCUL DE DP ==> RESOLUTION DE L'EQUATION SCALAIRE NON LINEAIRE
!                   FPRIM + FPLAS + SIGM0* ARGSH((DP/DT/EPSI0)^(1/M))
!  IN MUTRBE : MU * TR(Bel)
!  IN RPRIM  : ECROUISSAGE
!  IN SEUIL  : VALEUR DE F=SIGEQ-SIGY - R
!  IN DT     : PAS DE TEMPS
!  IN  DP    : DP RECHERCHE LORS DE LA RESOLUTION SCALAIRE
!  IN SIGM0,EPSI0, COEFM : PARAMETRE LOI VISQUEUSE
!  OUT FPLAS : VALEUR DE LA COMPOSANTE PLASTIQUE DE FPRIM
!  OUT FPRIM : VALEUR DE FPRIM
!  OUT DFPRIM : DERIVEE DE FPRIM PAR RAPPORT A DP (SI DP>0)
!--------------------------------------------------------------------
!
    real(kind=8) :: r0, arg, asinh
!
!
    r0 = mutrbe + rprim
    fplas = seuil - r0*dp
    arg = (dp/dt/epsi0)**(1.d0/coefm)
    asinh = log(arg+sqrt(arg**2+1))
    fprim = fplas - sigm0 * asinh
!
!    CALCUL DE LA DERIVEE DE FPRIM
!
    if (dp .gt. 0.d0) then
        dfprim = - r0 + sigm0 / sqrt(arg**2+1.d0) / coefm / (dt*epsi0) **(1.d0/coefm) * dp**(1.d0&
                 &/coefm - 1)
    endif
!
end subroutine
