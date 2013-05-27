function defaxe(icoq, imod, z, long, nbm,&
                tcoef)
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!-----------------------------------------------------------------------
! COUPLAGE FLUIDELASTIQUE, CONFIGURATIONS DU TYPE "COQUE_COAX"
! CALCUL DE LA DEFORMEE AXIALE AU POINT DE COTE Z SUR LA COQUE ICOQ POUR
! LE MODE IMOD
!-----------------------------------------------------------------------
!  IN : ICOQ  : INDICE CARACTERISTIQUE DE LA COQUE CONSIDEREE
!               ICOQ=1 COQUE INTERNE  ICOQ=2 COQUE EXTERNE
!  IN : IMOD  : INDICE DU MODE
!  IN : Z     : COTE
!  IN : LONG  : LONGUEUR DU DOMAINE DE RECOUVREMENT DES DEUX COQUES
!  IN : NBM   : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
!  IN : TCOEF : TABLEAU CONTENANT LES COEFFICIENTS DES DEFORMEES AXIALES
!-----------------------------------------------------------------------
    real(kind=8) :: defaxe
    integer :: icoq, imod, nbm
    real(kind=8) :: z, long, tcoef(10, nbm)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: itab
    real(kind=8) :: zz
!-----------------------------------------------------------------------
    itab = 0
    if (icoq .eq. 2) itab = 5
    zz = tcoef(1+itab,imod)*z/long
    defaxe = tcoef(2+itab,imod)*dble(cos(zz)) + tcoef(3+itab,imod)*dble(sin(zz)) + tcoef(4+itab,i&
             &mod)*dble(cosh(zz)) + tcoef(5+itab,imod)*dble(sinh(zz))
!
end function
