subroutine cfrott(visc, rug, hmoy, umoy, cf0,&
                  mcf0)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! COUPLAGE FLUIDELASTIQUE, CONFIGURATIONS DU TYPE "COQUE_COAX"
! CALCUL DU COEFFICIENT DE FROTTEMENT VISQUEUX
!-----------------------------------------------------------------------
!  IN : VISC : VISCOSITE CINEMATIQUE DU FLUIDE
!  IN : RUG  : RUGOSITE ABSOLUE DE PAROI DES COQUES
!  IN : HMOY : JEU ANNULAIRE MOYEN
!  IN : UMOY : VITESSE DE L'ECOULEMENT MOYEN
! OUT : CF0  : COEFFICIENT DE FROTTEMENT VISQUEUX
! OUT : MCF0 : EXPOSANT VIS-A-VIS DU NOMBRE DE REYNOLDS
!-----------------------------------------------------------------------
    real(kind=8) :: visc, rug
    real(kind=8) :: hmoy, umoy, cf0, mcf0
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    real(kind=8) :: corr, re, rel
!-----------------------------------------------------------------------
    if (visc .lt. 1.d-10) then
        mcf0 = 0.d0
        cf0 = 0.d0
        goto 100
    endif
!
    re = 2.d0*hmoy*umoy/visc
    rel = (17.85d0/rug*2.d0*hmoy)**1.143d0
!
    corr = 1.5d0
!
    if (re .lt. 2000.d0) then
        mcf0 = -1.d0
        cf0 = 16.d0*(re**mcf0)*corr
    else if (re.lt.4000.d0) then
        mcf0 = 0.55d0
        cf0 = 1.2d-4*(re**mcf0)*corr
    else if (re.lt.100000.d0) then
        if (re .lt. rel) then
            mcf0 = -0.25d0
            cf0 = 0.079d0*(re**mcf0)*corr
        else
            mcf0 = 0.d0
            cf0 = 0.079d0*(rel**(-0.25d0))*corr
        endif
    else if (re.lt.rel) then
        mcf0 = -0.87d0/(dble(log10(re))-0.91d0)
        cf0 = 0.25d0/((1.8d0*dble(log10(re))-1.64d0)**2)*corr
    else
        mcf0 = 0.d0
        cf0 = 0.25d0/((1.8d0*dble(log10(rel))-1.64d0)**2)*corr
    endif
!
100  continue
end subroutine
