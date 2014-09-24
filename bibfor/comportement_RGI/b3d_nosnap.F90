subroutine b3d_nosnap(istep, fr, rtref, dpic, gf,&
                      e, beta, gama, li, vref,&
                      xkweib, veq, rteff, errmesh, coeff3,&
                      coefft)
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
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!=====================================================================
!          calcul de la taille maximale de l element  et de la resistanc
!     donnees materiaux:fr,rtref,dpic,Gf,E,beta,gama
!     exposant de Weibull pour la contrainte
!
!     pour assurer le non snp back en traction et la condition proba loc
!=====================================================================
    implicit none
    integer :: istep
    real(kind=8) :: fr
    real(kind=8) :: rtref
    real(kind=8) :: dpic
    real(kind=8) :: gf
    real(kind=8) :: e
    real(kind=8) :: beta
    real(kind=8) :: gama
    real(kind=8) :: li
    real(kind=8) :: vref
    real(kind=8) :: xkweib
    real(kind=8) :: veq
    real(kind=8) :: rteff
    real(kind=8) :: errmesh
    real(kind=8) :: coeff3
    real(kind=8) :: coefft, gf2, xgf, rtmeca, xlimax, t17, t10, gf1, rtproba, xwb
    if (istep .ne. 0) then
!       methode WL2 : effet d echelle probabiliste
!       resistance probabiliste maxi
!       rem: en deterministe veq=max(vi,vref)
        xwb=1.d0/xkweib
        if (veq .le. 0.) then
            print*,'donnee incoherente dans b3d_nosnap'
            print*,'V equivalent ne peut etre < ou = a 0'
            read*
        end if
!       en local veq=vref et coefft=1 -> Rtproba=Rtref
!       coefft modifie l energie de fissuration egalement cf ci dessous
        rtproba=(rtref*coefft)*(vref/veq)**(xwb)
!
!       modification  probabiliste l energie de fissuration pour
!       conserver la la fragilite local
!       en calcul local coeff3 sera egal a 1 et l energie sera conservee
        coeff3=(rtproba/rtref)**2
!
!       print*, 'modif probabiliste de Rt',rtproba/rtref, ' de Gf',coeff
!       si on veut gft independant de la taille de la zone chargéee il
        gf1=gf*coeff3
    else
!       calcul deterministe avec priorite sur rt sur les grand elements
        rtproba=rtref
        coeff3=1.d0
        gf1=gf
    end if
!
!     condition mecanique pour ne pas avoir de snap back dans les gros e
    t10 = gama * fr
!     t17 = rtref ** 2
!     on prend en compte la modif probabiliste de rt ds la condition de
    t17 = rtproba ** 2
!     taille maximale de l element pour conserver rt
    xlimax = -0.6d1 * (-0.1d1 + dpic) * gf1* e * fr / (fr + 0.2d1 * beta + 0.2d1 * gama - 0.4d1 *&
             & fr * beta - 0.2d1 * gama * beta + 0.4d1 * t10 * beta - 0.2d1 + 0.2d1 * t10) / t17
!     resistance maximale pour conserver li et gf
    rtmeca=sqrt(xlimax*t17/li)
!     multiplicateur de l energie de fissuration mini pour conserver rt
!     si les mailles sont trop grandes
    xgf=max(li/xlimax,1.d0)
!
!     rt et gf a adopter avec priorite a rt
    rteff=rtproba
!     print*,rteff,li,veq
!     modification eventuelle de gf pour ne pas avoir de snap back local
    gf2=gf1*xgf
    coeff3=gf2/gf
!
!     erreur relative sur gf induite par les mailles trop grandes
    errmesh=abs(gf2-gf1)/gf1
!
!      print*,'li',li,'rt ref',rtref
!      print*,'limax',xlimax
!      print*,'rtmeca',rtmeca
!      print*,'rtproba',rtproba
!      print*,'volume sollicite',veq
!      print*,'resistance adoptee',rteff
!      print*,'erreur maillage',errmesh
end subroutine
