subroutine chauxi(ndim, mu, ka, r, t,&
                  invp, lcour, courb, du1dm, du2dm,&
                  du3dm, u1l, u2l, u3l)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
#include "asterc/r8depi.h"
    integer :: ndim
    real(kind=8) :: mu, ka, r, t, invp(3, 3), courb(3, 3, 3)
    real(kind=8) :: du1dm(3, 3), du2dm(3, 3), du3dm(3, 3)
    real(kind=8) :: u1l(3), u2l(3), u3l(3)
    logical :: lcour
!
!     BUT : CALCUL DES CHAMPS AUXILIAIRES ET DE LEURS DÉRIVÉES EN (R,T)
!       (POUR LE CALCUL ENERGÉTIQUE DES SIFS EN MÉCANIQUEDE LA RUPTURE)
!
!
!   IN
!     MU    :  2ÈME COEFFICIENT DE LAMÉ
!     KA    :  KAPPA
!     R,T   :  COORDONNÉES POLAIRES DU POINT
!     INVP  :  INVERSE DE LA MATRICE DE PASSAGE LOCALE GLOBALE
!     LCOUR :  PRISE EN COMPTE DE LA COURBURE DE LA BASE LOCALE
!     COURB :  COURBURE DE LA BASE LOCALE
!
!   OUT
!     DU1DM :  DÉRIVÉES DU CHAMP SINGULIER AUXILIAIRE 1
!     DU2DM :  DÉRIVÉES DU CHAMP SINGULIER AUXILIAIRE 2
!     DU3DM :  DÉRIVÉES DU CHAMP SINGULIER AUXILIAIRE 3
!     U1L   :  CHAMP SINGULIER AUXILIAIRE 1 (DANS LA BASE LOCALE)
!     U2L   :  CHAMP SINGULIER AUXILIAIRE 2 (DANS LA BASE LOCALE)
!     U3L   :  CHAMP SINGULIER AUXILIAIRE 3 (DANS LA BASE LOCALE)
!
    integer :: i, j, k, l
    real(kind=8) :: du1dpo(3, 2), du2dpo(3, 2), du3dpo(3, 2)
    real(kind=8) :: du1dl(3, 3), du2dl(3, 3), du3dl(3, 3), cr1, cr2
!
!
!     COEFFS  DE CALCUL
    cr1=1.d0/(4.d0*mu*sqrt(r8depi()*r))
    cr2=sqrt(r/r8depi())/(2.d0*mu)
!
!-----------------------------------------------------------------------
!     DÉFINITION DU CHAMP SINGULIER AUXILIAIRE U1 ET DE SA DÉRIVÉE
!-----------------------------------------------------------------------
!     CHAMP SINGULIER AUXILIAIRE U1 DANS LA BASE LOCALE
    u1l(1)=cr2*cos(t*0.5d0)*(ka-cos(t))
    u1l(2)=cr2*sin(t*0.5d0)*(ka-cos(t))
    u1l(3)=0.d0
!
!     MATRICE DES DÉRIVÉES DE U1 DANS LA BASE POLAIRE (3X2)
!
!     DERIVÉES PAR RAPPORT À R (RAYON) DE U1
    du1dpo(1,1)=cr1*(cos(t*0.5d0)*(ka-cos(t)))
    du1dpo(2,1)=cr1*(sin(t*0.5d0)*(ka-cos(t)))
    du1dpo(3,1)=0.d0
!
!     DERIVÉES PAR RAPPORT À T (THETA) DE U1
    du1dpo(1,2)=cr2*(-0.5d0*sin(t*0.5d0)*(ka-cos(t))&
     &                                 + cos(t*0.5d0)*sin(t))
    du1dpo(2,2)=cr2*(0.5d0*cos(t*0.5d0)*(ka-cos(t))&
     &                                 + sin(t*0.5d0)*sin(t))
    du1dpo(3,2)=0.d0
!
!     MATRICE DES DÉRIVÉES DE U1 DANS LA BASE LOCALE (3X3)
    do 140 i = 1, 3
        du1dl(i,1)=cos(t)*du1dpo(i,1)-sin(t)/r*du1dpo(i,2)
        du1dl(i,2)=sin(t)*du1dpo(i,1)+cos(t)/r*du1dpo(i,2)
        du1dl(i,3)=0.d0
140  end do
!
!     MATRICE DES DÉRIVÉES DE U1 DANS LA BASE GLOBALE (3X3)
    do 141 i = 1, ndim
        do 142 j = 1, ndim
            do 143 k = 1, ndim
                do 144 l = 1, ndim
                    du1dm(i,j)=du1dm(i,j)+du1dl(k,l)*invp(l,j)*invp(k,&
                    i)
144              continue
!           PRISE EN COMPTE DE LA BASE MOBILE
                if (lcour) du1dm(i,j)=du1dm(i,j)+u1l(k)*courb(k,i,j)
143          continue
142      continue
141  end do
!
!-----------------------------------------------------------------------
!     DÉFINITION DU CHAMP SINGULIER AUXILIAIRE U2 ET DE SA DÉRIVÉE
!-----------------------------------------------------------------------
!     CHAMP SINGULIER AUXILIAIRE U2 DANS LA BASE LOCALE
    u2l(1)=cr2*sin(t*0.5d0)*(ka+2.d0+cos(t))
    u2l(2)=cr2*cos(t*0.5d0)*(2.d0-ka-cos(t))
    u2l(3)=0.d0
!
!     MATRICE DES DÉRIVÉES DE U2 DANS LA BASE POLAIRE (3X2)
    du2dpo(1,1)=cr1*(sin(t*0.5d0)*(ka+2.d0+cos(t)))
    du2dpo(2,1)=cr1*cos(t*0.5d0)*(2.d0-ka-cos(t))
    du2dpo(3,1)=0.d0
    du2dpo(1,2)=cr2*(0.5d0*cos(t*0.5d0)*(ka+2.d0+cos(t))&
     &                               - sin(t*0.5d0)*sin(t))
    du2dpo(2,2)=cr2*(-0.5d0*sin(t*0.5d0)*(2.d0-ka-cos(t))&
     &                               + cos(t*0.5d0)*sin(t))
    du2dpo(3,2)=0.d0
!
!     MATRICE DES DÉRIVÉES DE U2 DANS LA BASE LOCALE (3X3)
    do 150 i = 1, 3
        du2dl(i,1)=cos(t)*du2dpo(i,1)-sin(t)/r*du2dpo(i,2)
        du2dl(i,2)=sin(t)*du2dpo(i,1)+cos(t)/r*du2dpo(i,2)
        du2dl(i,3)=0.d0
150  end do
!
!     MATRICE DES DÉRIVÉES DE U2 DANS LA BASE GLOBALE (3X3)
    do 151 i = 1, ndim
        do 152 j = 1, ndim
            do 153 k = 1, ndim
                do 154 l = 1, ndim
                    du2dm(i,j)=du2dm(i,j)+du2dl(k,l)*invp(l,j)*invp(k,&
                    i)
154              continue
!           PRISE EN COMPTE DE LA BASE MOBILE
                if (lcour) du2dm(i,j)=du2dm(i,j)+u2l(k)*courb(k,i,j)
153          continue
152      continue
151  end do
!
!-----------------------------------------------------------------------
!     DÉFINITION DU CHAMP SINGULIER AUXILIAIRE U3 ET DE SA DÉRIVÉE
!-----------------------------------------------------------------------
!     CHAMP SINGULIER AUXILIAIRE U3 DANS LA BASE LOCALE
    u3l(1)=0.d0
    u3l(2)=0.d0
    u3l(3)=4.d0*cr2*sin(t*0.5d0)
!
!     MATRICE DES DÉRIVÉES DE U3 DANS LA BASE POLAIRE (3X2)
    du3dpo(1,1)=0.d0
    du3dpo(2,1)=0.d0
    du3dpo(1,2)=0.d0
    du3dpo(2,2)=0.d0
    du3dpo(3,1)=4.d0*cr1*sin(t*0.5d0)
    du3dpo(3,2)=2.d0*cr2*cos(t*0.5d0)
!
!     MATRICE DES DÉRIVÉES DE U3 DANS LA BASE LOCALE (3X3)
    do 160 i = 1, 3
        du3dl(i,1)=cos(t)*du3dpo(i,1)-sin(t)/r*du3dpo(i,2)
        du3dl(i,2)=sin(t)*du3dpo(i,1)+cos(t)/r*du3dpo(i,2)
        du3dl(i,3)=0.d0
160  end do
!
!     MATRICE DES DÉRIVÉES DE U3 DANS LA BASE GLOBALE (3X3)
    do 161 i = 1, ndim
        do 162 j = 1, ndim
            do 163 k = 1, ndim
                do 164 l = 1, ndim
                    du3dm(i,j)=du3dm(i,j)+du3dl(k,l)*invp(l,j)*invp(k,&
                    i)
164              continue
!           PRISE EN COMPTE DE LA BASE MOBILE
                if (lcour) du3dm(i,j)=du3dm(i,j)+u3l(k)*courb(k,i,j)
163          continue
162      continue
161  end do
!
end subroutine
