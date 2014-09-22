subroutine coef_infl(prodef, londef, lrev, matrev, matmdb,&
                     tempa, tempb, coeinf )
    implicit none
    real(kind=8) :: prodef, londef, lrev
    real(kind=8) :: tempa, tempb
    real(kind=8) :: coeinf(15)
    character(len=8) :: matrev, matmdb
!
#include "asterfort/coef_infl_a1.h"
#include "asterfort/coef_infl_b1.h"
#include "asterfort/coef_infl_b2.h"
#include "asterfort/coef_infl_c1.h"
#include "asterfort/coef_infl_c2.h"
#include "asterfort/rcvale.h"
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! --- BUT : CALCUL DES COEFFICIENTS D INFLUENCE
! ======================================================================
! IN  : PRODEF : PROFONDEUR DU DEFAUT ----------------------------------
! --- : LONDEF : LONGUEUR DU DEFAUT ------------------------------------
! --- : LREV   : LONGUEUR DU REVETEMENT --------------------------------
! --- : MATREV : MATERIAU DU REVETEMENT --------------------------------
! --- : MATMDB : MATERIAU DU METAL DE BASE -----------------------------
! --- : TEMPA  : TEMPERATURE EN POINTE C -------------------------------
! --- : TEMPB  : TEMPERATURE EN POINTE B -------------------------------
! OUT : COEINF : COEFFICIENTS D INFLUENCE AUX POINTS A, B et C ---------
!                5 COEFFICIENTS PAR POINT
! ======================================================================
! ======================================================================
!          RSE-M - Edition 2010
!               ANNEXE 5.4
!   METHODES ANALYTIQUES DE CALCUL DES FACTEURS
!   D'INTENSITE DE CONTRAINTE ET DE L'INTEGRALE J
!-----------------------------------------------------------------------
! ======================================================================
    real(kind=8) :: x, y, z
    real(kind=8) :: x1, x2
    real(kind=8) :: y1, y2
    real(kind=8) :: z1, z2
    real(kind=8) :: v111,v211,v121,v221
    real(kind=8) :: v112,v212,v122,v222
    real(kind=8) :: asc (6)
    real(kind=8) :: asr (9)
    real(kind=8) :: erev,emdb,valres(2)
    real(kind=8) :: ztempb, ztempc, valpar(1)
    integer :: ix1, ix2, iy1, iy2
    integer :: i,k, icodre(2)
    character(len=8)  :: nompar(2)
    character(len=16) :: nomres(2)
    character(len=32) :: phenom
!
!   asc <=> a/c 
!   asr <=> a/r 
!      a : profondeur du defaut
!      c : demi longueur du defaut
!      r : epaisseur du revetement
!
    data (asc(k),k=1,6) / 1.d0,    0.5d0,    0.25d0, &
                          0.125d0, 0.0625d0, 0.d0/
    data (asr(k),k=1,9) / 0.d0,    0.125d0,  0.25d0, &
                          0.5d0,   1.d0,     1.5d0,  &
                          2.d0,    3.d0,     4.d0 /
!
! initialisation
!
   do i=1,15
     coeinf(i)=0.d0
   end do
!
! calcul des variables x,y et z
!
   x  = prodef/londef*2.d0
   y  = prodef/lrev
!
!=========================================================================== 
! --- recherche des bornes x1 et x2 encadrant (x=a/c) ----------------------
!=========================================================================== 
   do i=1,6
      if(x.ge.asc(i)) then
         x1  = asc(i-1)
         x2  = asc(i)
         ix1 = i-1
         ix2 = i
         goto 10
      endif
   end do
10 continue
!=========================================================================== 
! --- recherche des bornes y1 et y2 encadrant (y=a/r) ----------------------
!=========================================================================== 
   do i=1,9
      if(y.le.asr(i)) then
         y1=asr(i-1)
         y2=asr(i)
         iy1 = i-1
         iy2 = i
         goto 20
      endif
   end do
20 continue
!===========================================================================
! --- Bornes en z : z1 et z2 
!===========================================================================
  z1 = 1.0d0
  z2 = 0.7d0
  phenom    = 'ELAS'
  nompar(1) = 'TEMP'
  nomres(1) = 'E'

! --- calcul au point B du rapport : 
!             z = module d'young revetement / module d'young metal de base  
!
  valpar = tempb
  call rcvale(matrev,phenom,1,nompar,valpar,&
              1,nomres,valres,icodre,1)
  erev=valres(1)
  call rcvale(matmdb,phenom,1,nompar,valpar,&
              1,nomres,valres,icodre,1)
  emdb=valres(1)
  ztempb = erev/emdb
!
! --- calcul au point c du rapport: 
!             z = module d'young revetement / module d'younf metal de base
!
  valpar = tempa
  call rcvale(matrev,phenom,1,nompar,valpar,&
              1,nomres,valres,icodre,1)
  erev=valres(1)
  call rcvale(matmdb,phenom,1,nompar,valpar,&
              1,nomres,valres,icodre,1)
  emdb=valres(1)
  ztempc=erev/emdb
!=========================================================================== 
! Point A (pour EDF) : Calcul des coefficients d'influence -----------------
! Interpolation sur valeurs tabulees : tableau dependant de deux variables -
!=========================================================================== 
   do i=1,5
      call coef_infl_a1(ix1,ix2,iy1,iy2,i, &
                        v111,v211,v121,v221)
!
      coeinf(i) = 1.d0/((x2 - x1) * (y2 - y1)) *&
                  ((x2 - x ) * (y2 - y) * v111 +&
                   (x2 - x ) * (y - y1) * v121 +&
                   (x  - x1) * (y2 - y) * v211 +&
                   (x  - x1) * (y - y1) * v221)
    end do 

!=========================================================================== 
! Point B et C  : Calcul des coefficients d'influence ----------------------
! Interpolation sur valeurs tabulees : tableau dependant de trois variables-
!=========================================================================== 
      do i=6,15
         if(i.le.10) then
           z = ztempb
           call coef_infl_b1(ix1,ix2,iy1,iy2,(i-5), &
                             v111,v211,v121,v221)
           call coef_infl_b2(ix1,ix2,iy1,iy2,(i-5), &
                             v112,v212,v122,v222)
         else  if(i.gt.10) then
           z = ztempc
           call coef_infl_c1(ix1,ix2,iy1,iy2,(i-10), &
                             v111,v211,v121,v221)
           call coef_infl_c2(ix1,ix2,iy1,iy2,(i-10), &
                             v112,v212,v122,v222)
         end if      
!
           coeinf(i) = 1.d0/((x2 - x1) * (y2 - y1) * (z2 - z1))*&
                       ((x2 - x ) * (y2 - y) * (z2 - z ) * v111  +&
                        (x2 - x ) * (y - y1) * (z2 - z ) * v121  +&
                        (x  - x1) * (y2 - y) * (z2 - z ) * v211  +&
                        (x  - x1) * (y - y1) * (z2 - z ) * v221  +&
                        (x2 - x ) * (y2 - y) * (z  - z1) * v112  +&
                        (x2 - x ) * (y - y1) * (z  - z1) * v122  +&
                        (x  - x1) * (y2 - y) * (z  - z1) * v212  +&
                        (x  - x1) * (y - y1) * (z  - z1) * v222)
   end do

end subroutine
