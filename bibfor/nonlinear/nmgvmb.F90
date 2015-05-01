subroutine nmgvmb(ndim, nno1, nno2, npg, axi,&
                  geom, vff1, vff2, idfde1, idfde2,&
                  iw, nddl, neps, b, w,&
                  ni2ldc)
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
!
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/dfdmip.h"
#include "asterfort/r8inir.h"
    aster_logical :: axi
    integer :: ndim, nno1, nno2, npg, idfde1, idfde2, iw
    real(kind=8) :: geom(ndim, nno1), vff1(nno1, npg), vff2(nno2, npg)
    integer :: nddl, neps
    real(kind=8) :: b(3*ndim+2, npg, *)
    real(kind=8) :: w(npg), ni2ldc(3*ndim+2)
! ----------------------------------------------------------------------
!  CALCUL DES ELEMENTS CINEMATIQUES POUR LA MODELISATION GRAD_VARI
! ----------------------------------------------------------------------
! IN  NDIM   DIMENSION DE L'ESPACE
! IN  NNO1   NOMBRE DE NOEUDS TOTAL (SUPPORT DES DEPLACEMENTS)
! IN  NNO2   NOMBRE DE NOEUDS SOMMETS (SUPPORT DE VI ET LAGRANGE)
! IN  NPG    NOMBRE DE POINTS DE GAUSS
! IN  AXI    .TRUE. SI MODELISATION AXIS
! IN  GEOM   COORDONNEES DES NOEUDS
! IN  VFF1   VALEUR DE LA FAMILLE DE FONCTIONS DE FORME NO 1
! IN  VFF2   VALEUR DE LA FAMILLE DE FONCTIONS DE FORME NO 2
! IN  IDFDE1 POINTEUR SUR LES DER. REFERENCE FAMILLE FCT FORME NO 1
! IN  IDFDE2 POINTEUR SUR LES DER. REFERENCE FAMILLE FCT FORME NO 2
! IN  IW     POINTEUR SUR LES POIDS DES PTS DE GAUSS DE REFERENCE
! OUT NDDL   NOMBRE DE DDL / ELEMENT
! OUT NEPS   NBR DE COMPOSANTE DE DEFORMATION (GENERALISEE)
! OUT B      MATRICE CINEMATIQUE EPS = B.U
! OUT W      POIDS DES POINTS DE GAUSS REELS
! OUT NI2LDC CONVERSION CONTRAINTE STOCKEE -> CONTRAINTE LDC (AVEC RAC2)
! ----------------------------------------------------------------------
    integer :: g
    real(kind=8) :: rac2, r2, r, dfdi1(27*3), dfdi2(8*3), unsurr
! ----------------------------------------------------------------------
    integer :: n, i
#define iu1(n,i) (n-1)*(ndim+2) + i
#define iu2(n,i) nno2*2 + (n-1)*ndim + i
#define ia(n) (n-1)*(ndim+2) + ndim + 1
#define il(n) (n-1)*(ndim+2) + ndim + 2
#define dff1(n,i) dfdi1(nno1*(i-1) + n)
#define dff2(n,i) dfdi2(nno2*(i-1) + n)
! ----------------------------------------------------------------------
    ASSERT(nno1.le.27)
    ASSERT(nno2.le.8)
    rac2 = sqrt(2.d0)
    r2 = sqrt(2.d0)/2
    nddl = nno1*ndim + nno2*2
    neps = 3*ndim + 2
    call r8inir(neps*npg*nddl, 0.d0, b, 1)
!
! - AFFECTATION DE LA FONCTION DE TRANSFERT SIGMA NICE --> SIGMA LDC
!
    call r8inir(neps, 1.d0, ni2ldc, 1)
    do 5 i = 4, 2*ndim
        ni2ldc(i) = rac2
  5 end do
!
!
! - AFFECTATION DE LA MATRICE CINEMATIQUE B
!
    do 1000 g = 1, npg
        call dfdmip(ndim, nno2, axi, geom, g,&
                    iw, vff2(1, g), idfde2, r, w(g),&
                    dfdi2)
        call dfdmip(ndim, nno1, axi, geom, g,&
                    iw, vff1(1, g), idfde1, r, w(g),&
                    dfdi1)
        if (ndim .eq. 2) then
            if (axi) then
                unsurr = 1/r
            else
                unsurr = 0
            endif
!
            do 10 n = 1, nno2
                b(1,g,iu1(n,1)) = dff1(n,1)
                b(2,g,iu1(n,2)) = dff1(n,2)
                b(3,g,iu1(n,1)) = vff1(n,g)*unsurr
                b(4,g,iu1(n,1)) = r2*dff1(n,2)
                b(4,g,iu1(n,2)) = r2*dff1(n,1)
                b(5,g,ia(n)) = vff2(n,g)
                b(6,g,il(n)) = vff2(n,g)
                b(7,g,ia(n)) = dff2(n,1)
                b(8,g,ia(n)) = dff2(n,2)
 10         continue
!
            do 20 n = nno2+1, nno1
                b(1,g,iu2(n,1)) = dff1(n,1)
                b(2,g,iu2(n,2)) = dff1(n,2)
                b(3,g,iu2(n,1)) = vff1(n,g)*unsurr
                b(4,g,iu2(n,1)) = r2*dff1(n,2)
                b(4,g,iu2(n,2)) = r2*dff1(n,1)
 20         continue
!
        else if (ndim.eq.3) then
            do 30 n = 1, nno2
                b(1,g,iu1(n,1)) = dff1(n,1)
                b(2,g,iu1(n,2)) = dff1(n,2)
                b(3,g,iu1(n,3)) = dff1(n,3)
                b(4,g,iu1(n,1)) = r2*dff1(n,2)
                b(4,g,iu1(n,2)) = r2*dff1(n,1)
                b(5,g,iu1(n,1)) = r2*dff1(n,3)
                b(5,g,iu1(n,3)) = r2*dff1(n,1)
                b(6,g,iu1(n,2)) = r2*dff1(n,3)
                b(6,g,iu1(n,3)) = r2*dff1(n,2)
                b(7,g,ia(n)) = vff2(n,g)
                b(8,g,il(n)) = vff2(n,g)
                b(9,g,ia(n)) = dff2(n,1)
                b(10,g,ia(n)) = dff2(n,2)
                b(11,g,ia(n)) = dff2(n,3)
 30         continue
!
            do 40 n = nno2+1, nno1
                b(1,g,iu2(n,1)) = dff1(n,1)
                b(2,g,iu2(n,2)) = dff1(n,2)
                b(3,g,iu2(n,3)) = dff1(n,3)
                b(4,g,iu2(n,1)) = r2*dff1(n,2)
                b(4,g,iu2(n,2)) = r2*dff1(n,1)
                b(5,g,iu2(n,1)) = r2*dff1(n,3)
                b(5,g,iu2(n,3)) = r2*dff1(n,1)
                b(6,g,iu2(n,2)) = r2*dff1(n,3)
                b(6,g,iu2(n,3)) = r2*dff1(n,2)
 40         continue
        endif
1000 end do
end subroutine
