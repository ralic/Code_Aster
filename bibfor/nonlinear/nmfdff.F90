subroutine nmfdff(ndim, nno, axi, g, r,&
                  rigi, matsym, fr, vff, dff,&
                  def, pff)
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
! ----------------------------------------------------------------------
!     BUT:  CALCUL DES PRDUITS DU GRADIENT DE TRANSFORMATION FR
!     PAR LES DERIVEES DE FONCITONS DE FORME DFF POUR NMDLOG ET NMGR3D
!     CONFIGURATION LAGRANGIENNE.
! ----------------------------------------------------------------------
! IN  NDIM    : DIMENSION DU PROBLEME : 2 OU 3
! IN  DFF     : DERIVEE DES FONCTIONS DE FORME
! IN  FR      : GRADIENT TRANSFORMATION
! OUT DEF     : PRODUITS F*DFF
! OUT PFF     : PRODUITS DFF*DFF
!
    implicit none
#include "asterf_types.h"
    integer :: ndim, nno, n, nmax, i, m, g
    real(kind=8) :: dff(nno, *), vff(nno, *), fr(3, 3), rac2, r
    real(kind=8) :: def(2*ndim, nno, ndim), pff(2*ndim, nno, nno)
    aster_logical :: axi, rigi, matsym
!
    rac2=sqrt(2.d0)
!
    if (ndim .eq. 3) then
!
        do 40 n = 1, nno
            do 30 i = 1, 3
                def(1,n,i) = fr(i,1)*dff(n,1)
                def(2,n,i) = fr(i,2)*dff(n,2)
                def(3,n,i) = fr(i,3)*dff(n,3)
                def(4,n,i) = (fr(i,1)*dff(n,2) + fr(i,2)*dff(n,1))/ rac2
                def(5,n,i) = (fr(i,1)*dff(n,3) + fr(i,3)*dff(n,1))/ rac2
                def(6,n,i) = (fr(i,2)*dff(n,3) + fr(i,3)*dff(n,2))/ rac2
 30         continue
 40     continue
!
        if (rigi) then
            do 125 n = 1, nno
                if (matsym) then
                    nmax = n
                else
                    nmax = nno
                endif
                do 126 m = 1, nmax
                    pff(1,n,m) = dff(n,1)*dff(m,1)
                    pff(2,n,m) = dff(n,2)*dff(m,2)
                    pff(3,n,m) = dff(n,3)*dff(m,3)
                    pff(4,n,m) =(dff(n,1)*dff(m,2)+dff(n,2)*dff(m,1))/&
                    rac2
                    pff(5,n,m) =(dff(n,1)*dff(m,3)+dff(n,3)*dff(m,1))/&
                    rac2
                    pff(6,n,m) =(dff(n,2)*dff(m,3)+dff(n,3)*dff(m,2))/&
                    rac2
126             continue
125         continue
        endif
!
    else if (ndim.eq.2) then
!
        do 41 n = 1, nno
            do 31 i = 1, 2
                def(1,n,i) = fr(i,1)*dff(n,1)
                def(2,n,i) = fr(i,2)*dff(n,2)
                def(3,n,i) = 0.d0
                def(4,n,i) = (fr(i,1)*dff(n,2) + fr(i,2)*dff(n,1))/ rac2
 31         continue
 41     continue
! 5.2.5 - TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
        if (axi) then
            do 50 n = 1, nno
                def(3,n,1) = fr(3,3)*vff(n,g)/r
 50         continue
        endif
        if (rigi) then
            do 135 n = 1, nno
                if (matsym) then
                    nmax = n
                else
                    nmax = nno
                endif
                do 136 m = 1, nmax
                    pff(1,n,m) = dff(n,1)*dff(m,1)
                    pff(2,n,m) = dff(n,2)*dff(m,2)
                    pff(3,n,m) = 0.d0
                    pff(4,n,m) =(dff(n,1)*dff(m,2)+dff(n,2)*dff(m,1))/&
                    rac2
136             continue
135         continue
        endif
!
    endif
end subroutine
