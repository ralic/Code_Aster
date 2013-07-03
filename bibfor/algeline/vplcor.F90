subroutine vplcor(ldynam, neq, nbvect, nborto, prorto,&
                  signes, vect, ivecp, pkx, plx)
    implicit none
#include "jeveux.h"
#include "asterfort/mrmult.h"
#include "asterfort/u2mesi.h"
    integer :: ldynam, neq, nborto, nbvect, ivecp
    real(kind=8) :: prorto
    real(kind=8) :: signes(nbvect), vect(neq, nbvect), pkx(neq, nbvect)
    real(kind=8) :: plx(neq)
!     ------------------------------------------------------------------
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
!     ORTHOGONALISATION D'UN VECTEUR PAR RAPPORT A UNE FAMILLE DE
!     VECTEURS DEJA ORTHOGONAUX
!     LES VECTEURS OBTENUS SONT K-ORTHOGONAUX .
!     ------------------------------------------------------------------
! IN  LDYNAM : IS : DESCRIPTEUR MATRICE DE "RAIDEUR"
! IN  NEQ    : IS : DIMENSION DES VECTEURS
! IN  NBVECT : IS : NOMBRE TOTAL DE VECTEURS (SERT A DIMENSIONER)
! IN  PRORTO : R8 : PRECISON DEMANDEE POUR L'ORTHOGONALISATION
! IN  NBORTO : IS : NOMBRE MAXIMUM D'ORTHOGONALISATION PERMISE.
! IN  PLX    : R8 : VECTEUR DE TRAVAIL
!     SIGNES : R8 : (+/- 1)  SIGNE DU PSEUDO PRODUIT SCALAIRE ENTRE LE
!                   I EME VECTEUR ET LE I-1 EME VECTEUR
!     VECT   : R8 : VECT(1..NEQ,1..NBVECT) PRODUIT DE LA MATRICE DE
!                   RAIDEUR PAR LES VECTEURS DE LA BASE
! IN  IVECP  : IS : NUMERO DU VECTEUR A ORTHOGONALISER AVEC LES IVECP-1
!                   AUTRES PREMIERS VECTEURS
!     ------------------------------------------------------------------
!
!
!     -----------------------------------------------------------------
    integer :: ieq, ito
    real(kind=8) :: coef, xikxi, xjkxi, xjkxis
!     -----------------------------------------------------------------
!
!         --- K-REORTHOGONALISATION COMPLETE DU VECTEUR IVECP
!
!-----------------------------------------------------------------------
    integer :: ior, iortho, jvec
!-----------------------------------------------------------------------
    ior = 0
!
    do 10 jvec = 1, ivecp-1
!
        xjkxi = 0.d0
        do 20 ieq = 1, neq
            xjkxi = xjkxi + pkx(ieq,jvec) * vect(ieq,ivecp)
20      continue
!
        iortho = 0
        if (abs(xjkxi) .gt. prorto) then
            ior = 1
            do 30 ito = 1, nborto
                iortho = ito
!
                do 35 ieq = 1, neq
                    plx(ieq) = vect(ieq,ivecp) - xjkxi*signes(jvec)* vect(ieq,jvec)
35              continue
!
                xjkxis = 0.d0
                do 40 ieq = 1, neq
                    xjkxis = xjkxis + pkx(ieq,jvec) * plx(ieq)
40              continue
!
                if (abs(xjkxis) .lt. prorto) then
                    do 50 ieq = 1, neq
                        vect(ieq,ivecp) = plx(ieq)
50                  continue
                    xjkxi = xjkxis
                    goto 100
                else if (abs(xjkxis).lt.abs(xjkxi)) then
                    do 60 ieq = 1, neq
                        vect(ieq,ivecp) = plx(ieq)
60                  continue
                    xjkxi = xjkxis
                else
                    call u2mesi('A', 'ALGELINE4_76', 1, iortho)
                    goto 100
                endif
!
30          continue
            iortho = - nborto
!
100          continue
            call mrmult('ZERO', ldynam, vect(1, ivecp), pkx(1, ivecp), 1,&
                        .false.)
        endif
!
10  continue
!
!         --- SI LE VECTEUR IVECP A ETE MODIFIE (IOR=1) ALORS ---
!         ---               ON LE RENORMALISE                 ---
!
    if (ior .eq. 1) then
        xikxi = 0.d0
        do 200 ieq = 1, neq
            xikxi = xikxi + vect(ieq,ivecp) * pkx(ieq,ivecp)
200      continue
        signes(ivecp) = sign(1.d0,xikxi)
        coef = 1.d0 / sqrt(abs(xikxi))
        do 210 ieq = 1, neq
            vect(ieq,ivecp) = coef * vect(ieq,ivecp)
            pkx(ieq,ivecp) = coef * pkx(ieq,ivecp)
210      continue
    endif
!
end subroutine
