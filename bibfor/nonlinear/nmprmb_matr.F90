subroutine nmprmb_matr(nno, npg, kpg, poidsg, vff, dff,&
                igeom,ideplm,ideplp, i_pres, imatun)
!
    implicit none
!
#include "asterfort/assert.h"
#include "jeveux.h"
#include "asterfort/r8inir.h"
#include "asterfort/subaco.h"
#include "asterfort/subacv.h"
#include "asterfort/sumetr.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    integer, intent(in) :: nno
    integer, intent(in) :: npg, kpg
    integer, intent(in) :: igeom, ideplm,ideplp , imatun, i_pres
    real(kind=8), intent(in) :: poidsg
    real(kind=8), intent(in) :: vff(nno, npg)
    real(kind=8), intent(in) :: dff(2, nno)
!
! --------------------------------------------------------------------------------------------------
!
! Calcul de chargement
!
! Pression pour les membranes - matrice tangente
!
! --------------------------------------------------------------------------------------------------
!
!
! In  nno       : nombre de noeuds
! In  kpg       : numéro du points de Gauss
! In  poidsg    : poids du point de Gauss
! In  vff       : Valeurs des fonctions de F. aux pts de Gauss
! In  dff       : Valeurs des derivees des fonctions de F. aux pts de Gauss
! In  igeom     : adresse du tableau dans zr des coordonnees des noeuds
! In  ideplm    : adresse du tableau dans zr du tableau PDEPLMR
! In  ideplp    : adresse du tableau dans zr du tableau PDEPLPR
! In  i_pres    : adresse dans le tableau zr de la pression
! In  imatun    : adresse dans le tableau zr de la matrice tangente
!
! --------------------------------------------------------------------------------------------------
!
    integer :: n, i, j, k, a, b, p, q
    real(kind=8) :: geom(3, 9)
    real(kind=8) :: cova(3, 3), metr(2, 2), jac, cnva(3, 2)
    real(kind=8) :: acv(2, 2)
    real(kind=8) :: pres
    real(kind=8) :: matc(3*9, 3*9)
    real(kind=8) :: xpq1,xpq2
! --------------------------------------------------------------------------------------------------
!
    do n = 1, nno
        do i = 1, 3
            geom(i,n) = zr(igeom +i+3*(n-1)-1) + &
                        zr(ideplm+i+3*(n-1)-1) + &
                        zr(ideplp+i+3*(n-1)-1)
        end do
    end do


! Calcul de la pression au point de Gauss
    pres = 0.d0
    do n = 1, nno
        pres = pres + zr(i_pres+n-1) * vff(n,kpg)
    end do
    
!
! - Initializations
!
    call r8inir(9*9*9, 0.d0, matc, 1)

!
! ----- Covariant basis
!
    call subaco(nno, dff, geom, cova)
!
! ----- Metric tensor
!
    call sumetr(cova, metr, jac)
!
! ----- Contra-variant basis
!
    call subacv(cova, metr, jac, cnva, acv)
!
! ----- Tangent matrix
!
    do a = 1, nno
        do b = 1, nno
            do p = 1, 3
                do q = 1, 3
                    i = 3*(a-1) + p
                    j = 3*(b-1) + q
                    
                    xpq1 = 0.0
                    xpq2 = 0.0
                   
                    if (p.eq.1) then
                       if (q.eq.2) then
                           xpq1 = cova(3,1)
                           xpq2 = cova(3,2)
                       elseif (q.eq.3) then
                           xpq1 = -cova(2,1)
                           xpq2 = -cova(2,2)
                       endif
                    elseif (p.eq.2) then
                       if (q.eq.1) then
                           xpq1 = -cova(3,1)
                           xpq2 = -cova(3,2)
                       elseif (q.eq.3) then
                           xpq1 = cova(1,1)
                           xpq2 = cova(1,2)
                       endif
                    elseif (p.eq.3) then
                       if (q.eq.1) then
                           xpq1 = cova(2,1)
                           xpq2 = cova(2,2)
                       elseif (q.eq.2) then
                           xpq1 = -cova(1,1)
                           xpq2 = -cova(1,2)
                       endif
                    endif
                   
                   
                    matc(i,j) = matc(i,j) + poidsg*pres*vff(a,kpg)*( &
                                        dff(1,b)*xpq2-dff(2,b)*xpq1)
                enddo
            enddo
        enddo
    enddo

!
    k = 0
    do i = 1, 3*nno
        do j = 1, 3*nno
            k = k + 1
            zr(imatun-1+k) = zr(imatun-1+k) + matc(i,j)
        end do
    end do
    
end subroutine
