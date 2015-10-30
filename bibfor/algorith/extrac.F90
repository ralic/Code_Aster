subroutine extrac(interp, prec, crit, nbinst, ti,&
                  temps, y, neq, xtract, ier, &
                  index)
    implicit none
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
    character(len=*), intent(in) :: interp
    real(kind=8), intent(in) :: prec
    character(len=*), intent(in) :: crit
    integer, intent(in) :: nbinst
    real(kind=8), intent(in) :: ti(*)
    real(kind=8), intent(in) :: temps
    integer, intent(in) :: neq
    real(kind=8), intent(in) :: y(nbinst*neq)
    real(kind=8), intent(out) :: xtract(neq)
    integer, intent(out) :: ier
    integer, optional, intent(out) :: index

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
!     EXTRACTION DANS UN TABLEAU CONTENANT DES VECTEURS A DES INSTANTS
!     SUCESSIFS DU VECTEUR EVENTUELLEMENT INTERPOLLE A L INSTANT SOUHAIT
!-----------------------------------------------------------------------
! IN  : INTERP  : TYPE D'INTERPOLATION
! IN  : PREC    : PRECISION DU TEST
! IN  : CRIT    : CRITERE 'ABSOLU' OU 'RELATIF'
! IN  : NBINST  : DIMENSION DE LA LISTE DES INSTANTS
! IN  : TI      : LISTE DES INSTANTS
! IN  : TEMPS   : TEMPS A INTERPOLER
! IN  : Y       : TABLEAU DE VECTEURS A DES INSTANTS DONNES, DE DIMENSION NEQ
! IN  : NEQ     : DIMENSION DES VECTEURS
! OUT : XTRACT  : VECTEUR INTERPOLE AU TEMPS TEMPS, DE DIMENSION NEQ
! OUT : IER     : CODE RETOUR, = 0 : IL Y A EU EXTRACTION
! OUT : INDEX   : OPTIONNEL : INDICE (1-NBINST) OU L'INSTANT (OU PREMIER INSTANT
!                 EN CAS D'INTERPOLATION) A ETE TROUVE
!-----------------------------------------------------------------------
    real(kind=8) :: prec2
!
!-----------------------------------------------------------------------
    integer :: i
    real(kind=8) :: alpha
!-----------------------------------------------------------------------
    ier = 0
!
!     --- RECUPERATION DU CHAMP ---
!
    if (present(index)) index = -1
    prec2 = prec
    if (crit(1:7) .eq. 'RELATIF') prec2 = prec * ti(1)
    if (abs( temps - ti(1) ) .le. prec2) then
        call dcopy(neq, y(1), 1, xtract, 1)
        if (present(index)) index = 1
        goto 9999
    endif
    if (crit(1:7) .eq. 'RELATIF') prec2 = prec * ti(nbinst)
    if (abs( temps - ti(nbinst) ) .le. prec2) then
        call dcopy(neq, y((nbinst-1)*neq+1), 1, xtract, 1)
        if (present(index)) index = nbinst
        goto 9999
    endif
!
    if (temps .lt. ti(1)) then
        ier = ier + 1
        goto 9999
    endif
    if (temps .gt. ti(nbinst)) then
        ier = ier + 1
        goto 9999
    endif
    if (interp(1:3) .eq. 'NON') then
!
!        --- PAS D'INTERPOLATION ---
        do i = 2, nbinst-1
            if (crit(1:7) .eq. 'RELATIF') prec2 = prec * ti(i)
            if (abs( temps - ti(i) ) .le. prec2) then
                if (present(index)) index = i
                call dcopy(neq, y((i-1)*neq+1), 1, xtract, 1)
                goto 9999
            endif
        end do
        ier = ier + 1
    else
!
!        --- INTERPOLATION LINEAIRE ---
        do i = 1, nbinst-1
            if (temps .ge. ti(i) .and. temps .lt. ti(i+1)) then
                if (present(index)) index = i
                alpha = ( temps - ti(i) ) / ( ti(i+1) - ti(i) )
                call dcopy(neq, y((i-1)*neq+1), 1, xtract, 1)
                call dscal(neq, (1.d0-alpha), xtract, 1)
                call daxpy(neq, alpha, y(i*neq+1), 1, xtract,&
                           1)
                goto 9999
            endif
        end do
        ier = ier + 1
    endif
!
9999  continue
end subroutine
