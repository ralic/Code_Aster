subroutine mppsta(h, ldh, v, ldv, ddlsta,&
                  n, vectt, ddlexc, indico, proj)
!-----------------------------------------------------------------------
!
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
!
!-----------------------------------------------------------------------
!
! METHODE DES PUISSANCES AVEC PROJECTION. OPTIMISATION
! SOUS CONTRAINTES INEGALITES
!
! ----------------------------------------------------------------------
!
! IN  H        : MATRICE REDUITE
! IN  LDH      : NOMBRE DE COEFFICIENTS DE H
! IN  V        : MATRICE DE CHANGEMENT DE BASE
! IN  LDV      : NOMBRE DE COEFFICIENTS DE V
! IN  DDLSTA   : POSITION DES DDL_STAB
! IN  N        : DIMENSION ESPACE GLOBAL
! IN/OUT VECTT : MODE CONVERGE
! IN  DDLEXC   : POSITION DDL IMPOSES
! IN  INDICO   : SI =1 ALORS VECTEUR INITIAL =VECTT
!                SINON RANDOM
! IN  PROJ     : =1 PROJECTION
!                =0 PAS DE PROJECTION DANS LA METHODE
!
! ----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
#include "jeveux.h"
#include "asterfort/jedetr.h"
#include "asterfort/r8inir.h"
#include "asterfort/wkvect.h"
#include "blas/dgemv.h"
#include "blas/dlarnv.h"
#include "blas/dnrm2.h"
#include "blas/dscal.h"
    integer :: n, ldh, ldv, indico, proj
    integer :: ddlsta(n), ddlexc(n)
    real(kind=8) :: h(ldh, ldh), v(ldv, ldh)
    real(kind=8) :: vectt(ldv)
    integer :: i, j, nitmax, num, npro
    integer(kind=4) :: iseed(4)
    integer :: x0, bounds, x
    real(kind=8) :: norm
    real(kind=8) :: temp, epsil, one, zero
    real(kind=8) :: crit2, epsf
    parameter (epsil=1.d-15)
    parameter (crit2=1.d-12)
    parameter (nitmax = 40000)
    parameter (one = 1.0d+0, zero = 0.0d+0)
!
    call wkvect('&&MPPSTA.VECT.TEM1', 'V V R', ldh, x0)
    call wkvect('&&MPPSTA.VECT.TEM2', 'V V R', ldh, bounds)
    call wkvect('&&MPPSTA.VECT.TEM3', 'V V R', ldh, x)
!
    call r8inir(ldh, 0.d0, zr(x0), 1)
    call r8inir(ldh, 0.d0, zr(bounds), 1)
    call r8inir(ldh, 0.d0, zr(x), 1)
!
!     INITIALISATION PAR UN VECT RANDOMDE TAILLE LDH
!
    if (indico .eq. 1) then
        call dgemv('T', ldv, ldh, one, v,&
                   ldv, vectt, 1, zero, zr(x0),&
                   1)
        temp = dnrm2(ldh,zr(x0),1)
        call dscal(ldh, one / temp, zr(x0), 1)
        epsf = crit2
        goto 100
    else
        epsf = epsil
    endif
!
    iseed(1) = 1
    iseed(2) = 3
    iseed(3) = 5
    iseed(4) = 7
!
    call dlarnv(2, iseed, ldh, zr(x0))
!
!     PROJECTION DANS LA BASE INITIALE
!
    call dgemv('N', ldv, ldh, one, v,&
               ldv, zr(x0), 1, zero, vectt,&
               1)
!
!     PROJECTION DANS SUR LES DDLS_STAB POSITIFS ET CL
!
    do 10 i = 1, n
        if (ddlsta(i) .eq. 0 .and. proj .eq. 1) then
            if (vectt(i) .lt. zero) then
                vectt(i) = zero
            endif
        else
            vectt(i) = vectt(i)*ddlexc(i)
        endif
10  end do
!
!     RETOUR DANS LA BASE D'ARNOLDI
!
    call dgemv('T', ldv, ldh, one, v,&
               ldv, vectt, 1, zero, zr(x0),&
               1)
!
!     ON NORME X0
!
    temp = dnrm2(ldh,zr(x0),1)
    call dscal(ldh, one / temp, zr(x0), 1)
!
!     ON APPLIQUE LA METHODE DES PUISSANCES
!
100  continue
!
    norm = 1.d0
    do 15 j = 1, nitmax
        npro = 0
        num = 0
        call dgemv('N', ldh, ldh, one, h,&
                   ldh, zr(x0), 1, zero, zr(x),&
                   1)
        call dgemv('N', ldv, ldh, one, v,&
                   ldv, zr(x), 1, zero, vectt,&
                   1)
        do 20 i = 1, n
            if (ddlsta(i) .eq. 0 .and. proj .eq. 1) then
                num = num+1
                if (vectt(i) .le. zero) then
                    vectt(i) = zero
                    npro = npro+1
                endif
            else
                vectt(i) = vectt(i)*ddlexc(i)
            endif
20      continue
        call dgemv('T', ldv, ldh, one, v,&
                   ldv, vectt, 1, zero, zr(x),&
                   1)
        temp = dnrm2(ldh,zr(x),1)
        call dscal(ldh, one / temp, zr(x), 1)
        do 25 i = 1, ldh
            zr(bounds+i-1) = zr(x0+i-1)-zr(x+i-1)
25      continue
        norm = dnrm2(ldh,zr(bounds),1)
!
        do 50 i = 1, ldh
            zr(x0+i-1) = zr(x+i-1)
50      continue
!
        if (norm .le. epsf) then
            goto 900
        else if (j.eq.nitmax) then
            goto 900
        endif
15  end do
!
900  continue
!
!
!     MENAGE
!
    call jedetr('&&MPPSTA.VECT.TEM1')
    call jedetr('&&MPPSTA.VECT.TEM2')
    call jedetr('&&MPPSTA.VECT.TEM3')
!
!     %---------------%
!     | END OF ENDSTA |
!     %---------------%
!
end subroutine
