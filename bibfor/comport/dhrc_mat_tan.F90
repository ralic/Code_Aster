subroutine dhrc_mat_tan(a, ap1, ap2, b,&
                  bp1, bp2, bocaj, neta1, neta2,&
                  indip, cstseu, eps, vint, dsidep)
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
! person_in_charge: sebastien.fayolle at edf.fr
!
    implicit none
#include "asterfort/matini.h"
    integer :: indip(6)
    real(kind=8) :: eps(8), vint(*)
    real(kind=8) :: a(6, 6), ap1(6, 6), ap2(6, 6)
    real(kind=8) :: b(6, 2, 2), bp1(6, 2), bp2(6, 2)
    real(kind=8) :: neta1(2), neta2(2), cstseu(2)
    real(kind=8) :: bocaj(6, 6)
    real(kind=8) :: dsidep(6, 6)
!
! ----------------------------------------------------------------------
!
!      CALCUL DES FORCES THERMODYNAMIQUES ASSOCIEES A LA PLASTICITE
!      APPELE PAR "SEUGLC"
!
! IN:
!       A       : TENSEUR DE RAIDEUR ELASTIQUE ENDOMMAGEE
!
! OUT:
!       DSIDEP  : MATRICE DE RAIDEUR ELASTIQUE
!
! ----------------------------------------------------------------------
!
    integer :: i, j, jb, k, l, lb
    real(kind=8) :: dsida(6, 6), dsedep(6, 6)
!
    do k = 1, 6
        do i = 1, 6
            dsidep(k,i)=a(k,i)
        end do
    end do
!
! ----------------------------------------------------------------------
!     CALCUL DE DSIDA
! ----------------------------------------------------------------------
!     INITIALISATION
    dsida(:,:) = 0.d0
!
    do k = 1, 6
        do i = 1, 6
            dsida(k,1) = dsida(k,1)+ap1(k,i)*eps(i)
            dsida(k,2) = dsida(k,2)+ap2(k,i)*eps(i)
            if (i .lt. 3) then
                dsida(k,1) = dsida(k,1)+bp1(k,i)*vint(i+2)*0.5d0
                dsida(k,2) = dsida(k,2)+bp2(k,i)*vint(i+4)*0.5d0
            endif
        end do
        dsida(k,3) = b(k,1,1)*0.5d0
        dsida(k,4) = b(k,2,1)*0.5d0
        dsida(k,5) = b(k,1,2)*0.5d0
        dsida(k,6) = b(k,2,2)*0.5d0
    end do
!
!----------------------------------------------------------------------
!     CALCUL DE DSEDEP
!----------------------------------------------------------------------
!     INITIALISATION
!
    do k = 1, 6
        dsedep(1,k) = dsida(k,1)/cstseu(1)
        dsedep(2,k) = dsida(k,2)/cstseu(1)
        dsedep(3,k) = dsida(k,3)*2.d0*neta1(1)/(cstseu(2)**2.d0)
        dsedep(4,k) = dsida(k,4)*2.d0*neta1(2)/(cstseu(2)**2.d0)
        dsedep(5,k) = dsida(k,5)*2.d0*neta2(1)/(cstseu(2)**2.d0)
        dsedep(6,k) = dsida(k,6)*2.d0*neta2(2)/(cstseu(2)**2.d0)
    end do
!
!----------------------------------------------------------------------
!    CALCUL DE DSIDEP
!----------------------------------------------------------------------
!
    do k = 1, 6
        do i = 1, 6
            jb=0
            do j = 1, 6
                lb=0
                if (indip(j) .eq. j) then
                    jb=jb+1
                    do l = 1, 6
                        if (indip(l) .eq. l) then
                            lb=lb+1
                            dsidep(k,i)=dsidep(k,i)+ dsida(k,j)*bocaj(jb,lb)*dsedep(l,i)
                        endif
                    end do
                endif
            end do
        end do
    end do
!
end subroutine
