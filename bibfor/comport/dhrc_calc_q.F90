subroutine dhrc_calc_q(a, b, vint, eps, epse)
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
#include "asterfort/mgauss.h"
!
    real(kind=8) :: a(6, 6)
    real(kind=8) :: b(6, 2, 2)
    real(kind=8) :: vint(*)
    real(kind=8) :: eps(*)
    real(kind=8) :: epse(6)
! ----------------------------------------------------------------------
!
!      CALCUL DU TENSEUR Q QUI IDENTIFIE LES DEFORMATIONS RESIDUELLES
!
! IN:
!       A      : MATRICE SECANTE
!       B      : TENSEUR DE RAIDEUR COUPLE ELAS-PLAS
!
! OUT:
!       Q     : QUI IDENTIFIE LES DEFORMATIONS RESIDUELLES
!
! ----------------------------------------------------------------------
!
    integer :: ij, i, j, k, l, op, iret
    real(kind=8) :: inva(6, 6), det
    real(kind=8) :: q(6, 2, 2)
    real(kind=8) :: epsp(6)
!
    inva(:, :)=0.d0
!
    do ij = 1, 6
        inva(ij, ij)=1.0d0
    end do
!
    call mgauss('NFSP', a, inva, 6, 6, 6, det, iret)
!
    q(:, :, :)=0.d0
!
    do op = 1, 6
        do k = 1, 2
            do l = 1, 2
                do ij = 1, 6
                    q(op, k, l) = q(op, k, l) + inva(op, ij)*b(ij, k, l)
                end do
            end do
        end do
    end do
!
    do i = 1, 6
        do j = 1, 2
            do k =1, 2
                epsp(i)=epsp(i)+q(i,j,k)*vint((k-1)*2+j+2)
            end do
        end do
    end do
!
    do i = 1, 6
        epse(i)=eps(i)+epsp(i)
    end do
!
end subroutine
