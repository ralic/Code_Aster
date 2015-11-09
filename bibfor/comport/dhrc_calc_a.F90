subroutine dhrc_calc_a(a0, aa_t, ga_t, aa_c, ga_c, eps, vint, a, ap1, ap2, as1, as2)
!
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
! person_in_charge: sebastien.fayolle at edf.fr
!
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dhrc_calc_a_term.h"
#include "asterfort/diago2.h"
#include "asterfort/matini.h"
#include "asterc/r8prem.h"
#include "blas/dcopy.h"
!
    real(kind=8), intent(in) :: vint(*), eps(*)
    real(kind=8), intent(in) :: a0(6, 6)
    real(kind=8), intent(in) :: aa_t(6, 6, 2), ga_t(6, 6, 2), aa_c(6, 6, 2), ga_c(6, 6, 2)
    real(kind=8), intent(out) :: a(6, 6), ap1(6, 6), ap2(6, 6), as1(6, 6), as2(6, 6)
! ----------------------------------------------------------------------
!
!      CALCUL DU TENSEUR DE RAIDEUR A ET DE SES DERIVEES PAR RAPPORT A D
!
! IN:
!       EPS   : TENSEUR DE DEFORMATIONS
!               (EXX EYY 2EXY KXX KYY 2KXY)
!       VINT   : VECTEUR DES VARIABLES INTERNES
!                VINT=(D1,D2,EPSP1X,EPSP1Y,EPSP2X,EPSP2Y)
!       A0     : RAIDEUR ELASTIQUE (D=0)
!    POUR LA TRACTION :
!       AA_T   : PARAMETRE ALPHA DE LA FONCTION D'ENDOMMAGEMENT
!       GA_T   : PARAMETRE GAMMA DE LA FONCTION D'ENDOMMAGEMENT
!    POUR LA COMPRESSION :
!       AA_C   : PARAMETRE ALPHA DE LA FONCTION D'ENDOMMAGEMENT
!       GA_C   : PARAMETRE GAMMA DE LA FONCTION D'ENDOMMAGEMENT
!
! OUT:
!       A     : TENSEUR DE RAIDEUR ELASTIQUE
!       AP1   : DERIVEE PREMIERE DU TENSEUR DE RAIDEUR ELASTIQUE PAR
!               RAPPORT A D1
!       AP2   : DERIVEE PREMIERE DU TENSEUR DE RAIDEUR ELASTIQUE PAR
!               RAPPORT A D2
!       AS1   : DERIVEE SECONDE DU TENSEUR DE RAIDEUR ELASTIQUE PAR
!               RAPPORT A D1
!       AS2   : DERIVEE SECONDE DU TENSEUR DE RAIDEUR ELASTIQUE PAR
!               RAPPORT A D2
!
! ----------------------------------------------------------------------
!
    integer :: i, j
    real(kind=8) :: treps, trkap, deteps, detkap
    real(kind=8) :: epsl(8), emp(2), efp(2), vmp(2, 2), vfp(2, 2)
    real(kind=8) :: rvp

! -- POUR LES TERMES DE MF POSITION D EVALUATION DE LA DEFORMATION POUR DISTINGUER TRAC-COMP
!
    call matini(6, 6, 0.d0, a)
    call matini(6, 6, 0.d0, ap1)
    call matini(6, 6, 0.d0, ap2)
    call matini(6, 6, 0.d0, as1)
    call matini(6, 6, 0.d0, as2)
!
    call dcopy(8, eps, 1, epsl, 1)
!
    epsl(3) = epsl(3)*0.5d0
    epsl(6) = epsl(6)*0.5d0
!
    treps=epsl(1)+epsl(2)
    trkap=epsl(4)+epsl(5)
    deteps=epsl(1)*epsl(2)-epsl(3)**2.0d0
    detkap=epsl(4)*epsl(5)-epsl(6)**2.0d0
!
! -- DIAGONALISATION
!
    call diago2(epsl(1), vmp, emp)
    call diago2(epsl(4), vfp, efp)
!
! -- TERMES DE MEMBRANE PURE
    if (deteps .ge. -r8prem()) then
        if (treps .ge. -r8prem()) then
!
! -- ZONE 1
!
            do i = 1, 2
                do j = i, 2
                    call dhrc_calc_a_term(i, j, 1, 1, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                          ap1(i,j), ap2(i,j), as1(i,j), as2(i,j))
                end do
            end do
        else
!
! -- ZONE 2
!
            do i = 1, 2
                do j = i, 2
                    call dhrc_calc_a_term(i, j, 2, 2, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                          ap1(i,j), ap2(i,j), as1(i,j), as2(i,j))
                end do
            end do
        end if
    else
        if (treps .ge. 0.d0) then
            if (emp(1) .gt. 0.d0) then
!
! -- ZONE 3
!
                rvp = abs(emp(2))/abs(emp(1))
!
                i = 1
                do j = 1, 2
                    call dhrc_calc_a_term(i, j, 1, 1, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                          ap1(i,j), ap2(i,j), as1(i,j), as2(i,j))
                end do
!
                i = 2
                j = 2
                call dhrc_calc_a_term(i, j, 1, 1, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j), rvp, 2, 2)
!
            elseif(emp(2) .gt. 0.d0) then
!
! -- ZONE 4
!
                rvp = abs(emp(1))/abs(emp(2))
!
                j = 2
                do i = 1, 2
                    call dhrc_calc_a_term(i, j, 1, 1, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                          ap1(i,j), ap2(i,j), as1(i,j), as2(i,j))
                end do
!
                i = 1
                j = 1
                call dhrc_calc_a_term(i, j, 1, 1, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j), rvp, 2, 2)
!
            else
                write(6,*) 'deteps :', deteps
                write(6,*) 'treps  :', treps
                write(6,*) 'emp    :', emp
                ASSERT(.FALSE.)
            end if
        else
            if (emp(1) .ge. 0.d0) then
!
! -- ZONE 5
!
                rvp = abs(emp(1))/abs(emp(2))
!
                i = 2
                j = 2
                call dhrc_calc_a_term(i, j, 2, 2, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j))
!
                i = 1
                do j = 1, 2
                    call dhrc_calc_a_term(i, j, 2, 2, a0, aa_t, ga_t, aa_c, ga_c, vint,a(i,j), &
                                          ap1(i,j), ap2(i,j), as1(i,j), as2(i,j), rvp, 1, 1)
                end do
!
            elseif(emp(2) .ge. 0.d0) then
!
! -- ZONE 6
!
                rvp = abs(emp(2))/abs(emp(1))
!
                i = 1
                j = 1
                call dhrc_calc_a_term(i, j, 2, 2, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j))
!
                j = 2
                do i = 1, 2
                    call dhrc_calc_a_term(i, j, 2, 2, a0, aa_t, ga_t, aa_c, ga_c, vint,a(i,j), &
                                          ap1(i,j), ap2(i,j), as1(i,j), as2(i,j), rvp, 1, 1)
                end do
            else
                write(6,*) 'deteps :', deteps
                write(6,*) 'treps  :', treps
                write(6,*) 'emp    :', emp
                ASSERT(.FALSE.)
            end if
        end if
    end if
!
! -- Terme Amm_xyxy identique en traction et compression
!
    call dhrc_calc_a_term(3, 3, 1, 1, a0, aa_t, ga_t, aa_c, ga_c, vint, a(3,3), &
                          ap1(3,3), ap2(3,3), as1(3,3), as2(3,3))
!
! -- TERMES DE FLEXION PURE
! -- DANS LES DKTG LA COURBURE EST EGALE A - LA DERIVEE DE LA FLECHE
! -- C EST POURQUOI ON TESTE L OPPOSE DE LA COURBURE
    if (detkap .ge. -r8prem()) then
        if (trkap .ge. -r8prem()) then
!
! -- ZONE 1
!
            do i = 4, 5
                do j = i, 5
                    call dhrc_calc_a_term(i, j, 1, 2, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                          ap1(i,j), ap2(i,j), as1(i,j), as2(i,j))
                end do
            end do
!
        else
!
! -- ZONE 2
!
            do i = 4, 5
                do j = i, 5
                    call dhrc_calc_a_term(i, j, 2, 1, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                          ap1(i,j), ap2(i,j), as1(i,j), as2(i,j))
                end do
            end do
        end if
    else
        if (trkap .ge. 0.d0) then
            if (efp(1) .gt. 0.d0) then
!
! -- ZONE 3
!
                rvp = abs(efp(2))/abs(efp(1))
!
                i = 4
                j = 4
                call dhrc_calc_a_term(i, j, 1, 2, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j))
!
                i = 4
                j = 5
                call dhrc_calc_a_term(i, j, 1, 2, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j), rvp, 2, 2)
!
                i = 5
                j = 5
                call dhrc_calc_a_term(i, j, 1, 2, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j), rvp, 2, 1)
!
            elseif(efp(2) .gt. 0.d0) then
!
! -- ZONE 4
!
                rvp = abs(efp(1))/abs(efp(2))
!
                i = 4
                j = 4
                call dhrc_calc_a_term(i, j, 1, 2, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j), rvp, 2, 1)
!
                i = 4
                j = 5
                call dhrc_calc_a_term(i, j, 1, 2, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j), rvp, 2, 2)
!
                i = 5
                j = 5
                call dhrc_calc_a_term(i, j, 1, 2, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j))
!
            else
                write(6,*) 'detkap :', detkap
                write(6,*) 'trkap  :', trkap
                write(6,*) 'efp    :', efp
                ASSERT(.FALSE.)
            end if
        else
            if (efp(1) .ge. 0.d0) then
!
! -- ZONE 5
!
                rvp = abs(efp(1))/abs(efp(2))
!
                i = 4
                j = 4
                call dhrc_calc_a_term(i, j, 2, 1, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j), rvp, 1, 2)
!
                i = 4
                j = 5
                call dhrc_calc_a_term(i, j, 2, 1, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j), rvp, 2, 2)
!
                i = 5
                j = 5
                call dhrc_calc_a_term(i, j, 2, 1, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j))
!
            elseif(efp(2) .ge. 0.d0) then
!
! -- ZONE 6
!
                rvp = abs(efp(2))/abs(efp(1))
!
                i = 4
                j = 4
                call dhrc_calc_a_term(i, j, 2, 1, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j))
!
                i = 4
                j = 5
                call dhrc_calc_a_term(i, j, 2, 1, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j), rvp, 2, 2)
!
                i = 5
                j = 5
                call dhrc_calc_a_term(i, j, 2, 1, a0, aa_t, ga_t, aa_c, ga_c, vint, a(i,j), &
                                      ap1(i,j), ap2(i,j), as1(i,j), as2(i,j), rvp, 1, 2)
!
            else
                write(6,*) 'detkap :', detkap
                write(6,*) 'trkap  :', trkap
                write(6,*) 'efp    :', efp
                ASSERT(.FALSE.)
            end if
        end if
    end if
!
! -- Terme Aff_xyxy identique en traction et compression
!
    call dhrc_calc_a_term(6, 6, 1, 1, a0, aa_t, ga_t, aa_c, ga_c, vint, a(6,6), &
                          ap1(6,6), ap2(6,6), as1(6,6), as2(6,6))
!
! -- TERMES DE MEMBRANE-FLEXION
!    do i = 1, 3
!        do j = 4, 6
!            a(i,j)=-(a0(i,j)+0.5d0*(ga_c(i,j,1)*vint(1)/(aa_c(i,j,1)+vint(1))&
!                                   +ga_c(i,j,2)*vint(2)/(aa_c(i,j,2)+vint(2))))

!            ap1(i,j)=-0.5d0*aa_c(i,j,1)*ga_c(i,j,1)/(aa_c(i,j,1)+vint(1))**2.d0
!            ap2(i,j)=-0.5d0*aa_c(i,j,2)*ga_c(i,j,2)/(aa_c(i,j,2)+vint(2))**2.d0

!            as1(i,j)=aa_c(i,j,1)*ga_c(i,j,1)/(aa_c(i,j,1)+vint(1))**3.d0
!            as2(i,j)=aa_c(i,j,2)*ga_c(i,j,2)/(aa_c(i,j,2)+vint(2))**3.d0
!        end do
!    end do
!
    do i = 1, 6
        do j = 1, 6
            a(j,i)  =a(i,j)
            ap1(j,i)=ap1(i,j)
            ap2(j,i)=ap2(i,j)
            as1(j,i)=as1(i,j)
            as2(j,i)=as2(i,j)
        end do
    end do
!
end subroutine dhrc_calc_a
