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
#include "asterfort/matini.h"
#include "asterc/r8prem.h"
!
    real(kind=8) :: vint(*), eps(*)
    real(kind=8) :: a0(6, 6)
    real(kind=8) :: aa_t(6, 6, 2), ga_t(6, 6, 2), aa_c(6, 6, 2), ga_c(6, 6, 2)
    real(kind=8) :: a(6, 6), ap1(6, 6), ap2(6, 6), as1(6, 6), as2(6, 6)
! ----------------------------------------------------------------------
!
!      CALCUL DU TENSEUR DE RAIDEUR A ET DE SES DERIVEES PAR RAPPORT A D
!      APPELE PAR "SEUGLC"
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
    real(kind=8) :: treps, trkap

! -- POUR LES TERMES DE MF POSITION D EVALUATION DE LA DEFORMATION POUR DISTINGUER TRAC-COMP
!
    call matini(6, 6, 0.d0, a)
    call matini(6, 6, 0.d0, ap1)
    call matini(6, 6, 0.d0, ap2)
    call matini(6, 6, 0.d0, as1)
    call matini(6, 6, 0.d0, as2)
!
    treps=eps(1)+eps(2)
    trkap=eps(4)+eps(5)
!
! -- TERMES DE MEMBRANE PURE
    do i = 1, 3
        if (eps(i) .ge. -r8prem()) then
            a(i,i)=0.5d0*a0(i,i)*((aa_t(i,i,1)+ga_t(i,i,1)*vint(1))/(aa_t(i,i,1)+vint(1)) &
                                 +(aa_t(i,i,2)+ga_t(i,i,2)*vint(2))/(aa_t(i,i,2)+vint(2)))
!
            ap1(i,i)=0.5d0*a0(i,i)*aa_t(i,i,1)*(ga_t(i,i,1)-1.d0)/(aa_t(i,i,1)+vint(1))**2.d0
            ap2(i,i)=0.5d0*a0(i,i)*aa_t(i,i,2)*(ga_t(i,i,2)-1.d0)/(aa_t(i,i,2)+vint(2))**2.d0
!
            as1(i,i)=-a0(i,i)*aa_t(i,i,1)*(ga_t(i,i,1)-1.d0)/(aa_t(i,i,1)+vint(1))**3.d0
            as2(i,i)=-a0(i,i)*aa_t(i,i,2)*(ga_t(i,i,2)-1.d0)/(aa_t(i,i,2)+vint(2))**3.d0
!
        else
            a(i,i)=0.5d0*a0(i,i)*((aa_c(i,i,1)+ga_c(i,i,1)*vint(1))/(aa_c(i,i,1)+vint(1)) &
                                 +(aa_c(i,i,2)+ga_c(i,i,2)*vint(2))/(aa_c(i,i,2)+vint(2)))
!
            ap1(i,i)=0.5d0*a0(i,i)*aa_c(i,i,1)*(ga_c(i,i,1)-1.d0)/(aa_c(i,i,1)+vint(1))**2.d0
            ap2(i,i)=0.5d0*a0(i,i)*aa_c(i,i,2)*(ga_c(i,i,2)-1.d0)/(aa_c(i,i,2)+vint(2))**2.d0
!
            as1(i,i)=-a0(i,i)*aa_c(i,i,1)*(ga_c(i,i,1)-1.d0)/(aa_c(i,i,1)+vint(1))**3.d0
            as2(i,i)=-a0(i,i)*aa_c(i,i,2)*(ga_c(i,i,2)-1.d0)/(aa_c(i,i,2)+vint(2))**3.d0
        endif
!
        do j = 1, 3
            if (j .ne. i) then
                if (treps .ge. -r8prem()) then
               a(i,j)=0.5d0*a0(i,j)*((aa_t(i,j,1)+ga_t(i,j,1)*vint(1))/(aa_t(i,j,1)+vint(1)) &
                                    +(aa_t(i,j,2)+ga_t(i,j,2)*vint(2))/(aa_t(i,j,2)+vint(2)))
!
               ap1(i,j)=0.5d0*a0(i,j)*aa_t(i,j,1)*(ga_t(i,j,1)-1.d0)/(aa_t(i,j,1)+vint(1))**2.d0
               ap2(i,j)=0.5d0*a0(i,j)*aa_t(i,j,2)*(ga_t(i,j,2)-1.d0)/(aa_t(i,j,2)+vint(2))**2.d0
!
               as1(i,j)=-a0(i,j)*aa_t(i,j,1)*(ga_t(i,j,1)-1.d0)/(aa_t(i,j,1)+vint(1))**3.d0
               as2(i,j)=-a0(i,j)*aa_t(i,j,2)*(ga_t(i,j,2)-1.d0)/(aa_t(i,j,2)+vint(2))**3.d0
!
                else
               a(i,j)=0.5d0*a0(i,j)*((aa_c(i,j,1)+ga_c(i,j,1)*vint(1))/(aa_c(i,j,1)+vint(1)) &
                                    +(aa_c(i,j,2)+ga_c(i,j,2)*vint(2))/(aa_c(i,j,2)+vint(2)))
!
               ap1(i,j)=0.5d0*a0(i,j)*aa_c(i,j,1)*(ga_c(i,j,1)-1.d0)/(aa_c(i,j,1)+vint(1))**2.d0
               ap2(i,j)=0.5d0*a0(i,j)*aa_c(i,j,2)*(ga_c(i,j,2)-1.d0)/(aa_c(i,j,2)+vint(2))**2.d0
!
               as1(i,j)=-a0(i,j)*aa_c(i,j,1)*(ga_c(i,j,1)-1.d0)/(aa_c(i,j,1)+vint(1))**3.d0
               as2(i,j)=-a0(i,j)*aa_c(i,j,2)*(ga_c(i,j,2)-1.d0)/(aa_c(i,j,2)+vint(2))**3.d0
                endif
            endif
        end do
    end do
!
! -- TERMES DE FLEXION PURE
! -- DANS LES DKTG LA COURBURE EST EGALE A - LA DERIVEE DE LA FLECHE
! -- C EST POURQUOI ON TESTE L OPPOSE DE LA COURBURE
    do i = 4, 6
        if (eps(i) .lt. -r8prem()) then
            a(i,i)=0.5d0*a0(i,i)*((aa_c(i,i,1)+ga_c(i,i,1)*vint(1))/(aa_c(i,i,1)+vint(1)) &
                                 +(aa_t(i,i,2)+ga_t(i,i,2)*vint(2))/(aa_t(i,i,2)+vint(2)))
!
            ap1(i,i)=0.5d0*a0(i,i)*aa_c(i,i,1)*(ga_c(i,i,1)-1.d0)/(aa_c(i,i,1)+vint(1))**2.d0
            ap2(i,i)=0.5d0*a0(i,i)*aa_t(i,i,2)*(ga_t(i,i,2)-1.d0)/(aa_t(i,i,2)+vint(2))**2.d0
!
            as1(i,i)=-a0(i,i)*aa_c(i,i,1)*(ga_c(i,i,1)-1.d0)/(aa_c(i,i,1)+vint(1))**3.d0
            as2(i,i)=-a0(i,i)*aa_t(i,i,2)*(ga_t(i,i,2)-1.d0)/(aa_t(i,i,2)+vint(2))**3.d0
!
        else
            a(i,i)=0.5d0*a0(i,i)*((aa_t(i,i,1)+ga_t(i,i,1)*vint(1))/(aa_t(i,i,1)+vint(1)) &
                                 +(aa_c(i,i,2)+ga_c(i,i,2)*vint(2))/(aa_c(i,i,2)+vint(2)))
!
            ap1(i,i)=0.5d0*a0(i,i)*aa_t(i,i,1)*(ga_t(i,i,1)-1.d0)/(aa_t(i,i,1)+vint(1))**2.d0
            ap2(i,i)=0.5d0*a0(i,i)*aa_c(i,i,2)*(ga_c(i,i,2)-1.d0)/(aa_c(i,i,2)+vint(2))**2.d0
!
            as1(i,i)=-a0(i,i)*aa_t(i,i,1)*(ga_t(i,i,1)-1.d0)/(aa_t(i,i,1)+vint(1))**3.d0
            as2(i,i)=-a0(i,i)*aa_c(i,i,2)*(ga_c(i,i,2)-1.d0)/(aa_c(i,i,2)+vint(2))**3.d0
        endif
!
        do j = 4, 6
            if (j .ne. i) then
                if (trkap .ge. -r8prem()) then
                a(i,j)=0.5d0*a0(i,j)*((aa_c(i,j,1)+ga_c(i,j,1)*vint(1))/(aa_c(i,j,1)+vint(1)) &
                                     +(aa_t(i,j,2)+ga_t(i,j,2)*vint(2))/(aa_t(i,j,2)+vint(2)))
!
                ap1(i,j)=0.5d0*a0(i,j)*aa_c(i,j,1)*(ga_c(i,j,1)-1.d0)/(aa_c(i,j,1)+vint(1))**2.d0
                ap2(i,j)=0.5d0*a0(i,j)*aa_t(i,j,2)*(ga_t(i,j,2)-1.d0)/(aa_t(i,j,2)+vint(2))**2.d0
!
                as1(i,j)=-a0(i,j)*aa_c(i,j,1)*(ga_c(i,j,1)-1.d0)/(aa_c(i,j,1)+vint(1))**3.d0
                as2(i,j)=-a0(i,j)*aa_t(i,j,2)*(ga_t(i,j,2)-1.d0)/(aa_t(i,j,2)+vint(2))**3.d0
!
                else
                a(i,j)=0.5d0*a0(i,j)*((aa_t(i,j,1)+ga_t(i,j,1)*vint(1))/(aa_t(i,j,1)+vint(1)) &
                                     +(aa_c(i,j,2)+ga_c(i,j,2)*vint(2))/(aa_c(i,j,2)+vint(2)))
!
                ap1(i,j)=0.5d0*a0(i,j)*aa_t(i,j,1)*(ga_t(i,j,1)-1.d0)/(aa_t(i,j,1)+vint(1))**2.d0
                ap2(i,j)=0.5d0*a0(i,j)*aa_c(i,j,2)*(ga_c(i,j,2)-1.d0)/(aa_c(i,j,2)+vint(2))**2.d0
!
                as1(i,j)=-a0(i,j)*aa_t(i,j,1)*(ga_t(i,j,1)-1.d0)/(aa_t(i,j,1)+vint(1))**3.d0
                as2(i,j)=-a0(i,j)*aa_c(i,j,2)*(ga_c(i,j,2)-1.d0)/(aa_c(i,j,2)+vint(2))**3.d0
                endif
            endif
        end do
    end do
!
! -- TERMES DE MEMBRANE-FLEXION
    do i = 1, 3
        a(i,i+3)=(a0(i,i+3)+0.25d0*(ga_t(i,i+3,1)*vint(1)/(aa_t(i,i+3,1)+vint(1))&
                                    +ga_t(i,i+3,2)*vint(2)/(aa_t(i,i+3,2)+vint(2))&
                                    +ga_c(i,i+3,1)*vint(1)/(aa_c(i,i+3,1)+vint(1))&
                                    +ga_c(i,i+3,2)*vint(2)/(aa_c(i,i+3,2)+vint(2))))
!
        ap1(i,i+3)=0.25d0*(aa_t(i,i+3,1)*ga_t(i,i+3,1)/(aa_t(i,i+3,1)+vint(1))**2.d0&
                           +aa_c(i,i+3,1)*ga_c(i,i+3,1)/(aa_c(i,i+3,1)+vint(1))**2.d0)
        ap2(i,i+3)=0.25d0*(aa_t(i,i+3,2)*ga_t(i,i+3,2)/(aa_t(i,i+3,2)+vint(2))**2.d0&
                           +aa_c(i,i+3,2)*ga_c(i,i+3,2)/(aa_c(i,i+3,2)+vint(2))**2.d0)
!
        as1(i,i+3)=-0.5*(aa_t(i,i+3,1)*ga_t(i,i+3,1)/(aa_t(i,i+3,1)+vint(1))**3.d0&
                       +aa_c(i,i+3,1)*ga_c(i,i+3,1)/(aa_c(i,i+3,1)+vint(1))**3.d0)
        as2(i,i+3)=-0.5*(aa_t(i,i+3,2)*ga_t(i,i+3,2)/(aa_t(i,i+3,2)+vint(2))**3.d0&
                       +aa_c(i,i+3,2)*ga_c(i,i+3,2)/(aa_c(i,i+3,2)+vint(2))**3.d0)
        do j = 4, 6
            if (j .ne. (i+3)) then
                a(i,j)=(a0(i,j)+0.25d0*(ga_t(i,j,1)*vint(1)/(aa_t(i,j,1)+vint(1))&
                                        +ga_t(i,j,2)*vint(2)/(aa_t(i,j,2)+vint(2))&
                                        +ga_c(i,j,1)*vint(1)/(aa_c(i,j,1)+vint(1))&
                                        +ga_c(i,j,2)*vint(2)/(aa_c(i,j,2)+vint(2))))
!
                    ap1(i,j)=0.25d0*(aa_t(i,j,1)*ga_t(i,j,1)/(aa_t(i,j,1)+vint(1))**2.d0&
                                     +aa_c(i,j,1)*ga_c(i,j,1)/(aa_c(i,j,1)+vint(1))**2.d0)
                    ap2(i,j)=0.25d0*(aa_t(i,j,2)*ga_t(i,j,2)/(aa_t(i,j,2)+vint(2))**2.d0&
                                     +aa_c(i,j,2)*ga_c(i,j,2)/(aa_c(i,j,2)+vint(2))**2.d0)
!
                    as1(i,j)=-0.5*(aa_t(i,j,1)*ga_t(i,j,1)/(aa_t(i,j,1)+vint(1))**3.d0&
                                 +aa_c(i,j,1)*ga_c(i,j,1)/(aa_c(i,j,1)+vint(1))**3.d0)
                    as2(i,j)=-0.5*(aa_t(i,j,2)*ga_t(i,j,2)/(aa_t(i,j,2)+vint(2))**3.d0&
                                 +aa_c(i,j,2)*ga_c(i,j,2)/(aa_c(i,j,2)+vint(2))**3.d0)
            end if
        end do
    end do
!
    do i = 1, 3
        do j = 1, 3
            a(j+3,i)  =a(i,j+3)
            ap1(j+3,i)=ap1(i,j+3)
            ap2(j+3,i)=ap2(i,j+3)
            as1(j+3,i)=as1(i,j+3)
            as2(j+3,i)=as2(i,j+3)
        end do
    end do
!
end subroutine dhrc_calc_a

