subroutine dhrc_calc_a_term(i, j, sup_s, inf_s, a0, aa_t, ga_t, aa_c, ga_c, vint, &
                            a, ap1, ap2, as1, as2, rvp, sup_d, inf_d)
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
!
    integer, intent(in) :: i, j, sup_s, inf_s
    integer, optional, intent(in) :: sup_d, inf_d
    real(kind=8), optional, intent(in) :: rvp
    real(kind=8), intent(in) :: vint(*)
    real(kind=8), intent(in) :: a0(6, 6)
    real(kind=8), intent(in) :: aa_t(6, 6, 2), ga_t(6, 6, 2), aa_c(6, 6, 2), ga_c(6, 6, 2)
    real(kind=8), intent(out) :: a, ap1, ap2, as1, as2
! ----------------------------------------------------------------------
!
!      CALCUL D UNE COMPOSANTE DU TENSEUR DE RAIDEUR A ET DE SES DERIVEES PAR RAPPORT A D
!
! IN:
!       I, J   : POSITION DANS A DE LA COMPOSANTE A CALCULER
!       SUP_S  : CHOIX DU PARAMETRE EN PARTIE SUP DE LA DALLE
!                1->TRACTION
!                2->COMPRESSION
!       INF_S  : CHOIX DU PARAMETRE EN PARTIE INF DE LA DALLE
!                1->TRACTION
!                2->COMPRESSION
!       RVP    : RAPPORT DES VALEURS PROPRES
!       SUP_D  : CHOIX DU PARAMETRE EN PARTIE SUP DE LA DALLE QUAND RVP=1
!                1->TRACTION
!                2->COMPRESSION
!       INF_D  : CHOIX DU PARAMETRE EN PARTIE INF DE LA DALLE QUAND RVP=1
!                1->TRACTION
!                2->COMPRESSION
!       A0     : RAIDEUR ELASTIQUE (D=0)
!    POUR LA TRACTION :
!       AA_T   : PARAMETRE ALPHA DE LA FONCTION D'ENDOMMAGEMENT
!       GA_T   : PARAMETRE GAMMA DE LA FONCTION D'ENDOMMAGEMENT
!    POUR LA COMPRESSION :
!       AA_C   : PARAMETRE ALPHA DE LA FONCTION D'ENDOMMAGEMENT
!       GA_C   : PARAMETRE GAMMA DE LA FONCTION D'ENDOMMAGEMENT
!       VINT   : VECTEUR DES VARIABLES INTERNES
!                VINT=(D1,D2,EPSP1X,EPSP1Y,EPSP2X,EPSP2Y)
!
! OUT:
!       A     : VALEUR DU TENSEUR DE RAIDEUR ELASTIQUE SITUEE EN (I, J)
!       AP1   : VALEUR DE LA DERIVEE PREMIERE DU TENSEUR DE RAIDEUR ELASTIQUE PAR
!               RAPPORT A D1 SITUEE EN (I, J)
!       AP2   : VALEUR DE LA DERIVEE PREMIERE DU TENSEUR DE RAIDEUR ELASTIQUE PAR
!               RAPPORT A D2 SITUEE EN (I, J)
!       AS1   : VALEUR DE LA DERIVEE SECONDE DU TENSEUR DE RAIDEUR ELASTIQUE PAR
!               RAPPORT A D1 SITUEE EN (I, J)
!       AS2   : VALEUR DE LA DERIVEE SECONDE DU TENSEUR DE RAIDEUR ELASTIQUE PAR
!               RAPPORT A D2 SITUEE EN (I, J)
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: a_d, ap1_d, ap2_d, as1_d, as2_d, unmrvp
!
    if (sup_s .eq. 1 .and.  inf_s .eq. 1) then
        a=0.5d0*a0(i,j)*((aa_t(i,j,1)+ga_t(i,j,1)*vint(1))/(aa_t(i,j,1)+vint(1)) &
                        +(aa_t(i,j,2)+ga_t(i,j,2)*vint(2))/(aa_t(i,j,2)+vint(2)))
!
        ap1=0.5d0*a0(i,j)*aa_t(i,j,1)*(ga_t(i,j,1)-1.d0)/(aa_t(i,j,1)+vint(1))**2.d0
        ap2=0.5d0*a0(i,j)*aa_t(i,j,2)*(ga_t(i,j,2)-1.d0)/(aa_t(i,j,2)+vint(2))**2.d0
!
        as1=-a0(i,j)*aa_t(i,j,1)*(ga_t(i,j,1)-1.d0)/(aa_t(i,j,1)+vint(1))**3.d0
        as2=-a0(i,j)*aa_t(i,j,2)*(ga_t(i,j,2)-1.d0)/(aa_t(i,j,2)+vint(2))**3.d0
    elseif (sup_s .eq. 1 .and.  inf_s .eq. 2) then
        a=0.5d0*a0(i,j)*((aa_t(i,j,1)+ga_t(i,j,1)*vint(1))/(aa_t(i,j,1)+vint(1)) &
                        +(aa_c(i,j,2)+ga_c(i,j,2)*vint(2))/(aa_c(i,j,2)+vint(2)))
!
        ap1=0.5d0*a0(i,j)*aa_t(i,j,1)*(ga_t(i,j,1)-1.d0)/(aa_t(i,j,1)+vint(1))**2.d0
        ap2=0.5d0*a0(i,j)*aa_c(i,j,2)*(ga_c(i,j,2)-1.d0)/(aa_c(i,j,2)+vint(2))**2.d0
!
        as1=-a0(i,j)*aa_t(i,j,1)*(ga_t(i,j,1)-1.d0)/(aa_t(i,j,1)+vint(1))**3.d0
        as2=-a0(i,j)*aa_c(i,j,2)*(ga_c(i,j,2)-1.d0)/(aa_c(i,j,2)+vint(2))**3.d0
    elseif (sup_s .eq. 2 .and.  inf_s .eq. 1) then
        a=0.5d0*a0(i,j)*((aa_c(i,j,1)+ga_c(i,j,1)*vint(1))/(aa_c(i,j,1)+vint(1)) &
                        +(aa_t(i,j,2)+ga_t(i,j,2)*vint(2))/(aa_t(i,j,2)+vint(2)))
!
        ap1=0.5d0*a0(i,j)*aa_c(i,j,1)*(ga_c(i,j,1)-1.d0)/(aa_c(i,j,1)+vint(1))**2.d0
        ap2=0.5d0*a0(i,j)*aa_t(i,j,2)*(ga_t(i,j,2)-1.d0)/(aa_t(i,j,2)+vint(2))**2.d0
!
        as1=-a0(i,j)*aa_c(i,j,1)*(ga_c(i,j,1)-1.d0)/(aa_c(i,j,1)+vint(1))**3.d0
        as2=-a0(i,j)*aa_t(i,j,2)*(ga_t(i,j,2)-1.d0)/(aa_t(i,j,2)+vint(2))**3.d0
    elseif (sup_s .eq. 2 .and.  inf_s .eq. 2) then
        a=0.5d0*a0(i,j)*((aa_c(i,j,1)+ga_c(i,j,1)*vint(1))/(aa_c(i,j,1)+vint(1)) &
                        +(aa_c(i,j,2)+ga_c(i,j,2)*vint(2))/(aa_c(i,j,2)+vint(2)))
!
        ap1=0.5d0*a0(i,j)*aa_c(i,j,1)*(ga_c(i,j,1)-1.d0)/(aa_c(i,j,1)+vint(1))**2.d0
        ap2=0.5d0*a0(i,j)*aa_c(i,j,2)*(ga_c(i,j,2)-1.d0)/(aa_c(i,j,2)+vint(2))**2.d0
!
        as1=-a0(i,j)*aa_c(i,j,1)*(ga_c(i,j,1)-1.d0)/(aa_c(i,j,1)+vint(1))**3.d0
        as2=-a0(i,j)*aa_c(i,j,2)*(ga_c(i,j,2)-1.d0)/(aa_c(i,j,2)+vint(2))**3.d0
    else
! -- ERREUR DE PROGRAMMATION
        ASSERT(.FALSE.)
    end if
!
    if (present(rvp)) then
        if (present(sup_d) .and. present(inf_d)) then
            unmrvp=1.d0-rvp
            if (sup_d .eq. 1 .and.  inf_d .eq. 1) then
                a_d=0.5d0*a0(i,j)*((aa_t(i,j,1)+ga_t(i,j,1)*vint(1))/(aa_t(i,j,1)+vint(1)) &
                                  +(aa_t(i,j,2)+ga_t(i,j,2)*vint(2))/(aa_t(i,j,2)+vint(2)))
!
                ap1_d=0.5d0*a0(i,j)*aa_t(i,j,1)*(ga_t(i,j,1)-1.d0)/(aa_t(i,j,1)+vint(1))**2.d0
                ap2_d=0.5d0*a0(i,j)*aa_t(i,j,2)*(ga_t(i,j,2)-1.d0)/(aa_t(i,j,2)+vint(2))**2.d0
!
                as1_d=-a0(i,j)*aa_t(i,j,1)*(ga_t(i,j,1)-1.d0)/(aa_t(i,j,1)+vint(1))**3.d0
                as2_d=-a0(i,j)*aa_t(i,j,2)*(ga_t(i,j,2)-1.d0)/(aa_t(i,j,2)+vint(2))**3.d0
            elseif (sup_d .eq. 1 .and.  inf_d .eq. 2) then
                a_d=0.5d0*a0(i,j)*((aa_t(i,j,1)+ga_t(i,j,1)*vint(1))/(aa_t(i,j,1)+vint(1)) &
                                  +(aa_c(i,j,2)+ga_c(i,j,2)*vint(2))/(aa_c(i,j,2)+vint(2)))
!
                ap1_d=0.5d0*a0(i,j)*aa_t(i,j,1)*(ga_t(i,j,1)-1.d0)/(aa_t(i,j,1)+vint(1))**2.d0
                ap2_d=0.5d0*a0(i,j)*aa_c(i,j,2)*(ga_c(i,j,2)-1.d0)/(aa_c(i,j,2)+vint(2))**2.d0
!
                as1_d=-a0(i,j)*aa_t(i,j,1)*(ga_t(i,j,1)-1.d0)/(aa_t(i,j,1)+vint(1))**3.d0
                as2_d=-a0(i,j)*aa_c(i,j,2)*(ga_c(i,j,2)-1.d0)/(aa_c(i,j,2)+vint(2))**3.d0
            elseif (sup_d .eq. 2 .and.  inf_d .eq. 1) then
                a_d=0.5d0*a0(i,j)*((aa_c(i,j,1)+ga_c(i,j,1)*vint(1))/(aa_c(i,j,1)+vint(1)) &
                                  +(aa_t(i,j,2)+ga_t(i,j,2)*vint(2))/(aa_t(i,j,2)+vint(2)))
!
                ap1_d=0.5d0*a0(i,j)*aa_c(i,j,1)*(ga_c(i,j,1)-1.d0)/(aa_c(i,j,1)+vint(1))**2.d0
                ap2_d=0.5d0*a0(i,j)*aa_t(i,j,2)*(ga_t(i,j,2)-1.d0)/(aa_t(i,j,2)+vint(2))**2.d0
!
                as1_d=-a0(i,j)*aa_c(i,j,1)*(ga_c(i,j,1)-1.d0)/(aa_c(i,j,1)+vint(1))**3.d0
                as2_d=-a0(i,j)*aa_t(i,j,2)*(ga_t(i,j,2)-1.d0)/(aa_t(i,j,2)+vint(2))**3.d0
            elseif (sup_d .eq. 2 .and.  inf_d .eq. 2) then
                a_d=0.5d0*a0(i,j)*((aa_c(i,j,1)+ga_c(i,j,1)*vint(1))/(aa_c(i,j,1)+vint(1)) &
                                  +(aa_c(i,j,2)+ga_c(i,j,2)*vint(2))/(aa_c(i,j,2)+vint(2)))
!
                ap1_d=0.5d0*a0(i,j)*aa_c(i,j,1)*(ga_c(i,j,1)-1.d0)/(aa_c(i,j,1)+vint(1))**2.d0
                ap2_d=0.5d0*a0(i,j)*aa_c(i,j,2)*(ga_c(i,j,2)-1.d0)/(aa_c(i,j,2)+vint(2))**2.d0
!
                as1_d=-a0(i,j)*aa_c(i,j,1)*(ga_c(i,j,1)-1.d0)/(aa_c(i,j,1)+vint(1))**3.d0
                as2_d=-a0(i,j)*aa_c(i,j,2)*(ga_c(i,j,2)-1.d0)/(aa_c(i,j,2)+vint(2))**3.d0
            else
! -- ERREUR DE PROGRAMMATION
                ASSERT(.FALSE.)
            end if
!
            a   = a  *unmrvp+a_d  *rvp
            ap1 = ap1*unmrvp+ap1_d*rvp
            ap2 = ap2*unmrvp+ap2_d*rvp
            as1 = as1*unmrvp+as1_d*rvp
            as2 = as2*unmrvp+as2_d*rvp
!
        else
! -- ERREUR DE PROGRAMMATION
        ASSERT(.FALSE.)
        end if
    end if
!
end subroutine dhrc_calc_a_term
