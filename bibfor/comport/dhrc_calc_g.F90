subroutine dhrc_calc_g(eps, vint, ap1, bp1, cp1, ap2, bp2, cp2, g1, g2)
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
    real(kind=8) :: ap1(6, 6), bp1(6, 2), cp1(2, 2)
    real(kind=8) :: ap2(6, 6), bp2(6, 2), cp2(2, 2)
    real(kind=8) :: vint(*), eps(6)
!
    real(kind=8) :: g1, g2
!
! ----------------------------------------------------------------------
!
!      CALCUL DES FORCES THERMODYNAMIQUES ASSOCIEES A L'ENDOMMAGEMENT
!      APPELE PAR "SEUGLC"
!
! IN:
!       EPS     : TENSEUR DES DEFORMATIONS
!       A       : TENSEUR ELASTIQUE ENDOMMAGE
!       B       : TENSEUR ASSOCIE AUX DEFORMATIONS PLASTIQUES
!       C       : TENSEUR DE RAIDEUR D'ÉCROUISSAGE PLASTIQUE
!       LA TROISIEME COMPOSANTE DE B ET C CORRESPOND A LA DISTINCTION
!       ENTRE PARTIE SUPERIEURE ET INFERIEURE DE LA PLAQUE
!       AP1     : DERIVEE DU TENSEUR A PAR RAPPORT A D1
!       BP1     : DERIVEE DU TENSEUR B PAR RAPPORT A D1
!       CP1     : DERIVEE DU TENSEUR C PAR RAPPORT A D1
!       AP2     : DERIVEE DU TENSEUR A PAR RAPPORT A D2
!       BP2     : DERIVEE DU TENSEUR B PAR RAPPORT A D2
!       CP2     : DERIVEE DU TENSEUR C PAR RAPPORT A D2
!       VINT   : VECTEUR DES VARIABLES INTERNES
!                VINT=(D1,D2,EPSP1X,EPSP1Y,EPSP2X,EPSP2Y)
!
! OUT:
!       G1      : TAUX DE RESTITUTION D'ENERGIE POUR D1
!       G2      : TAUX DE RESTITUTION D'ENERGIE POUR D2
!
! ----------------------------------------------------------------------
!
    integer :: i, k
!
    g1=0.0d0
    g2=0.0d0
!
    do k = 1, 6
        do i = 1, 6
            g1 = g1 - eps(k) * ap1(k,i) * eps(i)
            g2 = g2 - eps(k) * ap2(k,i) * eps(i)
            if (i .lt. 3) then
                g1 = g1 - eps(k) * bp1(k,i) * vint(i+2)
                g2 = g2 - eps(k) * bp2(k,i) * vint(i+4)
                if (k .lt. 3) then
                    g1 = g1 - vint(k+2) * cp1(k,i) * vint(i+2)
                    g2 = g2 - vint(k+4) * cp2(k,i) * vint(i+4)
                endif
            endif
        end do
    end do
!
    g1 = g1*0.5d0
    g2 = g2*0.5d0
!
end subroutine
