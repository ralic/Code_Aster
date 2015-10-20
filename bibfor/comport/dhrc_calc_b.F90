subroutine dhrc_calc_b(ab, gb, vint, b, bp1, bp2, bs1, bs2)
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
    real(kind=8) :: vint(*)
    real(kind=8) :: ab(6, 2, 2), gb(6, 2, 2)
!
!
    real(kind=8) :: b(6, 2, 2), bp1(6, 2), bp2(6, 2), bs1(6, 2), bs2(6, 2)
! ----------------------------------------------------------------------
!
!      CALCUL DU TENSEUR DE RAIDEUR B ET DE SES DERIVVES PAR RAPPORT A D
!      APPELE PAR "LCGLCC"
!
! IN:
!       VINT   : VECTEUR DES VARIABLES INTERNES
!                VINT=(D1,D2,EPSP1X,EPSP1Y,EPSP2X,EPSP2Y)
!       AB     : PARAMETRE ALPHA DE LA FONCTION D'ENDOMMAGEMENT
!       GB     : PARAMETRE GAMMA DE LA FONCTION D'ENDOMMAGEMENT
!
! OUT:
!       B     : TENSEUR DE RAIDEUR COUPLE ELAS-PLAS
!       LA PREMIERE COMPOSANTE DE B CORRESPOND AUX DEFORMATIONS M-F
!       LA DEUXIEME COMPOSANTE DE B CORRESPOND AUX GLISSEMENTS
!       LA TROISIEME COMPOSANTE DE B CORRESPOND A LA DISTINCTION
!       ENTRE PARTIE SUPERIEURE ET INFERIEURE DE LA PLAQUE
!       BP1   : DERIVEE PREMIERE DU TENSEUR DE RAIDEUR COUPLE PAR
!               RAPPORT A D1
!       BP2   : DERIVEE PREMIERE DU TENSEUR DE RAIDEUR COUPLE PAR
!               RAPPORT A D2
!       BS1   : DERIVEE SECONDE DU TENSEUR DE RAIDEUR COUPLE PAR
!               RAPPORT A D1
!       BS2   : DERIVEE SECONDE DU TENSEUR DE RAIDEUR COUPLE PAR
!               RAPPORT A D2
!
! -------------------------------------------------------------------
!
    integer :: i, k
!
!
    call matini(6, 2, 0.0d0, bp1)
    call matini(6, 2, 0.0d0, bp2)
    call matini(6, 2, 0.0d0, bs1)
    call matini(6, 2, 0.0d0, bs2)
!
    do i = 1, 6
        do k = 1, 2
!
            b(i,k,1)=gb(i,k,1)*vint(1)/(ab(i,k,1)+vint(1))
            b(i,k,2)=gb(i,k,2)*vint(2)/(ab(i,k,2)+vint(2))
!
            bp1(i,k)=ab(i,k,1)*gb(i,k,1)/(ab(i,k,1)+vint(1))**2
            bp2(i,k)=ab(i,k,2)*gb(i,k,2)/(ab(i,k,2)+vint(2))**2
!
            bs1(i,k)=-2.d0*ab(i,k,1)*gb(i,k,1)/(ab(i,k,1)+vint(1))**3
            bs2(i,k)=-2.d0*ab(i,k,2)*gb(i,k,2)/(ab(i,k,2)+vint(2))**3
!
        end do
    end do
!
end subroutine
