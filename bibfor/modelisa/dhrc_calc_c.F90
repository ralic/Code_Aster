subroutine dhrc_calc_c(c0, ac, gc, vint, c, cp1, cp2, cs1, cs2)
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
!
#include "asterfort/matini.h"
    real(kind=8) :: vint(7)
    real(kind=8) :: c0(2, 2, 2)
    real(kind=8) :: ac(2, 2, 2), gc(2, 2, 2)
!
    real(kind=8) :: c(2, 2, 2), cp1(2, 2), cp2(2, 2), cs1(2, 2), cs2(2, 2)
! ----------------------------------------------------------------------
!
!      CALCUL DU TENSEUR DE RAIDEUR B ET DE SES DERIVVES PAR RAPPORT A D
!      APPELE PAR "LCGLCC"
!
! IN:
!       VINT   : VECTEUR DES VARIABLES INTERNES
!                VINT=(D1,D2,EPSP1X,EPSP1Y,EPSP2X,EPSP2Y)
!       C0     : RAIDEUR ELASTIQUE (D=0)
!       AC     : PARAMETRE ALPHA DE LA FONCTION D'ENDOMMAGEMENT
!       GC     : PARAMETRE GAMMA DE LA FONCTION D'ENDOMMAGEMENT
!
! OUT:
!       C     : TENSEUR DE RAIDEUR D'ECROUISSAGE
!       LA TROISIEME COMPOSANTE DE C CORRESPOND A LA DISTINCTION
!       ENTRE PARTIE SUPERIEURE ET INFERIEURE DE LA PLAQUE
!       CP1   : DERIVEE PREMIERE DU TENSEUR DE RAIDEUR ECROUISSAGE PAR
!               RAPPORT A D1
!       CP2   : DERIVEE PREMIERE DU TENSEUR DE RAIDEUR ECROUISSAGE PAR
!               RAPPORT A D2
!       CS1   : DERIVEE SECONDE DU TENSEUR DE RAIDEUR ECROUISSAGE PAR
!               RAPPORT A D1
!       CS2   : DERIVEE SECONDE DU TENSEUR DE RAIDEUR ECROUISSAGE PAR
!               RAPPORT A D2
!
! -------------------------------------------------------------------
!
    integer :: k, l
!
    call matini(2, 2, 0.0d0, cp1)
    call matini(2, 2, 0.0d0, cp2)
    call matini(2, 2, 0.0d0, cs1)
    call matini(2, 2, 0.0d0, cs2)
!
    do k = 1, 2
        do l = 1, 2
!
! --      ON DIVISE C0 PAR 2 DANS A CAR ON SOMME LES DEUX FONCTIONS
!         D'ENDOMMAGEMENT CE QUI FAIT UN FACTEUR 2 A D=0
!
            c(k,l,1)=c0(k,l,1)*0.5d0* (ac(k,l,1)+gc(k,l,1)*vint(1))/(ac(k,l,1)+vint(1))
!
            cp1(k,l)= c0(k,l,1)*0.5d0* ac(k,l,1)*(gc(k,l,1)-1.0d0) /(ac(k,l,1)+vint(1))**2
!
            cs1(k,l)=-c0(k,l,1)* ac(k,l,1)*(gc(k,l,1)-1.0d0) /(ac(k,l,1)+vint(1))**3
!
            c(k,l,2)=c0(k,l,2)*0.5d0* (ac(k,l,2)+gc(k,l,2)*vint(2))/(ac(k,l,2)+vint(2))
!
            cp2(k,l)= c0(k,l,2)*0.5d0* ac(k,l,2)*(gc(k,l,2)-1.0d0)/(ac(k,l,2)+vint(2))**2
!
            cs2(k,l)=-c0(k,l,2)* ac(k,l,2)*(gc(k,l,2)-1.0d0) /(ac(k,l,2)+vint(2))**3
        end do
    end do
!
end subroutine
