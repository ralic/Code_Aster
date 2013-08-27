subroutine dhrc_seuils(eps, vint, b, c, ap1,&
                  bp1, cp1, ap2, bp2, cp2,&
                  cstseu, neta1, neta2, seuils)
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
    real(kind=8) :: b(6, 2, 2), c(2, 2, 2)
    real(kind=8) :: ap1(6, 6), bp1(6, 2), cp1(2, 2)
    real(kind=8) :: ap2(6, 6), bp2(6, 2), cp2(2, 2)
    real(kind=8) :: seuils(6), cstseu(2)
    real(kind=8) :: vint(7), eps(6)
!
! ----------------------------------------------------------------------
!
!      CALCUL DES SEUILS D'ENDOMMAGEMENT ET PLASTICITÉ
!      APPELE PAR "LCGLCC"
!
! IN:
!       CSTSEU  : CONSTANTES DE SEUILS
!       A       : TENSEUR DE RAIDEUR ELASTIQUE
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
!       SEUILS  : VALEUR DES SEUILS POUR L'ENDOMMAGEMENT ET LE
!                 GLISSEMENT DONNE
! ----------------------------------------------------------------------
!
    real(kind=8) :: g1, g2
    real(kind=8) :: neta1(2), neta2(2)
!
! ----------------------------------------------------------------------
! -------CALCUL DES FORCES THERMODYNAMIQUES -------
! ----------------------------------------------------------------------
!
    call dhrc_calc_g(eps, vint, ap1, bp1, cp1,&
                ap2, bp2, cp2, g1, g2)
!
    call dhrc_calc_n(eps, vint, b, c, neta1,&
                neta2)
!
! ----------------------------------------------------------------------
! -------CALCUL DES SEUILS-------
! ----------------------------------------------------------------------
!
!     SEUILS D'ENDOMMAGEMENT
!
    seuils(1)=g1/cstseu(1)-1.0d0
    seuils(2)=g2/cstseu(1)-1.0d0
!
!     SEUILS DE PLASTICITE
!
    seuils(3)= (neta1(1)/cstseu(2))**2.0d0-1.0d0
    seuils(4)= (neta1(2)/cstseu(2))**2.0d0-1.0d0
    seuils(5)= (neta2(1)/cstseu(2))**2.0d0-1.0d0
    seuils(6)= (neta2(2)/cstseu(2))**2.0d0-1.0d0
!
end subroutine
