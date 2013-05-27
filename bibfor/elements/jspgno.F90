subroutine jspgno(l, a, b)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    real(kind=8) :: a(21), b(14), l
!-----------------------------------------------------------------------
!    PERMET LE PASSAGE DU CHAMPS DE CONTRAINTES CALCULEES AUX POINTS DE
!    GAUSS AUX NOEUDS
!
!     EN ENTREE :
!                L : LONGUEUR DE L'ELEMENT
!                A : VECTEUR DES EFFORTS GENERALISES AUX POINTS
!                    D'INTEGRATION
!     EN SORTIE :
!                B : VECTEUR DES EFFORTS GENERALISES AUX NOEUDS
!-----------------------------------------------------------------------
    real(kind=8) :: mfypg1, mfypg2, mfzpg1, mfzpg2, mtpg1, mtpg2
    real(kind=8) :: const1, const2, const3, xpg
!
!-----------------------------------------------------------------------
    real(kind=8) :: deux, undemi
!-----------------------------------------------------------------------
    undemi = 0.5d0
    deux = 2.0d0
    xpg = sqrt(0.6d0)
!
! --- AFFECTATION DES EFFORTS NORMAUX ET DES BIMOMENTS AUX
! --- POINTS D'INTEGRATION AUX EFFORTS NODAUX :
!     ---------------------------------------
    b(1) = a(1)
    b(7) = a(7)
    b(8) = a(15)
    b(14)= a(21)
!
! --- NOTATIONS PLUS PARLANTES :
!     ------------------------
    mtpg1 = a(4)
    mfypg1 = a(5)
    mfzpg1 = a(6)
    mtpg2 = a(11)
    mfypg2 = a(12)
    mfzpg2 = a(13)
!
! --- EXTRAPOLATION LINEAIRE DES MOMENTS DE FLEXION AUX NOEUDS
! --- L'ABSCISSE DU PREMIER NOEUD SUR L'ELEMENT DE REFERENCE EST -1
! --- L'ABSCISSE DU SECOND NOEUD SUR L'ELEMENT DE REFERENCE EST +1 :
!     ------------------------------------------------------------
    const1 = -deux*(mtpg1 - mtpg2)/(l*xpg)
    const2 = -deux*(mfypg1 - mfypg2)/(l*xpg)
    const3 = -deux*(mfzpg1 - mfzpg2)/(l*xpg)
!
    b(4) = -const1*undemi*l + mtpg2
    b(5) = -const2*undemi*l + mfypg2
    b(6) = -const3*undemi*l + mfzpg2
!
    b(11) = const1*undemi*l + mtpg2
    b(12) = const2*undemi*l + mfypg2
    b(13) = const3*undemi*l + mfzpg2
!
! --- DETERMINATION DES EFFORTS TRANCHANTS PAR LES EQUATIONS D'EQUILIBRE
! --- RELIANT LES EFFORTS TRANCHANTS AUX MOMENTS DE FLEXION
! ---     VY + D(MFZ)/DX = 0
! ---     VZ - D(MFY)/DX = 0
! --- LES DERIVEES DES MOMENTS DE FLEXION ETANT APPROXIMEES PAR
! --- DIFFERENCES FINIES :
!     ------------------
    b(2) = -deux*(mfzpg2-mfzpg1)/(l*xpg)
    b(3) = deux*(mfypg2-mfypg1)/(l*xpg)
    b(9) = b(2)
    b(10)=  b(3)
!
end subroutine
