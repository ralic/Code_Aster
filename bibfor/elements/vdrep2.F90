subroutine vdrep2(alpha, beta, zilzi, zrlzr, matevn,&
                  matevg)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!.======================================================================
    implicit none
!
!      VDREP2   -- DETERMINATION DES MATRICES DE PASSAGE
!                  DES REPERES INTRINSEQUES AUX NOEUDS  DE L'ELEMENT
!                  AU REPERE UTILISATEUR (MATRICE MATEVN)
!                  ET DES REPERES INTRINSEQUES AUX POINTS D'INTEGRATION
!                  DE L'ELEMENT AU REPERE UTILISATEUR (MATRICE MATEVG)
!                  POUR LES ELEMENTS DE COQUE EPAISSE 3D .
!
!   ARGUMENT        E/S   TYPE         ROLE
!    ALPHA, BETA    IN     R    ANGLES DETERMINANT LE REPERE UTILISATEUR
!    MATEVN(2,2,10) OUT    R        MATRICES DE PASSAGE DES REPERES
!                                   INTRINSEQUES AUX NOEUDS  DE
!                                   L'ELEMENT AU REPERE UTILISATEUR
!    MATEVG(2,2,10) OUT    R        MATRICES DE PASSAGE DES REPERES
!                                   INTRINSEQUES AUX POINTS
!                                   D'INTEGRATION DE L'ELEMENT AU
!                                   REPERE UTILISATEUR
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "asterc/r8dgrd.h"
#include "asterfort/coqrep.h"
    real(kind=8) :: matevn(2, 2, 1), matevg(2, 2, 1)
! -----  VARIABLES LOCALES
    real(kind=8) :: pgl(3, 3), zrlzr(*)
    integer :: zilzi(*)
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- NOMBRE DE NOEUDS DE L'ELEMENT  :
!     -----------------------------
!-----------------------------------------------------------------------
    integer :: i, idec, igau, ino, j, k, nb2
    integer :: npgsr
    real(kind=8) :: alpha, beta, c
    real(kind=8) :: s
    real(kind=8) :: r8bid4(4)
!-----------------------------------------------------------------------
    nb2 = zilzi(2)
!
! --- NOMBRE DE POINTS D'INTEGRATION DE L'ELEMENT (SOUS-INTEGRE) :
!     ----------------------------------------------------------
    npgsr=zilzi(3)
!
! --- RECUPERATION DES ANGLES DETERMINANT LE REPERE UTILISATEUR
! --- PAR RAPPORT AU REPERE GLOBAL :
!     ============================
    alpha = alpha * r8dgrd()
    beta = beta * r8dgrd()
!
! --- DETERMINATION DES MATRICES DE PASSAGE DES REPERES INTRINSEQUES
! --- AUX NOEUDS DE L'ELEMENT AU REPERE UTILISATEUR :
!     =============================================
!
! --- ADRESSE DES MATRICES DE PASSAGE DU REPERE GLOBAL AUX REPERES
! --- INTRINSEQUES AUX NOEUDS DE L'ELEMENT DANS LE TABLEAU .DESR :
!     ----------------------------------------------------------
    idec = 1090
!
! --- BOUCLE SUR LES NOEUDS DE L'ELEMENT :
!     ----------------------------------
    do 10 ino = 1, nb2
!
! ---   RECUPERATION DE LA MATRICE DE PASSAGE AU NOEUD COURANT :
!       ------------------------------------------------------
        k = 0
        do 20 j = 1, 3
            do 30 i = 1, 3
                k = k + 1
                pgl(i,j) = zrlzr(idec+(ino-1)*9+k)
30          continue
20      continue
!
! ---   DETERMINATION DE LA PROJECTION DU VECTEUR X DU REPERE
! ---   UTILISATEUR SUR LE FEUILLET TANGENT A LA COQUE AU NOEUD
! ---   COURANT :
!       -------
        call coqrep(pgl, alpha, beta, r8bid4, r8bid4,&
                    c, s)
!
        matevn(1,1,ino) = c
        matevn(2,1,ino) = s
        matevn(1,2,ino) = -s
        matevn(2,2,ino) = c
!
10  end do
!
! --- DETERMINATION DES MATRICES DE PASSAGE DES REPERES INTRINSEQUES
! --- AUX POINTS D'INTEGRATION DE L'ELEMENT AU REPERE UTILISATEUR :
!     ===========================================================
!
! --- ADRESSE DES MATRICES DE PASSAGE DU REPERE GLOBAL AUX REPERES
! --- INTRINSEQUES AUX POINTS D'INTEGRATION DE L'ELEMENT
! --- DANS LE TABLEAU .DESR :
!     ---------------------
    idec = 2000
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION DE L'ELEMENT (SOUS-INTEGRE) :
!     --------------------------------------------------------------
    do 40 igau = 1, npgsr
!
! ---   RECUPERATION DE LA MATRICE DE PASSAGE AU POINT D'INTEGRATION
! ---   COURANT :
!       -------
        k = 0
        do 50 j = 1, 3
            do 60 i = 1, 3
                k = k + 1
                pgl(i,j) = zrlzr(idec+(igau-1)*9+k)
60          continue
50      continue
!
! ---   DETERMINATION DE LA PROJECTION DU VECTEUR X DU REPERE
! ---   UTILISATEUR SUR LE FEUILLET TANGENT A LA COQUE AU POINT
! ---   D'INTEGRATION COURANT :
!       ---------------------
        call coqrep(pgl, alpha, beta, r8bid4, r8bid4,&
                    c, s)
!
        matevg(1,1,igau) = c
        matevg(2,1,igau) = s
        matevg(1,2,igau) = -s
        matevg(2,2,igau) = c
!
40  end do
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
