subroutine vdrepe(nomtez, matevn, matevg)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!.======================================================================
    implicit none
!
!      VDREPE   -- DETERMINATION DES MATRICES DE PASSAGE
!                  DES REPERES INTRINSEQUES AUX NOEUDS  DE L'ELEMENT
!                  AU REPERE UTILISATEUR (MATRICE MATEVN)
!                  ET DES REPERES INTRINSEQUES AUX POINTS D'INTEGRATION
!                  DE L'ELEMENT AU REPERE UTILISATEUR (MATRICE MATEVG)
!                  POUR LES ELEMENTS DE COQUE EPAISSE 3D .
!
!   ARGUMENT        E/S   TYPE         ROLE
!    NOMTE          IN     K*       NOM DU TYPE D'ELEMENT
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
    include 'jeveux.h'
    include 'asterc/r8dgrd.h'
    include 'asterfort/coqrep.h'
    include 'asterfort/jevech.h'
    include 'asterfort/jevete.h'
    character(len=*) :: nomtez
    real(kind=8) :: matevn(2, 2, 1), matevg(2, 2, 1)
! -----  VARIABLES LOCALES
    character(len=16) :: nomte
    real(kind=8) :: pgl(3, 3), r8bid4(4)
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- INITIALISATIONS :
!     ---------------
!-----------------------------------------------------------------------
    integer :: i, idec, igau, ino, j, jcoqu, k
    integer :: lzi, lzr, nb2, npgsr
    real(kind=8) :: alpha, beta, c
    real(kind=8) :: s
!-----------------------------------------------------------------------
    nomte = nomtez
!
! --- RECUPERATION DES OBJETS DESCRIPTEURS DES ELEMENTS :
!     =================================================
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
!
! --- NOMBRE DE NOEUDS DE L'ELEMENT  :
!     -----------------------------
    nb2 = zi(lzi-1+2)
!
! --- NOMBRE DE POINTS D'INTEGRATION DE L'ELEMENT (SOUS-INTEGRE) :
!     ----------------------------------------------------------
    npgsr= zi(lzi-1+3)
!
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
! --- RECUPERATION DES ANGLES DETERMINANT LE REPERE UTILISATEUR
! --- PAR RAPPORT AU REPERE GLOBAL :
!     ============================
    call jevech('PCACOQU', 'L', jcoqu)
!
    alpha = zr(jcoqu+1) * r8dgrd()
    beta = zr(jcoqu+2) * r8dgrd()
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
                pgl(i,j) = zr(lzr+idec+(ino-1)*9+k-1)
30          continue
20      continue
!
! ---   DETERMINATION DE LA PROJECTION DU VECTEUR X DU REPERE
! ---   UTILISATEUR SUR LE FEUILLET TANGENT A LA COQUE AU NOEUD
! ---   COURANT :
!       -------
!
        call coqrep(pgl, alpha, beta, r8bid4, r8bid4,&
                    c, s)
!
!       -- (C,S) N'EST PAS TOUJOURS EXACTEMENT DE NORME=1:
        c=c/sqrt(c*c+s*s)
        s=s/sqrt(c*c+s*s)
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
                pgl(i,j) = zr(lzr+idec+(igau-1)*9+k-1)
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
!       -- (C,S) N'EST PAS TOUJOURS EXACTEMENT DE NORME=1:
        c=c/sqrt(c*c+s*s)
        s=s/sqrt(c*c+s*s)
!
        matevg(1,1,igau) = c
        matevg(2,1,igau) = s
        matevg(1,2,igau) = -s
        matevg(2,2,igau) = c
!
40  end do
!
end subroutine
