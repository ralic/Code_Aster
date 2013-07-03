subroutine caurtg(nomte, ncmp, sigmau, sigrtg)
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
!      CAURTG  -- PASSAGE DES CONTRAINTES DE CAUCHY SIGMAU
!                 CALCULEES DANS LE REPERE UTILISATEUR
!                 VERS LE REPERE UTILISATEUR TOURNE DE
!                 LA ROTATION FAISANT PASSER DE L'ETAT
!                 INITIAL A L'ETAT DEFORME DANS LE CAS GROT_GDEP .
!                 SIGRTG DESIGNE LES CONTRAINTES DE CAUCHY DANS
!                 CE DERNIER REPERE .
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NOMTE          IN     K16      NOM DU TYPE D'ELEMENT
!    NCMP           IN     I        NOMBRE DE COMPOSANTES DU TENSEUR
!                                   DES CONTRAINTES
!    SIGMAU(NCMP,1) IN     R        VECTEUR DES CONTRAINTES
!                                   DE CAUCHY DANS LE REPERE UTILISATEUR
!    SIGRTG(NCMP,1) VAR    R        VECTEUR DES CONTRAINTES DE CAUCHY
!                                   TOURNE DU REPERE UTILISATEUR VERS
!                                   LE REPERE TRANSFORME DU REPERE
!                                   UTILISATEUR PAR LA GRANDE ROTATION
!                                   CALCULEE POUR COMP_ELAS - GROT_GDEP
!
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/marota.h"
#include "asterfort/tecach.h"
#include "asterfort/utbtab.h"
#include "asterfort/vectan.h"
    character(len=16) :: nomte
    real(kind=8) :: sigmau(ncmp, 1), sigrtg(ncmp, 1)
    integer :: ncmp
! -----  VARIABLES LOCALES
    real(kind=8) :: vecthe(9, 3), vecta(9, 2, 3)
    real(kind=8) :: vectpt(9, 2, 3), vectn(9, 3)
    real(kind=8) :: xab(3, 3), sigmad(3, 3), sigmat(3, 3)
    real(kind=8) :: drot(3, 3), tetag(3)
!
    logical :: lgreen
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- INITIALISATIONS :
!     ---------------
!-----------------------------------------------------------------------
    integer :: i, icompo, idepl, igeom, ii, in, iret
    integer :: lzi, lzr, nb1, nb2
!-----------------------------------------------------------------------
    lgreen = .false.
!
! --- RECUPERATION DE LA CARTE DE COMPORTEMENT :
!     ----------------------------------------
    call jevech('PCOMPOR', 'L', icompo)
!
    if (zk16(icompo+2) .eq. 'GROT_GDEP') then
        lgreen = .true.
    endif
!
! --- RECUPERATION DU CHAMP DE DEPLACEMENT DANS LE CAS GROT_GDEP :
!     ---------------------------------------------------------
    if (lgreen) then
        call tecach('OON', 'PDEPLAR', 'L', 1, idepl,&
                    iret)
    else
        goto 9999
    endif
!
! --- RECUPERATION DES COORDONNEES DES NOEUDS DANS LA GEOMETRIE
! --- INITIALE :
!     --------
    call jevech('PGEOMER', 'L', igeom)
!
! --- RECUPERATION DES OBJETS INITIALISES :
!     -----------------------------------
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
!
! --- NOMBRE DE NOEUDS (NB1 : SERENDIP, NB2 : LAGRANGE) :
!     -------------------------------------------------
    nb1 = zi(lzi+1-1)
    nb2 = zi(lzi+2-1)
!
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
! --- AFFECTATION DES VECTEURS DE TRANSLATION ET DE ROTATION :
!     ------------------------------------------------------
    do 10 in = 1, nb1
        do 20 ii = 1, 3
            vecthe(in,ii) = zr(idepl+6*(in-1)+ii+3-1)
20      continue
10  end do
!
    do 30 ii = 1, 3
        vecthe(nb2,ii) = zr(idepl+6*nb1+ii-1)
30  end do
!
! --- DETERMINATION DES REPERES LOCAUX AUX NOEUDS DANS LA
! --- CONFIGURATION INITIALE
! --- VECTA DESIGNE LES VECTEURS COVARIANTS DANS LE PLAN MOYEN A
! ---       CHAQUE NOEUD
! --- VECTN DESIGNE LES VECTEURS NORMAUX AU PLAN MOYEN
! --- VECTPT DESIGNE LES REPERES LOCAUX ORTHORNORMES EN CHAQUE
! --- NOEUD DANS LA CONFIGURATION INITIALE :
!     ------------------------------------
    call vectan(nb1, nb2, zr(igeom), zr(lzr), vecta,&
                vectn, vectpt)
!
! ---   MISE DU VECTEUR DES CONTRAINTES DANS LE REPERE UTILISATEUR
! ---   SOUS LA FORME D'UN TENSEUR 3X3 :
!       ------------------------------
    do 40 i = 1, nb2
!
        tetag(1) = vecthe(i,1)
        tetag(2) = vecthe(i,2)
        tetag(3) = vecthe(i,3)
        call marota(tetag, drot)
!
        sigmat(1,1) = sigmau(1,i)
        sigmat(2,2) = sigmau(2,i)
        sigmat(3,3) = sigmau(3,i)
        sigmat(1,2) = sigmau(4,i)
        sigmat(2,1) = sigmat(1,2)
        if (ncmp .eq. 6) then
            sigmat(1,3) = sigmau(5,i)
            sigmat(2,3) = sigmau(6,i)
            sigmat(3,1) = sigmat(1,3)
            sigmat(3,2) = sigmat(2,3)
        endif
!
! ---   ROTATION DU TENSEUR DES CONTRAINTES DE CAUCHY DE LA
! ---   ROTATION FAISANT PASSER DE L'ETAT INITAL A L'ETAT DEFORME :
!       ---------------------------------------------------------
        call utbtab('ZERO', 3, 3, sigmat, drot,&
                    xab, sigmad)
!
! ---   AFFECTATION DU VECTEUR EN SORTIE DES CONTRAINTES
! ---   DE CAUCHY DANS LE REPERE UTILISATEUR TOURNE :
!       -------------------------------------------
        sigrtg(1,i) = sigmad(1,1)
        sigrtg(2,i) = sigmad(2,2)
        sigrtg(3,i) = sigmad(3,3)
        sigrtg(4,i) = sigmad(1,2)
        if (ncmp .eq. 6) then
            sigrtg(5,i) = sigmad(1,3)
            sigrtg(6,i) = sigmad(2,3)
        endif
!
40  end do
!
9999  continue
!.============================ FIN DE LA ROUTINE ======================
end subroutine
