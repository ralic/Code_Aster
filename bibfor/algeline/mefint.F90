subroutine mefint(nbz, nbgrp, nbmod, nbnoe, nbddl,&
                  irot, numnog, nbnog, zint, defm,&
                  phix, phiy, z, num)
    implicit none
!
#include "jeveux.h"
    integer :: nbz, nbgrp, nbmod, nbnoe, nbddl
    integer :: irot(3), numnog(nbgrp), nbnog(nbgrp), num(nbz)
    real(kind=8) :: zint(nbz, nbgrp), defm(6*nbnoe, nbmod), z(*)
    real(kind=8) :: phix(nbz, nbgrp, nbmod), phiy(nbz, nbgrp, nbmod)
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     INTERPOLATION DES DEFORMEES MODALES POUR TOUS LES CYLINDRES
!     EQUIVALENTS (OU REELS, SI IL N'Y A PAS DE GROUPE D EQUIVALENCE)
!     AUX POINTS COMMUNS DE DISCRETISATION, DONT ON CREE LE TABLEAU DES
!     COTES: Z(I=1,NBZ).
!     OPERATEUR APPELANT : OP0144 , FLUST3
! ----------------------------------------------------------------------
!     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
!     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
!     DE TUBES SOUS ECOULEMENT AXIAL"
! ----------------------------------------------------------------------
! IN  : NBZ    : NOMBRE DE NOEUDS DE LA DISCRETISATION AXIALE
! IN  : NBGRP  : NOMBRE DE GROUPES D EQUIVALENCE
! IN  : NBMOD  : NOMBRE DE MODES
! IN  : NBNOE  : NOMBRE DE NOEUDS DU MAILLAGE
! IN  : NBDDL  : NOMBRE DE DEGRES DE LIBERTE
! IN  : IROT   : INDICE DE PERMUTATION CIRCULAIRE DU CHANGEMENT DE
!                REPERE
! IN  : NUMNOG : TABLEAU DES ADRESSES DES NUMEROS DES NOEUDS DES
!                CYLINDRES
! IN  : NBNOG  : TABLEAU DED NOMBRED DE NOEUDS DE CHAQUE CYLINDRE
! IN  : ZINT   : COORDONNEES 'Z' DANS LE REPERE AXIAL DES
!                NOEUDS DES CYLINDRES
! IN  : DEFM   : DEFORMEES MODALES DES NOEUDS DES CYLINDRES EQUIVALENTS
! OUT : PHIX   : TABLEAU DES DEFORMEES MODALES INTERPOLEES DANS LA
!                DIRCTION 'X' DU REPERE AXIAL
! OUT : PHIY   : TABLEAU DES DEFORMEES MODALES INTERPOLEES DANS LA
!                DIRCTION 'Y' DU REPERE AXIAL
! OUT : Z      : COORDONNEES 'Z' DANS LE REPERE AXIAL DES
!                POINTS DISCRETISES, (IDENTIQUES POUR TOUS LES CYLINDRES
! --  : NUM    : TABLEAU DE TRAVAIL, INDICES POUR LE CLASSEMENT PAR
!                ORDRE CROISSANT SUIVANT Z, DES NOEUDS DES CYLINDRES
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: i, j
! ----------------------------------------------------------------------
!
!
!
!
! --- DETERMINATION DES COTES MAXIMALE ET MINIMALE DE L ENSEMBLE DES
! --- CYLINDRES
!-----------------------------------------------------------------------
    integer :: icomp, ind, ind1, ind2, k, nm, nn
    integer :: nno1, nno2
    real(kind=8) :: wmax, wmin, z0, zmax, zmin
!-----------------------------------------------------------------------
    zmax = zint(1,1)
    zmin = zint(1,1)
    do 10 j = 2, nbnog(1)
        if (zint(j,1) .gt. zmax) zmax = zint(j,1)
        if (zint(j,1) .lt. zmin) zmin = zint(j,1)
10  end do
    do 30 i = 2, nbgrp
        wmax = zint(1,i)
        wmin = zint(1,i)
        do 20 j = 2, nbnog(i)
            if (zint(j,i) .gt. wmax) wmax = zint(j,i)
            if (zint(j,i) .lt. wmin) wmin = zint(j,i)
20      continue
        if (wmin .gt. zmin) zmin = wmin
        if (wmax .lt. zmax) zmax = wmax
30  end do
    do 40 i = 1, nbz
        z(i) = zmin + (zmax-zmin)*(i-1)/(nbz-1)
40  end do
!
!
! --- INTERPOLATION DES DEFORMEES MODALES
!
! --- DEBUT BES BOUCLES SUR LES CYLINDRES OU GROUPES DE CYLINDRES
    do 200 i = 1, nbgrp
!
! ---    CLASSEMENT POUR LE CYLINDRE I DES NOEUDS PAR ORDRE DE COTE
        icomp = 0
        do 115 j = 1, nbnog(i)
            num(j) = j
115      continue
        do 120 j = 1, nbnog(i)
            z0 = zint(num(j),i)
            ind = j
            icomp = icomp + 1
            do 100 k = icomp+1, nbnog(i)
                if (z0 .gt. zint(num(k),i)) then
                    z0 = zint(num(k),i)
                    ind = k
                endif
100          continue
            if (ind .ne. j) then
                nn = num(ind)
                do 110 k = 1, (ind-icomp)
                    num(ind-k+1) = num(ind-k)
110              continue
                num(icomp) = nn
            endif
120      continue
!
! ---    BOUCLE SUR LES POINTS DE DISCRETISATION DU CYLINDRE I
        do 190 j = 1, nbz
! ---       RECHERCHE DU NOEUDS REEL LE PLUS PROCHE DU POINT DE
! ---       DISCRETISATION DE COTE J
            do 130 k = 1, nbnog(i)
                if (zint(num(k),i) .gt. z(j)) then
                    if (k .gt. 1) then
                        ind1 = num(k-1)
                        ind2 = num(k)
                    else
                        ind1 = num(k)
                        ind2 = num(k+1)
                    endif
                    goto 140
                endif
130          continue
            ind1 = num(nbnog(i)-1)
            ind2 = num(nbnog(i))
140          continue
!
            nno1 = zi(numnog(i)+ind1-1)
            nno2 = zi(numnog(i)+ind2-1)
!
! ---       INTERPOLATION DES DEFORMEES MODALES
! ---       DEBUT BES BOUCLES SUR LES MODES
            do 150 nm = 1, nbmod
                phix(j,i,nm) = defm(&
                               nbddl*(nno1-1)+irot(1),&
                               nm) +(&
                               defm(&
                               nbddl*(nno2-1)+irot(1), nm) - defm(nbddl*(nno1- 1)+irot(1),&
                               nm) ) *( z(j)-zint(ind1, i) ) / (zint(ind2, i&
                               )-zint(ind1, i&
                               )&
                               )
                phiy(j,i,nm) = defm(&
                               nbddl*(nno1-1)+irot(2),&
                               nm) +(&
                               defm(&
                               nbddl*(nno2-1)+irot(2), nm) - defm(nbddl*(nno1- 1)+irot(2),&
                               nm) ) *( z(j)-zint(ind1, i) ) / (zint(ind2, i&
                               )-zint(ind1, i&
                               )&
                               )
!
!
! ---       FIN BES BOUCLES SUR LES MODES
150          continue
!
! ---    FIN BES BOUCLES SUR LES POINTS DE DISCRETISATION
190      continue
!
! --- FIN BES BOUCLES SUR LES CYLINDRES
200  end do
!
!
end subroutine
