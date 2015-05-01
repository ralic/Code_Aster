subroutine maskcb(nbcmp, nbno, nbec, mcoddl, imask,&
                  numord, nbdef)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!***********************************************************************
!    P. RICHARD     DATE 20/02/91
!-----------------------------------------------------------------------
!  BUT:     CAS CRAIG BAMPTON ET CRAIG-BAMPTON HARMONIQUE
    implicit none
!
!     CONTROLER LES DEFORMEES A CALCULER EN TENANT COMPTE DE
!   LA TABLE DES ENTIERS CODES DES DDL AU NOEUD ET DE LA LISTE
!    DES ENTIER CODES DES MASQUES AUX NOEUDS
!
!   TABLE DES ENTIER CODE:  COLONNE 1 DDL PHYSIQUES
!                           COLONNE 2 DDL LAGRANGES
!
!  LE RESULTAT EST POUR CHAQUE NOEUD UN ENTIER CODES DONNANT LA LISTE
!    DES TYPES DE DDL POUR LESQUELS UNE DEFORMEE DOIT ETRE CALCULEE
!
!  REMARQUE: ON SUPPOSE QUE SI LE DDL DE LAGRANGE EXISTE, LE DDL
!           PHYSIQUE CORRESPONDANT EXISTE
!
!-----------------------------------------------------------------------
!
! NBCMP    /I/: NOMBRE DE COMPOSANTES DE LA GRANDEUR SOUS-JACENTE
! NBNO     /I/: NOMBRE DE NOEUDS DE LA TABLE
! MCODDL   /I/: TABLEAU DES ENTIER CODES
! IMASK    /M/: LISTE DES ENTIERS CODES MASQUES EN ENTREE
! NUMORD   /O/: NUMERO ORDRE PREMIERE DEFORME DE CHAQUE NOEUD
! NBDEF    /M/: NUMERO ORDRE DE LA DERNIERE DEFORMEE CALCULEE
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
#include "asterfort/isdeco.h"
#include "asterfort/isgeco.h"
    integer :: i, iec, iexcmp, j, nbcmp, nbcpmx, nbdef
    integer :: nbec, nbecmx, nbno
!-----------------------------------------------------------------------
    parameter (nbcpmx = 300)
    parameter (nbecmx =  10)
    integer :: mcoddl(nbno*nbec, 2), imask(nbno*nbec)
    integer :: idec(nbcpmx), numord(nbno), icoco(nbecmx)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
    if (nbno .eq. 0) goto 9999
!
    do 10 i = 1, nbecmx
        icoco(i) = 0
10  continue
!
    do 20 i = 1, nbno
        call isgeco(mcoddl((i-1)*nbec+1, 2), imask((i-1)*nbec+1), nbcmp, -1, icoco)
        iexcmp = 0
        do 30 iec = 1, nbec
            imask((i-1)*nbec+iec) = icoco(iec)
            if (icoco(iec) .gt. 1) then
                iexcmp = 1
            endif
30      continue
        if (iexcmp .eq. 1) then
            numord(i)=nbdef+1
            call isdeco(icoco, idec, nbcmp)
            do 40 j = 1, nbcmp
                nbdef=nbdef+idec(j)
40          continue
        endif
20  end do
!
9999  continue
end subroutine
