subroutine maskau(nbno, nbec, imask)
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
!***********************************************************************
!    P. RICHARD     DATE 20/02/91
!-----------------------------------------------------------------------
!  BUT:       CAS AUCUN
    implicit none
!
!    CONTROLER LES DEFORMEES A CALCULER EN TENANT COMPTE DE
!   LA TABLE DES ENTIERS CODES DES DDL AU NOEUD ET DE LA LISTE
!    DES ENTIER CODES DES MASQUES AUX NOEUDS
!
!   TABLE DES ENTIER CODE:  COLONNE 1 DDL PHYSIQUES
!                           COLONNE 2 DDL LAGRANGES
!
!  LE RESULTAT EST POUR CHAQUE NOEUD UN ENTIER CODES DONNANT LA LISTE
!    DES TYPES DE DDL POUR LESQUELS UNE DEFORMEE DOIT ETRE CALCULEE
!
!-----------------------------------------------------------------------
!
! NBNO     /I/: NOMBRE DE NOEUDS DE LA TABLE
! IMASK    /M/: LISTE DES ENTIERS CODES MASQUES EN ENTREE
!
!-----------------------------------------------------------------------
!
    integer :: imask(nbno*nbec)
    integer :: i, nbec, nbno
!-----------------------------------------------------------------------
!
    if (nbno .eq. 0) goto 9999
!
    do 10 i = 1, nbno*nbec
        imask(i)=0
10  end do
!
9999  continue
end subroutine
