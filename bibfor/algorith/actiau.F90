subroutine actiau(nbcmp, nbno, nbec, mcoddl, icodac)
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
!
!***********************************************************************
!    P. RICHARD     DATE 20/02/91
!-----------------------------------------------------------------------
!  BUT:    < DDL ACTIF CAS AUCUN >
!
! DETERMINER LES ENTIER CODES DES DDL ACTIFS A PARTIR DES ENTIERS CODES
!  DES DDL PHYSIQUE ET DES LAGRANGES ASSEMBLES AUX NOEUDS
!  POUR UNE INTERFACE DE TYPE AUCUN
!
!-----------------------------------------------------------------------
!
! NBCMP    /I/: NOMBRE DE COMPOSANTES MAX DE LA GRANDEUR SOUS-JACENTE
! NBNO     /I/: NOMBRE DE NOEUDS DE LA TABLE
! MCODDL   /I/: TABLEAU DES ENTIERS CODES DDL AUX NOEUDS
! ICODAC   /O/: LISTE DES ENTIERS CODES DES DDL ACTIFS DEMANDES
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    include 'asterfort/iscode.h'
    include 'asterfort/isgeco.h'
    integer :: i, iec, nbcmp, nbcpmx, nbec, nbecmx, nbno
!
!-----------------------------------------------------------------------
    parameter (nbcpmx = 300)
    parameter (nbecmx =  10)
    integer :: mcoddl(nbno*nbec, 2), icodac(nbno*nbec)
    integer :: idec(nbcpmx), itout(nbecmx), icoco(nbecmx), icici(nbecmx)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!    INITIALISATION DU VECTEUR A CODER (SERA INUTILE QUAND LA BOUCLE
!   INTERNE 20  SERA DE 1 A 30)
!
    do 10 i = 1, nbecmx
        itout(i) = 0
        icoco(i) = 0
        icici(i) = 0
10  continue
!
    do 20 i = 1, nbcmp
        idec(i)=1
20  continue
    call iscode(idec, itout, nbcmp)
!
    if (nbno .eq. 0) goto 9999
!
    do 30 i = 1, nbno
        call isgeco(itout, mcoddl((i-1)*nbec+1, 1), nbcmp, -1, icoco)
        call isgeco(icoco, mcoddl((i-1)*nbec+1, 2), nbcmp, 1, icici)
        call isgeco(icodac((i-1)*nbec+1), icici, nbcmp, -1, icoco)
        do 40 iec = 1, nbec
            icodac((i-1)*nbec+iec)=icoco(iec)
40      continue
30  end do
!
9999  continue
end subroutine
