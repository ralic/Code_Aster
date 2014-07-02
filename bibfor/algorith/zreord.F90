subroutine zreord(zmat, nbddg, nbmod, nbmob, nbddr,&
                  axok, liax, nbliax, zvec)
    implicit none
#include "asterf_types.h"
!-----------------------------------------------------------------------
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
!
!  BUT:  < REMISE EN ORDRE DES COORDONNEES COMPLEXES >
!
!   CETTE ROUTINE MET EN ORDRE LES COORDONNEES COMPLEXES DES MODE
!  DANS LE CAS D'ASSEMBLAGE PARTIEL DU A  UNE INTEFACE AXE EN
!  CYCLIQUE CRAIG-BAMPTON
!-----------------------------------------------------------------------
!
! ZMAT      /M/: MATRICE DES COORDONNEES GENERALISEES DES MODES PROPRES
! NBDDG     /I/: NOMBRE DE DEGRE DE LIBETE GENERALISE TOTAL
! NBMOD     /I/: NOMBRE DE MODE CYCLIQUES CALCULES
! NBMOB     /I/: NOMBRE DE MODE PROPRES DE LA BASE MODALE
! NBDDR     /I/: NOMBRE DDL GENERALISE DE DROITE (=GAUCHE)
! AXOK      /I/: INDICATEUR ASSEMBLAGE AXE
! LIAX      /I/: LISTE NUMERO DDL AXE A ASSEMBLES
! NBLIAX    /I/: NOMBRE DE DDL AXE A ASSEMBLES
! ZVEC      /M/: VECTEUR DE TRAVAIL COMPLEXE DIMENSIONNE A NBDDG
!
!-----------------------------------------------------------------------
    integer :: i, j, nbddg, nbddr, nbliax, nbmob, nbmod
    integer :: liax(nbliax)
    complex(kind=8) :: zmat(nbddg, nbmod), zvec(nbddg)
    aster_logical :: axok
!-----------------------------------------------------------------------
!
!  CAS DE PRESENCE DDL AXE
!
!-----------------------------------------------------------------------
    if (axok) then
        do 10 j = 1, nbmod
!
            do 20 i = 1, nbliax
                zvec(i)=zmat(i+nbmob+nbddr,j)
 20         continue
!
            if ((nbmob+nbddr) .lt. nbddg) then
                do 30 i = nbmob+nbddr+1, nbddg
                    zmat(i,j)=dcmplx(0.d0,0.d0)
 30             continue
            endif
!
            do 40 i = 1, nbliax
                zmat(nbmob+nbddr+liax(i),j)=zvec(i)
 40         continue
!
 10     continue
!
!  AUTRE CAS
!
    else
        if ((nbmob+nbddr) .lt. nbddg) then
            do 50 j = 1, nbmod
                do 60 i = nbmob+nbddr+1, nbddg
                    zmat(i,j)=dcmplx(0.d0,0.d0)
 60             continue
 50         continue
        endif
!
    endif
end subroutine
