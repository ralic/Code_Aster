subroutine gmatc3(nnoff, milieu, connex, &
                  abscur, matr)

implicit none

#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/gmate3.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"

    integer           :: nnoff
    character(len=24) :: abscur
    character(len=24) :: matr
    aster_logical     :: milieu, connex

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
!       CALCUL DE LA MATRICE DU SYSTEME LINEAIRE [A] {GS} = {GTHI}
!       POUR LA METHODE THETA-LAGRANGE ET G-LAGRANGE
!
! ENTREE
!
!   NNOFF    --> NOMBRE DE NOEUDS DU FOND DE FISSURE
!   ABSCUR   --> ABSCISSES CURVILIGNES S
!   MILIEU   --> .TRUE.  : ELEMENT QUADRATIQUE
!                .FALSE. : ELEMENT LINEAIRE
!   CONNEX   --> .TRUE.  : FOND DE FISSURE FERME
!                .FALSE. : FOND DE FISSURE OUVERT

! SORTIE
!
!   MATR     --> MATRICE DU SYTEME A RESOUDRE
!
! ......................................................................

    integer          :: nseg, iseg, imatr
    integer          :: i, j, ij, nno
    integer          :: conn(3), conn2(3)
    real(kind=8)     :: mele(3, 3)
    character(len=8) :: elrefe

! ......................................................................
!
    call jemarq()

    conn(1:3) = 0

!   NOMBRE DE SEGMENT DU FOND DE FISSURE
    if (milieu) then
        nseg = (nnoff-1)/2
        elrefe = 'SE3'
    else
        nseg = nnoff-1    
        elrefe = 'SE2'
    endif       

!   CREA OBJET TEMP POUR LA VAL DE GLEGEN A ABSC CURV S
    call wkvect(matr, 'V V R8', nnoff*nnoff, imatr)
!
!  BOUCLE SUR LES SEGMENTS
    do iseg = 1, nseg      

        if (milieu) then
            conn(1) = 2*iseg-1
            conn(2) = 2*iseg+1
            conn(3) = 2*iseg
        else
            conn(1) = iseg
            conn(2) = iseg+1
        endif
!
!       CALCUL DE LA MATRICE ELEMENTAIRE POUR L'ELEMENT COURANT
        call gmate3(abscur, elrefe, conn, nno, mele)

!       AJOUT DE LA CONTRIBUTION DE DE LA MATRICE DE MASSE ELEMENTAIRE
!       A LA MATRICE DE MASSE ASSEMBLEE
        do i=1, nno
            do j=1, nno
                ij = (conn(i)-1)*nnoff + conn(j)
                zr(imatr + ij - 1) = zr(imatr + ij - 1) + mele(i, j)
            end do
        end do

!       CAS CONNEX >> FISSURE FERME
        if (connex) then

!           AJOUT D'UNE CONTRIBUTION SUR LA LIGNE NDIMTE
            if (conn(1).eq.1) then
                conn2 = conn
                conn2(1) = nnoff

                i = 1 
                do j = 1, nno
                     ij = (conn2(i)-1)*nnoff + conn2(j)
                     zr(imatr + ij - 1) = zr(imatr + ij - 1) + mele(i, j)
                end do
            endif

!           AJOUT D'UNE CONTRIBUTION SUR LA LIGNE 1
            if (conn(2).eq.nnoff) then
                conn2 = conn
                conn2(2) = 1
 
                i=2 
                do j=1, nno
                     ij = (conn2(i)-1)*nnoff + conn2(j)
                     zr(imatr + ij - 1) = zr(imatr + ij - 1) + mele(i, j)
                end do
            endif
        endif

    end do

    call jedema()

end subroutine
