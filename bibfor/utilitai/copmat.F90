subroutine copmat(matr, numddl, mat)
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
! 15/03/91    G.JACQUART AMV/P61 47 65 49 41
!***********************************************************************
!
!     FONCTION : COPIE MATR_ASSE DANS MATRICE PLEINE
!
!-----------------------------------------------------------------------
!    MATR   /I/ : NOM DE LA MATRICE
!    NUMDDL /I/ : NUMEROTATION SI MATRICE STOCKEE LIGNE DE CIEL
!    MAT   /O/ : VECTEUR CONTENANT LA MATONALE DE MATR
!-----------------------------------------------------------------------
!
!
!
!
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: kbid, matr
    character(len=14) :: numddl
    real(kind=8) :: mat(*), pij
    logical(kind=1) :: lsym
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ib,   j, jblo2, jbloc
    integer ::    n1bloc, n2bloc, nbbloc, neq
!
    real(kind=8) :: pji
    integer, pointer :: schc(:) => null()
    integer, pointer :: scde(:) => null()
    character(len=24), pointer :: refa(:) => null()
    integer, pointer :: scbl(:) => null()
    integer, pointer :: scdi(:) => null()
!-----------------------------------------------------------------------
    data kbid /'        '/
!-----------------------------------------------------------------------
!
    call jemarq()
    if (numddl(1:8) .eq. kbid) then
!
        call utmess('F', 'UTILITAI_43', sk=matr)
!
    else
        call jeveuo(numddl(1:8)//'      .SLCS.SCDE', 'L', vi=scde)
        neq = scde(1)
        nbbloc = scde(3)
        call jelibe(numddl(1:8)//'      .SLCS.SCDE')
!
!
        call jeveuo(numddl(1:8)//'      .SLCS.SCBL', 'L', vi=scbl)
        call jeveuo(numddl(1:8)//'      .SLCS.SCDI', 'L', vi=scdi)
        call jeveuo(numddl(1:8)//'      .SLCS.SCHC', 'L', vi=schc)
        call jeveuo(matr//'           .REFA', 'L', vk24=refa)
        lsym=refa(9) .eq. 'MS'
        if (lsym) then
            do 20 ib = 1, nbbloc
                call jeveuo(jexnum(matr//'           .VALM', ib), 'L', jbloc)
                n1bloc=scbl(ib)+1
                n2bloc=scbl(ib+1)
!
!           BOUCLE SUR LES COLONNES DU BLOC
!
                do 30 j = n1bloc, n2bloc
!
!           BOUCLE SUR LES LIGNES DANS LA COLONNE
!
                    do 30 i = (j-schc(j)+1), j
                        pij = zr(jbloc+scdi(j)+i-j-1)
                        mat(i+ (j-1)*neq) = pij
                        mat(j+ (i-1)*neq) = pij
30                  continue
                call jelibe(jexnum(matr//'           .VALM', ib))
20          continue
        else
            ASSERT(nbbloc.eq.1)
!          TRIANGULAIRE SUPERIEURE
            call jeveuo(jexnum(matr//'           .VALM', 1), 'L', jbloc)
!          TRIANGULAIRE INFERIEURE
            call jeveuo(jexnum(matr//'           .VALM', 2), 'L', jblo2)
!            N1BLOC=ZI(JSCBL+IB-1)+1
!            N2BLOC=ZI(JSCBL+IB)
!
!           BOUCLE SUR LES COLONNES DU BLOC
!
            do 50 j = 1, neq
!
!           BOUCLE SUR LES LIGNES DANS LA COLONNE
!
                do 50 i = (j-schc(j)+1), j
                    pij = zr(jbloc+scdi(j)+i-j-1)
                    pji = zr(jblo2+scdi(j)+i-j-1)
                    mat(i+ (j-1)*neq) = pij
                    mat(j+ (i-1)*neq) = pji
50              continue
            call jelibe(jexnum(matr//'           .VALM', 1))
            call jelibe(jexnum(matr//'           .VALM', 2))
        endif
        call jelibe(numddl(1:8)//'      .SLCS.SCBL')
        call jelibe(numddl(1:8)//'      .SLCS.SCDI')
!
    endif
!
    call jedema()
end subroutine
