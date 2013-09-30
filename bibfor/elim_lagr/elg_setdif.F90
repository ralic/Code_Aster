subroutine elg_setdif(inda, nba, indb, nbb, indc,&
                  nbc, remp)
    implicit none
! aslint: disable=W0104
! person_in_charge: mathieu.corus at edf.fr
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
#   include "asterfort/assert.h"
#ifdef _HAVE_PETSC
#include "elim_lagr.h"
    integer :: nba, nbb, nbc
    PetscInt :: inda(nba), indb(nbb), indc(nba)
    character(len=6) :: remp
!
!-- DIFFERENCE DE DEUX VECTEURS D'INDICES - FORMELLEMENT INDC = INDA - I
!--
!-- NE FONCTIONNE QU'A CONDITION QUE LES VECTEURS SOIENT TRIES,
!--   ET NE POSSEDENT PAS DE DOUBLON
!--
!-- PREVU POUR LES VECTEURS D'INDICES
!--
!--
!-- ENTREE : INDA ET INDB LES VECTEURS D'INDICES, DE LONGUEUR NBA ET NBB
!--          REMP : INDIQUE SI ON NE FAIT QUE LE COMPTAGE, OU EGALEMENT
!--                 LE REMPLISSAGE DU VECTEUR INTERSECTION
!-- SORTIE : INDC : VECTEURS DES INDICES PRESENTS DANS INDA,
!--                 MAIS PAS INDB, DE LONGUEUR NBC
!--          INDA : POSITION DES INDICES NON PRESENTS DANS
!
!--
!-- RECOPIE DE LA ROUTINE C++ :
!--
!# Difference :
!#
!# set_difference (InputIterator1 first1, InputIterator1 last1,
!#                 InputIterator2 first2, InputIterator2 last2,
!#                 OutputIterator result)
!# {
!#   while (first1!=last1 && first2!=last2)
!#   {
!#     if (*first1<*first2) { *result = *first1; ++result; ++first1; }
!#     else if (*first2<*first1) ++first2;
!#     else { ++first1; ++first2; }
!#   }
!#   return std::copy(first1,last1,result);
!# }
!
    integer :: ia, ib, ic
!
    if (nbb .eq. 0) then
        nbc=nba
        indc(1:nbc)=inda(1:nba)
        goto 999
    endif
!
    if (nba .eq. 0) then
        nbc=0
        goto 999
    endif
!
    ia=1
    ib=1
    ic=1
!
!C      WRITE(6,*),' '
!C      WRITE(6,*),'NBA=',NBA
!C      WRITE(6,*),'INDA :',INDA(1:NBA)
!C      WRITE(6,*),' '
!C      WRITE(6,*),'NBB=',NBB
!C      WRITE(6,*),'INDB :',INDB(1:NBB)
!
    nbc=0
    if (remp .eq. 'COMPTE') then
!
10      continue
        if (inda(ia) .lt. indb(ib)) then
            ic=ic+1
            nbc=nbc+1
            ia=ia+1
        else if (inda(ia) .gt. indb(ib)) then
            ib=ib+1
        else
            ia=ia+1
            ib=ib+1
        endif
!
        if ((ia .le. nba) .and. (ib .le. nbb)) then
            goto 10
        endif
!
        if (ia .le. nba) nbc=nbc+nba-ia+1
!
!
    else if (remp .eq. 'REMPLI') then
!
20      continue
        if (inda(ia) .lt. indb(ib)) then
            indc(ic)=inda(ia)
            inda(ic)=ia
            ic=ic+1
            nbc=nbc+1
            ia=ia+1
        else if (inda(ia) .gt. indb(ib)) then
            ib=ib+1
        else
            ia=ia+1
            ib=ib+1
        endif
!
        if ((ia .le. nba) .and. (ib .le. nbb)) then
            goto 20
        endif
!
        if (ia .le. nba) then
30          continue
            indc(ic)=inda(ia)
            inda(ic)=ia
            ia=ia+1
            ic=ic+1
            nbc=nbc+1
            if (ia .le. nba) goto 30
        endif
!
    else
        write(6,*),'OUT elg_setdif'
        ASSERT(.false.)
    endif
!
!
!C      WRITE(6,*),' '
!C      WRITE(6,*),'NBC=',NBC
!C      WRITE(6,*),'INDC :',INDC(1:NBC)
!C      WRITE(6,*),' '
!C      WRITE(6,*),' ** Modif ** '
!C      WRITE(6,*),' '
!C      WRITE(6,*),'NBA=',NBA
!C      WRITE(6,*),'INDA :',INDA(1:NBC)
!C      WRITE(6,*),' ** Fin modif ** '
!
999  continue

#else
    integer :: nba, nbb, nbc
    integer(kind=4) :: inda(nba), indb(nbb), indc(min(nba, nbb))
    character(len=6) :: remp
    ASSERT(.false.)
#endif
end subroutine
