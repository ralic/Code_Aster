subroutine elg_setint(inda, nba, indb, nbb, indc,&
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
# include "asterfort/assert.h"
!
#ifdef _HAVE_PETSC
#include "elim_lagr.h"
    integer, intent(in) :: nba
    integer, intent(in) :: nbb
    integer, intent(out) :: nbc
!
    PetscInt :: inda(nba), indb(nbb), indc(min(nba, nbb))
    character(len=6) :: remp
!
! person_in_charge: mathieu.corus at edf.fr
!
!-- INTERSECTION DE DEUX VECTEURS D'INDICES
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
!-- SORTIE : INDC : VECTEURS DES INDICES IDENTIQUES, DE LONGUEUR NBC
!--          INDA ET INDB LES POSITIONS DES ELEMENTS COMMUNS
!
!--
!-- RECOPIE DE LA ROUTINE C++ :
!--
!#set_intersection (InputIterator1 first1, InputIterator1 last1,
!#                                   InputIterator2 first2, InputIterato
!#                                   OutputIterator result)
!#{
!#  while (first1!=last1 && first2!=last2)
!#  {
!#    if (*first1<*first2) ++first1;
!#    else if (*first2<*first1) ++first2;
!#    else {
!#      *result = *first1; first2;
!#      ++result; ++first1; ++first2;
!#    }
!#  }
!#  return result;
!#}
!
    integer :: ia, ib, ic
!
    nbc=0
!
    if (nba .eq. 0 .or. nbb .eq. 0) goto 999
!
!
!     WRITE(6,*),'--------',REMP
!     WRITE(6,*),'NBA=',NBA
!     WRITE(6,*),'INDA :',INDA(1:NBA)
!     WRITE(6,*),' '
!     WRITE(6,*),'NBB=',NBB
!     WRITE(6,*),'INDB :',INDB(1:NBB)
!
    if (remp .eq. 'COMPTE') then
!
        if (inda(nba) .lt. indb(1)) then
            nbc=0
        else if (indb(nbb) .lt. inda(1)) then
            nbc=0
        else
            ia=1
            ib=1
 10         continue
!
            if (inda(ia) .eq. indb(ib)) then
                nbc=nbc+1
                ia=ia+1
                ib=ib+1
!
            else if (inda(ia) .lt. indb(ib)) then
                ia=ia+1
!
            else if (inda(ia) .gt. indb(ib)) then
                ib=ib+1
            endif
!
            if ((ia .le. nba) .and. (ib .le. nbb)) then
                goto 10
            endif
!
        endif
!
    else if (remp .eq. 'REMPLI') then
!
        if (inda(nba) .lt. indb(1)) then
            nbc=0
        else if (indb(nbb) .lt. inda(1)) then
            nbc=0
        else
            ia=1
            ib=1
            ic=1
!
 20         continue
!
!
            if (inda(ia) .eq. indb(ib)) then
                indc(ic)=inda(ia)
                inda(ic)=ia
                indb(ic)=ib
                nbc=nbc+1
                ia=ia+1
                ib=ib+1
                ic=ic+1
!
!
            else if (inda(ia) .lt. indb(ib)) then
                ia=ia+1
!
            else if (inda(ia) .gt. indb(ib)) then
                ib=ib+1
            endif
!
            if ((ia .le. nba) .and. (ib .le. nbb)) then
                goto 20
            endif
!
        endif
!
    else
!       WRITE(6,*),'OUT SETDIF'
        ASSERT(.false.)
    endif
!
!     WRITE(6,*),' '
!     WRITE(6,*),'NBC=',NBC
!     WRITE(6,*),'INDC :',INDC(1:NBC)
!     WRITE(6,*),' '
!     WRITE(6,*),' ** FIN MODIF ** '
!
999 continue
!
#else
    integer :: nba, nbb, nbc
    integer(kind=4) :: inda(nba), indb(nbb), indc(min(nba, nbb))
    character(len=6) :: remp
    ASSERT(.false.)
#endif
end subroutine
