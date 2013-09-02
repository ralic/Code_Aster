subroutine cginit(nomte, iu, iuc, im)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "asterfort/assert.h"

    character(len=16) :: nomte
    integer :: iu(3, 3), iuc(3), im(3)
! ----------------------------------------------------------------------
!            DECALAGE D'INDICE POUR LES ELEMENTS GAINE/CABLE
! ----------------------------------------------------------------------
! IN  NOMTE  NOM DE L'ELEMENT FINI
! OUT IU     DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT GAINE
! OUT IUC    DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPL RELA CABLE
! OUT IM     DECALAGE D'INDICE POUR ACCEDER AUX DDL DE LAGRANGE
! ----------------------------------------------------------------------
    integer :: n
    integer :: use3(3), ucse3(3), umse3(2)
! ----------------------------------------------------------------------
    data use3   /1,2,3/
    data ucse3  /1,2,3/
    data umse3  /1,2/
! ----------------------------------------------------------------------
!
    if ((nomte.eq.'MECGSEG3')) then
        do 10 n = 1, 3
            iu(1,n) = 1 + (use3(n)-1)*5
            iu(2,n) = 2 + (use3(n)-1)*5
            iu(3,n) = 3 + (use3(n)-1)*5
10      continue
!
        do 20 n = 1, 3
            iuc(n) = 4 + (ucse3(n)-1)*5
20      continue
!
        do 30 n = 1, 2
            im(n) = umse3(n)*5
30      continue
!
    else
        ASSERT(.false.)
    endif
!
end subroutine
