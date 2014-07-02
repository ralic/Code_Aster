function lexseg(connex, typmai, nbrma, n1, n2)
    implicit none
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
!-----------------------------------------------------------------------
!     FONCTION BOOLEENNE INDIQUANT L'EXISTENCE DANS LE MAILLAGE D'UNE
!     MAILLE SEGMENT DE NOEUD ORIGINE DE NUMERO N1 ET NOEUD EXTREMITE
!     DE NUMERO N2
!     APPELANT : FENEXC
!-----------------------------------------------------------------------
! IN : CONNEX : CHARACTER*24 , NOM DE L'OBJET CONNECTIVITE
! IN : TYPMAI : CHARACTER*24 , NOM DE L'OBJET CONTENANT LES TYPES
!               DES MAILLES
! IN : NBRMA  : INTEGER , NOMBRE DE MAILLES DU MAILLAGE
! IN : N1     : INTEGER , NUMERO DU NOEUD ORIGINE
! IN : N2     : INTEGER , NUMERO DU NOEUD EXTREMITE
!-----------------------------------------------------------------------
#include "asterf_types.h"
#include "asterfort/i2extf.h"
    aster_logical :: lexseg
    character(len=24) :: connex, typmai
    integer :: nbrma, n1, n2
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: mi, nuextr, nuorig
!-----------------------------------------------------------------------
    lexseg = .false.
    do 10 mi = 1, nbrma
        call i2extf(mi, 1, connex(1:15), typmai(1:16), nuorig,&
                    nuextr)
        if (nuorig .eq. n1 .and. nuextr .eq. n2) then
            lexseg = .true.
            goto 11
        endif
 10 end do
 11 continue
end function
