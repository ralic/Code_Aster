subroutine detefa(nnose, pi1, pi2, it, typma,&
                  ainter, cnset, n)
    implicit none
!
#    include "jeveux.h"
#    include "asterfort/assert.h"
#    include "asterfort/conare.h"
#    include "asterfort/jedema.h"
#    include "asterfort/jemarq.h"
#    include "asterfort/xxmmvd.h"
    integer :: nnose, it, pi1, pi2, cnset(*), n(3)
    real(kind=8) :: ainter(*)
    character(len=8) :: typma
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
! person_in_charge: samuel.geniaut at edf.fr
!
!                      DETERMINER LE FACE DANS L'ELEMENT DE REFERENCE
!
!     ENTREE
!       NNOSE   : NOMBRE DE NOEUDS DU SOUS TETRA
!       PI1     : NUMERO DU PREMIER POINT D'INTERSECTION
!       PI2     : NUMERO DU DEUXIEME POINT D'INTERSECTION
!       CNSET   : CONNECTIVITE DES NOEUDS DE L'ELT PARENT
!       AINTER  : INFOS ARETE ASSOCIEE AU POINT D'INTERSECTION
!       IT      : INDICE DE SOUS-TETRA TRAITE EN COURS
!       TYPMA   : TYPE DE LA MAILLE (TYPE_MAILLE)
!
!     SORTIE
!       N       : LES INDICES DES NOEUX D'UNE FACE DANS L'ELEMENT PARENT
!-----------------------------------------------------------------------
!
    integer :: ar(12, 3), nbar, a1, a2, n1, n2, n3
    integer :: i, j, zxain
    logical :: found
!-----------------------------------------------------------------------
    call jemarq()
    zxain=xxmmvd('ZXAIN')
    call conare(typma, ar, nbar)
    a1=nint(ainter(zxain*(pi1-1)+1))
    a2=nint(ainter(zxain*(pi2-1)+1))
!
!     CAS OU LA FISSURE COINCIDE AVEC UNE ARETE NON TRAITE ICI
    ASSERT((a1.ne.0).or.(a2.ne.0))
!
    do 1 i=1,3
       n(i)=0
1   continue     
    found=.false. 
!
!     CAS UN DES DEUX POINTS D'INTERSECTION EST CONFONDU AVEC UN NOEUD
!     SOMMET, CALCULE LES 3 INDICES DANS L'ELEMENT ENFANT
    if ((a1.eq.0) .and. (a2.ne.0)) then
        n1=ar(a2,1)
        n(1)=cnset(nnose*(it-1)+n1)
        n2=ar(a2,2)
        n(2)=cnset(nnose*(it-1)+n2)
        n(3)=nint(ainter(zxain*(pi1-1)+2))
!
    else if ((a1.ne.0).and.(a2.eq.0)) then
        n1=ar(a1,1)
        n(1)=cnset(nnose*(it-1)+n1)
        n2=ar(a1,2)
        n(2)=cnset(nnose*(it-1)+n2)
        n(3)=nint(ainter(zxain*(pi2-1)+2))
!
!     CAS LES DEUX POINTS D'INTERSECTIONS NE SONT PAS CONFONDU AVEC
!     LES NOEUDS SOMMETS, CALCULE LES 3 INDICES DANS L'ELEMENT ENFANT
    else if ((a1.ne.0).and.(a2.ne.0)) then
        do 30 i = 1, 2
            do 40 j = 1, 2
                if (ar(a1,i) .eq. ar(a2,j)) then
!                if (cnset(nnose*(it-1)+ar(a1,i)) .eq. cnset(nnose*(it-1)+ar(a2,j))) then
                    found=.true.
                    n3=ar(a1,i)
                    n1=ar(a1,3-i)
                    n2=ar(a2,3-j)
                endif
40          continue
30      continue
        ASSERT(found)
        n(1)=cnset(nnose*(it-1)+n1)
        n(2)=cnset(nnose*(it-1)+n2)
        n(3)=cnset(nnose*(it-1)+n3)
    endif
!
    call jedema()
end subroutine
