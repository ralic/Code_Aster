subroutine gmeth3(nnoff, gthi, milieu, gs,&
                  objcur, gi, num, connex)

implicit none

#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/getvtx.h"
#include "asterfort/gmatc3.h"
#include "asterfort/gmatl3.h"
#include "asterfort/gsyste.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"

    integer           :: nnoff, num
    real(kind=8)      :: gthi(1), gs(1), gi(1)
    character(len=24) :: objcur
    aster_logical     :: milieu, connex

! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!      METHODE THETA-LAGRANGE ET G-LAGRANGE POUR LE CALCUL DE G(S)
!
! ENTREE
!
!     NNOFF    --> NOMBRE DE NOEUDS DU FOND DE FISSURE
!     GTHI     --> VALEURS DE G POUR LES CHAMPS THETAI
!     MILIEU   --> .TRUE.  : ELEMENT QUADRATIQUE
!                  .FALSE. : ELEMENT LINEAIRE
!     OBJCUR   --> ABSCISSES CURVILIGNES S
!     CONNEX   --> .TRUE.  : FOND DE FISSURE FERME
!                  .FALSE. : FOND DE FISSURE OUVERT
!  SORTIE
!
!      GS      --> VALEUR DE G(S)
!      GI      --> VALEUR DE GI
!      NUM     --> 3 (LAGRANGE-LAGRANGE)
!              --> 4 (NOEUD-NOEUD)

!......................................................................
    integer           :: i, iabsc
    integer           :: ivect, ibid
    character(len=24) :: vect, matr, lissg
!......................................................................
    call jemarq()
!
!   ABSCISSES CURVILIGNES DES NOEUDS DU FOND DE FISSURE
    call jeveuo(objcur, 'L', iabsc)

!   CHOIX DU LISSAGE
    call getvtx('LISSAGE', 'LISSAGE_G', iocc=1, scal=lissg, nbret=ibid)
!
    if (lissg .eq. 'LAGRANGE_NO_NO') then
        num = 4

!       CALCUL DE LA MATRICE DU SYSTEME LINÉAIRE : MATRICE LUMPEE
        vect = '&&METHO3.VECT'
        call gmatl3(nnoff, milieu, connex, &
                    objcur, vect)
!
!       RESOLUTION DU SYSTEME : MATRICE DIAGONALE
        call jeveuo(vect, 'L', ivect)
        do i = 1, nnoff
            gi(i) = gthi(i)/zr(ivect+i-1 )
        end do

    else if ( lissg.eq.'LAGRANGE' ) then
        num = 3

!       CALCUL DE LA MATRICE DU SYSTEME LINÉAIRE
        matr = '&&METHO3.MATRI'
        call  gmatc3(nnoff, milieu, connex, &
                     objcur, matr)

!       SYSTEME LINEAIRE:  MATR*GI = GTHI
        call gsyste(matr, nnoff, nnoff, gthi, gi)

    endif
!
    do i = 1, nnoff
        gs(i) = gi(i)
    end do

    call jedetr('&&METHO3.MATRI')
    call jedetr('&&METHO3.VECT')
!
    call jedema()
end subroutine
