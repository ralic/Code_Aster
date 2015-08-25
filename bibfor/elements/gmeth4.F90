subroutine gmeth4(nnoff, ndimte, gthi, milieu, &
                  pair, gs, objcur, gi, connex)

implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/gmatr4.h"
#include "asterfort/gsyste.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"

    integer           :: nnoff, ndimte
    real(kind=8)      :: gthi(1), gs(1), gi(1)   
    character(len=24) :: objcur  
    aster_logical     :: milieu, pair, connex

! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!      METHODE LAGRANGE_REGU POUR LE CALCUL DE G(S)
!
! ENTREE
!
!   NNOFF    --> NOMBRE DE NOEUDS DU FOND DE FISSURE
!   NDIMTE   --> NOMBRE de CHAMPS THETA CHOISIS
!   GTHI     --> VALEURS DE G POUR LES CHAMPS THETAI
!   OBJCUR   --> ABSCISSES CURVILIGNES S
!   MILIEU   --> .TRUE.  : ELEMENT QUADRATIQUE
!                .FALSE. : ELEMENT LINEAIRE
!   CONNEX   --> .TRUE.  : SI FOND FERME
!                .FALSE. : SI FOND OUVERT
!
! SORTIE
!
!   GS      --> VALEUR DE G(S)
!   GI      --> VALEUR DE GI
! ......................................................................

    integer            :: i, iabsc, nn
    real(kind=8)       :: s1, s2, s3
    character(len=24)  :: matr

! ......................................................................
!
    call jemarq()
!
!   ABSCISSES CURVILIGNES DES NOEUDS DU FOND DE FISSURE
    call jeveuo(objcur, 'L', iabsc)
!
!   CALCUL DE LA MATRICE DU SYSTEME LINÉAIRE
    matr = '&&METHO4.MATRI'
    call gmatr4(nnoff, ndimte, milieu, connex, &
                objcur, matr, pair) 

!   RESOLUTION DU SYSTEME LINEAIRE:  MATR*GI = GTHI
    call gsyste(matr, ndimte, ndimte, gthi, gi)

    if (nnoff .eq. 2) then
        gs(1) = gi(1)
        gs(nnoff) = gi(ndimte)
    else
        do i = 1, ndimte-1
            if (milieu) then
                nn = 4*(i-1) + 1
                gs(nn) = gi(i)
                s1 = zr(iabsc-1+nn)
                s3 = zr(iabsc-1+nn+4)
                gs(nn+1) = gi(i)+(zr(iabsc-1+nn+1)-s1)*(gi(i+1)-gi(i))/(s3-s1)
                gs(nn+2) = gi(i)+(zr(iabsc-1+nn+2)-s1)*(gi(i+1)-gi(i))/(s3-s1)
                gs(nn+3) = gi(i)+(zr(iabsc-1+nn+3)-s1)*(gi(i+1)-gi(i))/(s3-s1)
            else
                nn = 2*(i-1) + 1
                gs(nn) = gi(i)
                s1 = zr(iabsc-1+nn)
                s2 = zr(iabsc-1+nn+1)
                s3 = zr(iabsc-1+nn+2)
                gs(nn+1) = gi(i)+(s2-s1)*(gi(i+1)-gi(i))/(s3-s1)
            endif
        end do
        gs(nnoff) = gi(ndimte)

!       SI PAIR, ON CORRIGE LA VALEUR DE G AU DERNIER NOEUD
        if (pair) then
            nn=2*(ndimte-2) 
            s1 = zr(iabsc-1+nn)
            s2 = zr(iabsc-1+nn+1)
            s3 = zr(iabsc-1+nn+2)
            gs(nnoff) = gs(nnoff-1)+ (s3-s2)*(gs(nnoff-2)-gs(nnoff-1)) /(s1-s2)
        endif

    endif

    call jedetr(matr)

    call jedema()
end subroutine
