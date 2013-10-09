subroutine diagav(noma19, neq, ilfin, typvar, eps)
    implicit none
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
!     BUT : AJOUTER L'OBJET .DIGS A UNE MATR_ASSE
!           ET CALCULER UN EPSILON NUMERIQUE POUR LA FACTORISATION
!     IN  : NOMA19 : MATR_ASSE QUE L'ON COMPLETERA PAR L'OBJET .DIGS
!     IN  : NEQ    : NOMBRE D'EQUATIONS
!     IN  : ILFIN  : NUMERO DE LA LIGNE DE FIN DE FACTORISITION
!     IN  : TYPVAR : REEL/COMPLEXE
!     OUT : EPS    : 'EPSILON' TEL QU'UN TERME DIAGONAL APRES
!                    FACTORISATION SERA CONSIDERE COMME NUL
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=19) :: noma19
    character(len=14) :: nu
    character(len=1) :: base
    character(len=4) :: kmpic
    real(kind=8) :: eps, diamax, diamin, vabs
    integer :: neq, ilfin, typvar, ifm, niv, iret, iadigs, jrefa
    integer :: jsxdi, jscbl, jscib, nbbloc, ibloc, iavale, idern, iprem, i
!     ------------------------------------------------------------------
!
!
    call jemarq()
    call infdbg('FACTOR', ifm, niv)
!
    call dismoi('MPI_COMPLET', noma19, 'MATR_ASSE', repk=kmpic)
    if (kmpic .ne. 'OUI') then
        call utmess('F', 'CALCULEL6_54')
    endif
    call jeveuo(noma19//'.REFA', 'L', jrefa)
    call jelira(noma19//'.REFA', 'CLAS', cval=base)
    ASSERT(zk24(jrefa-1+3).ne.'ELIML')
!
!
!     -- ALLOCATION ET CALCUL DE L'OBJET .DIGS :
!        CET OBJET CONTIENDRA LES TERMES DIAGONAUX
!        AVANT ET APRES FACTORISATION :
!        (1->NEQ : AVANT , NEQ+1 ->2*NEQ : APRES )
!     -----------------------------------------
    call jedetr(noma19//'.DIGS')
    if (typvar .eq. 1) then
        call wkvect(noma19//'.DIGS', base//' V R', 2*neq, iadigs)
    else
        call wkvect(noma19//'.DIGS', base//' V C', 2*neq, iadigs)
    endif
    call dismoi('NOM_NUME_DDL', noma19, 'MATR_ASSE', repk=nu)
!
!
!     CAS STOCKAGE MORSE DISPONIBLE (OBJET .VALM):
!     ---------------------------------------------
    call jeexin(noma19//'.VALM', iret)
    if (iret .gt. 0) then
        call jeveuo(nu//'.SMOS.SMDI', 'L', jsxdi)
        call jeveuo(jexnum(noma19//'.VALM', 1), 'L', iavale)
        if (typvar .eq. 1) then
            do 40 i = 1, neq
                zr(iadigs-1+i) = zr(iavale-1+zi(jsxdi+i-1))
 40         continue
        else if (typvar.eq.2) then
            do 50 i = 1, neq
                zc(iadigs-1+i) = zc(iavale-1+zi(jsxdi+i-1))
 50         continue
        else
            ASSERT(.false.)
        endif
        goto 9998
    endif
!
!
!     CAS STOCKAGE MORSE INDISPONIBLE (OBJET .VALM):
!     ---------------------------------------------
    ASSERT((noma19.eq.'&&OP0070.RESOC.MATC') .or. (noma19.eq.'&&OP0070.RESUC.MATC'))
    call jeveuo(nu//'.SLCS.SCDI', 'L', jsxdi)
    call jeveuo(nu//'.SLCS.SCBL', 'L', jscbl)
    call jeveuo(nu//'.SLCS.SCIB', 'L', jscib)
    nbbloc = zi(jscib-1+ilfin)
    do 30 ibloc = 1, nbbloc
        call jeveuo(jexnum(noma19//'.UALF', ibloc), 'L', iavale)
        idern = zi(jscbl-1+ibloc+1)
        ASSERT(idern.le.neq)
        iprem = zi(jscbl-1+ibloc) + 1
        if (typvar .eq. 1) then
            do 10 i = iprem, idern
                zr(iadigs-1+i) = zr(iavale-1+zi(jsxdi+i-1))
 10         continue
        else if (typvar.eq.2) then
            do 20 i = iprem, idern
                zc(iadigs-1+i) = zc(iavale-1+zi(jsxdi+i-1))
 20         continue
        else
            ASSERT(.false.)
        endif
        call jelibe(jexnum(noma19//'.UALF', ibloc))
 30 end do
!
!
!
9998 continue
!     -- CALCUL DE EPS :
!     ------------------
!     ON AVAIT PENSE CALCULER EPS COMME:
!     1.D-15 FOIS LE TERME DIAGONAL MIN (/=0)
!     MAIS IL ARRIVE QU'AVEC MULT_FRONT ON PASSE EN
!     DESSOUS SANS QUE CELA FASSE D'OVERFLOW
!     DONC ON PREND UNE VALEUR ARBITRAIRE :
    eps = 1.d0/r8gaem()
!
    if (niv .gt. 1) then
        diamax = 0.d0
        diamin = r8maem()
        do 70 i = 1, neq
            if (typvar .eq. 1) then
                vabs=abs(zr(iadigs-1+i))
            else
                vabs=abs(zc(iadigs-1+i))
            endif
            diamax = max(diamax,vabs)
            if (vabs .ne. 0.d0) diamin = min(diamin,vabs)
!
!
 70     continue
        write (ifm,*) '<FACTOR> AVANT FACTORISATION :'
        write (ifm,*) '<FACTOR>   NB EQUATIONS : ',neq
        write (ifm,*) '<FACTOR>   TERME DIAGONAL MAXIMUM :  ',diamax
        write (ifm,*) '<FACTOR>   TERME DIAGONAL (NON NUL) MINIMUM : ',&
     &                 diamin
        write (ifm,*) '<FACTOR>   EPSILON CHOISI  : ',eps
    endif
!
    call jedema()
end subroutine
