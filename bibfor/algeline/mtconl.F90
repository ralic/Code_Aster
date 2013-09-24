subroutine mtconl(nbcomb, typcst, const, lmat, typres,&
                  lres)
    implicit none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtxcnl.h"
#include "asterfort/wkvect.h"
    integer :: nbcomb, lmat(*), lres
    character(len=*) :: typcst(*)
    character(len=*) :: typres
    real(kind=8) :: const(*)
    character(len=1) :: typrez
!     ------------------------------------------------------------------
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
!     COMBINAISON LINEAIRE DU CONDITIONNEMENT DES LAGRANGES DES MATRICES
!       *  LES MATRICES SONT SONT A COEFFICIENTS REELS OU COMPLEXES
!       *  LES SCALAIRES SONT REELS OU COMPLEXES
!     -----------------------------------------------------------------
! IN  NBCOMB : I : NOMBRE DE MATRICES A COMBINER
! IN  TYPCST : K1: TYPE DES CONSTANTES (R OU C)
! IN  CONST  : R : TABLEAU DE R*8    DES COEFICIENTS
! IN  LMAT   : I : TABLEAU DES POINTEURS DES MATRICES
! IN  TYPRES : K1: TYPE DES MATRICES   (R OU C)
! IN  LRES   : I : POINTEUR DE MATRICE RESULTAT
!     -----------------------------------------------------------------
!
!
!     ------------------------------------------------------------------
    character(len=1) :: type
    character(len=4) :: clas, cumul
    character(len=19) :: cbid(2)
    real(kind=8) :: tcst
    character(len=8) :: tpcst
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer ::  icomb, iconst, ier1, ier2, jcomb, lconl1
    integer :: lconl2, neq
!-----------------------------------------------------------------------
    call jemarq()
    typrez = typres
!
!     --- DUPLICATION EVENTUELLE DU .CONL ---
    cbid(1) = zk24(zi(lres+1))
    neq = zi(lres+2)
    jcomb = 0
    iconst = 1
    do 10 icomb = 1, nbcomb
        cbid(2) = zk24(zi(lmat(icomb)+1))
        call jeexin(cbid(2)//'.CONL', ier2)
        if (ier2 .ne. 0) then
!           --- CREATION (EVENTUELLE)  D'UN .CONL AU TYPE "TYPRES" ---
            call jeexin(cbid(1)//'.CONL', ier1)
            if (ier1 .ne. 0) then
                call jelira(cbid(1)//'.CONL', 'TYPE', cval=type)
                if (type .ne. typrez) then
                    call jedetr(cbid(1)//'.CONL')
                    ier1 = 0
                endif
            endif
            if (ier1 .eq. 0) then
                call jelira(cbid(1)//'.VALM', 'CLAS', cval=clas)
                call wkvect(cbid(1)//'.CONL', clas(1:1)//' V '//typrez, neq, lconl1)
            else
                call jeveuo(cbid(1)//'.CONL', 'E', lconl1)
            endif
!
!           --- REMPLISSAGE ---
            cumul = 'ZERO'
            do 30 jcomb = icomb, nbcomb
                cbid(2) = zk24(zi(lmat(jcomb)+1))
                call jeexin(cbid(2)//'.CONL', ier2)
                if (ier2 .ne. 0) then
!                 --- MOULINEX ----
                    call jeveuo(cbid(2)//'.CONL', 'L', lconl2)
                    call jelira(cbid(2)//'.CONL', 'TYPE', cval=type)
!
!                 SI LE COEFFICIENT EST COMPLEXE : ATTENTION !
!                 CAR LES MATRICES INITIALES ET RESULTATS .CONL SONT
!                 FORCEMENT REELLES DU COUP ON DOIT FOURNIR UN
!                 CONST REEL !
                    if (typcst(jcomb) .eq. 'C') then
                        tcst = abs(dcmplx(const(iconst),const(iconst+ 1)))
                        tpcst = 'R'
                        call mtxcnl(cumul, tpcst, [tcst,0.d0], type, lconl2,&
                                    typrez, lconl1, neq)
                    else
                        call mtxcnl(cumul, typcst(jcomb), const(iconst), type, lconl2,&
                                    typrez, lconl1, neq)
                    endif
                    cumul = 'CUMU'
                endif
                iconst = iconst + 1
                if (typcst(jcomb) .eq. 'C') iconst = iconst + 1
30          continue
            goto 20
        else
            iconst = iconst + 1
            if (typcst(icomb) .eq. 'C') iconst = iconst + 1
        endif
10  end do
!
!
20  continue
    if (jcomb .eq. 0) then
!        --- PAS DE .CONL ---
        call jeexin(cbid(1)//'.CONL', ier1)
        if (ier1 .ne. 0) call jedetr(cbid(1)//'.CONL')
    endif
    call jedema()
end subroutine
