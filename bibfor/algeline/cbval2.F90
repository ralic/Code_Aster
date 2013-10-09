subroutine cbval2(nbcomb, typcst, const, lmat, typres,&
                  lres, ddlexc)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/pteddl.h"
#include "asterfort/rrssm2.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbcomb, lmat(*), lres
    character(len=*) :: typres, ddlexc, typcst(*)
    real(kind=8) :: const(*)
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
!     COMBINAISON LINEAIRE DES .VALM DES MATRICES
!       *  LES MATRICES SONT SUPPOSEES AVOIR LE MEME TYPE DE STOCKAGE
!          (MORSE) MAIS ELLES ONT DES PROFILS DIFFERENTS
!       *  POUR L'INSTANT ON NE TRAITE QUE LE CAS DE MATRICES
!          SYMETRIQUES, REELLES.
!       *  LES SCALAIRES SONT PRIS REELS POUR L'INSTANT
!     -----------------------------------------------------------------
! IN  I  NBCOMB = NOMBRE DE MATRICES A COMBINER
! IN  R  CONST  = TABLEAU DE R*8    DES COEFICIENTS
! IN  I  LMAT = TABLEAU DES POINTEURS DES MATRICES
! IN  K* TYPRES = TYPE DES MATRICES   (R)
! IN  I  LRES = POINTEUR DE MATRICE RESULTAT
! IN  K* DDLEXC = NOM DES DDLS A EXCLURE (CONCRETEMENT IL S'AGIT
!                                         DES LAGRANGE)
!
!     -----------------------------------------------------------------
!
    logical :: symr, symi
!
!     -----------------------------------------------------------------
    integer :: lgbloc
    character(len=1) :: clas, typmat
    character(len=8) :: nomddl
    character(len=14) :: numr, numi
    character(len=19) :: matres, mati
    character(len=24) :: valmi, valmr
    integer :: neq, mxddl, lddl, jrefai, jrefar, jsmhcr, jsmdir
    integer :: iconst, imat, jsmhci, jsmdii, jvlmi1, jvlmr1, k
    integer :: jvlmi2, jvlmr2
    real(kind=8) :: zero
!     -----------------------------------------------------------------
    call jemarq()
    zero = 0.d0
!
    nomddl = ddlexc
    matres = zk24(zi(lres+1))
    ASSERT(typres.eq.'R' .or. typres.eq.'C')
    neq = zi(lres+2)
    valmr = matres//'.VALM'
    lgbloc = zi(lres+14)
    call jelira(matres//'.REFA', 'CLAS', cval=clas)
    call jeveuo(matres//'.REFA', 'L', jrefar)
    ASSERT(zk24(jrefar-1+9) (1:1).eq.'M')
    symr = zk24(jrefar-1+9) .eq. 'MS'
!
    mxddl = 1
    call dismoi('NOM_NUME_DDL', matres, 'MATR_ASSE', repk=numr)
    call wkvect('&&CBVAL2', 'V V I', neq*mxddl, lddl)
    call pteddl('NUME_DDL', numr, mxddl, nomddl, neq,&
                zi(lddl))
!
!
    call jeveuo(numr//'.SMOS.SMHC', 'L', jsmhcr)
    call jeveuo(numr//'.SMOS.SMDI', 'L', jsmdir)
    call jeveuo(jexnum(valmr, 1), 'E', jvlmr1)
    if (typres(1:1) .eq. 'R') then
        do k = 1, lgbloc
            zr(jvlmr1-1+k) = zero
        end do
        if (.not.symr) then
            call jeveuo(jexnum(valmr, 2), 'E', jvlmr2)
            do k = 1, lgbloc
                zr(jvlmr2-1+k) = zero
            end do
        endif
!
    else
        call utmess('F', 'ALGELINE_5')
    endif
!
!
! --- BOUCLE SUR LES MATRICES A COMBINER :
!     ----------------------------------
    iconst = 1
    do imat = 1, nbcomb
        ASSERT(typcst(imat).eq.'R')
        mati = zk24(zi(lmat(imat)+1))
        call dismoi('NOM_NUME_DDL', mati, 'MATR_ASSE', repk=numi)
        call jeveuo(numi//'.SMOS.SMHC', 'L', jsmhci)
        call jeveuo(numi//'.SMOS.SMDI', 'L', jsmdii)
        call jeveuo(mati//'.REFA', 'L', jrefai)
        valmi = mati//'.VALM'
        symi = zk24(jrefai-1+9) .eq. 'MS'
        if (.not.symi) ASSERT(.not.symr)
        call jelira(valmi, 'TYPE', cval=typmat)
        call jeveuo(jexnum(valmi, 1), 'L', jvlmi1)
        ASSERT(typmat.eq.'R')
        if (.not.symi) call jeveuo(jexnum(valmi, 2), 'L', jvlmi2)
!
        if (typres(1:1) .eq. 'R') then
            if (typmat .eq. 'R') then
                call rrssm2(neq, zi4(jsmhcr), zi4(jsmhci), zi(jsmdir), zi(jsmdii),&
                            zi(lddl), const(iconst), zr(jvlmi1), zr( jvlmr1))
                if (.not.symr) then
                    if (.not.symi) then
                        call rrssm2(neq, zi4(jsmhcr), zi4(jsmhci), zi( jsmdir), zi(jsmdii),&
                                    zi(lddl), const(iconst), zr(jvlmi2), zr(jvlmr2))
!
                    else
                        call rrssm2(neq, zi4(jsmhcr), zi4(jsmhci), zi( jsmdir), zi(jsmdii),&
                                    zi(lddl), const(iconst), zr(jvlmi1), zr(jvlmr2))
                    endif
                endif
            endif
        endif
        iconst = iconst + 1
        call jelibe(jexnum(valmi, 1))
        if (.not.symi) call jelibe(jexnum(valmi, 2))
    end do
!
    call jedetr('&&CBVAL2')
!
    call jedema()
!
end subroutine
