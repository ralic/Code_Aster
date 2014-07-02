subroutine cbvale(nbcomb, typcst, const, lmat, typres,&
                  lres, ddlexc, matd)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/cbvalc.h"
#include "asterfort/cbvalr.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mtdsc2.h"
#include "asterfort/pteddl.h"
#include "asterfort/vecinc.h"
#include "asterfort/vecini.h"
#include "asterfort/wkvect.h"
!
    integer :: nbcomb, lmat(*), lres
    character(len=*) :: ddlexc, typcst(*), typres
    real(kind=8) :: const(*)
    aster_logical :: matd
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
!       *  LES MATRICES SONT SUPPOSEES ETRE DE MEME STOCKAGE
!          MAIS PEUVENT ETRE A ELEMENTS REELS OU COMPLEXES
!       *  LES SCALAIRES SONT REELS OU COMPLEXES
!     -----------------------------------------------------------------
! IN  I  NBCOMB = NOMBRE DE MATRICES A COMBINER
! IN  R  CONST  = TABLEAU DE R*8    DES COEFICIENTS
! IN  I  LMAT = TABLEAU DES POINTEURS DES MATRICES
! IN  K1 TYPRES = TYPE DE LA MATRICE RESULTAT   (R/C)
! IN  I  LRES = POINTEUR DE MATRICE RESULTAT
! IN  K* DDLEXC = NOM DES DDLS A EXCLURE (CONCRETEMENT IL S'AGIT
!                                         DES LAGRANGE)
!
!     -----------------------------------------------------------------
!
!
!     -----------------------------------------------------------------
!     LGBLOC = LONGUEUR DES BLOCS
    integer :: lgbloc
    aster_logical :: symr, symi
!     -----------------------------------------------------------------
    character(len=1) :: clas, typmat
    character(len=19) :: matres, mati
    character(len=24) :: valmr, valmi, mat1
    character(len=8) :: nomddl
    character(len=14) :: nume
    character(len=19) :: noma
    character(len=2) :: rouc
    integer :: neq, mxddl, lddl, jsmdi, jsmhc
    integer :: iconst, imat, jvamr1, jvamr2, jvami1, jvami2
    real(kind=8) :: zero, r8cst, rbid
    complex(kind=8) :: czero, c8cst, cbid
    character(len=24), pointer :: refa(:) => null()
!     -----------------------------------------------------------------
    call jemarq()
    zero = 0.d0
    czero = dcmplx(zero,zero)
    rbid = zero
    cbid = czero
!
!
    nomddl = ddlexc
    matres = zk24(zi(lres+1))
    if (matd) then
        neq = zi(lres+5)
    else
        neq = zi(lres+2)
    endif
    call jelira(matres//'.REFA', 'CLAS', cval=clas)
    valmr = matres//'.VALM'
    lgbloc = zi(lres+14)
!
!
    mat1 = zk24(zi(lmat(1)+1))
    noma = mat1
    mxddl = 1
!
!     I) RECUPERATION DU NOM DE LA NUMEROTATION ASSOCIEE AUX MATRICES
    call dismoi('NOM_NUME_DDL', noma, 'MATR_ASSE', repk=nume)
!
!     II) RECUPERATION DES POSITIONS DES DDL
    call wkvect('&&CBVALE', 'V V I', neq*mxddl, lddl)
    call pteddl('NUME_DDL', nume, mxddl, nomddl, neq,&
                zi(lddl))
!
    symr = zi(lres+4) .eq. 1
    ASSERT(typres.eq.'R' .or. typres.eq.'C')
!
!
    call mtdsc2(zk24(zi(lres+1)), 'SMDI', 'L', jsmdi)
    call jeveuo(zk24(zi(lres+1)) (1:19)//'.REFA', 'L', vk24=refa)
    call jeveuo(refa(2) (1:14)//'.SMOS.SMHC', 'L', jsmhc)
!
!
    call jeveuo(jexnum(valmr, 1), 'E', jvamr1)
    if (.not.symr) call jeveuo(jexnum(valmr, 2), 'E', jvamr2)
!
!
! --- MISE A ZERO DE LA MATRICE RESULTAT :
!     ----------------------------------------
    if (typres .eq. 'R') then
        call vecini(lgbloc, zero, zr(jvamr1))
        if (.not.symr) call vecini(lgbloc, zero, zr(jvamr2))
!
    else if (typres.eq.'C') then
        call vecinc(lgbloc, czero, zc(jvamr1))
        if (.not.symr) call vecinc(lgbloc, czero, zc(jvamr2))
    endif
!
!
! --- BOUCLE SUR LES MATRICES A COMBINER ---
!     ----------------------------------------
    iconst = 1
    do imat = 1, nbcomb
        if (typcst(imat) .eq. 'R') then
            r8cst = const(iconst)
            c8cst = dcmplx(r8vide(),r8vide())
            iconst=iconst+1
        else
            r8cst = r8vide()
            c8cst = dcmplx(const(iconst),const(iconst+1))
            iconst=iconst+2
        endif
        mati = zk24(zi(lmat(imat)+1))
        valmi = mati//'.VALM'
        call jelira(valmi, 'TYPE', cval=typmat)
        ASSERT(typmat.eq.'R' .or. typmat.eq.'C')
        call jeveuo(jexnum(valmi, 1), 'L', jvami1)
        symi = zi(lmat(imat)+4) .eq. 1
        if (.not.symi) call jeveuo(jexnum(valmi, 2), 'L', jvami2)
        rouc=typres(1:1)//typcst(imat)(1:1)
!
!
        if (typres .eq. 'R') then
!       --------------------------
            if (typmat .eq. 'R') then
!         --------------------------
                call cbvalr(rouc, neq, zi4(jsmhc), zi(jsmdi), zi(lddl),&
                            r8cst, c8cst, zr(jvami1), zr(jvamr1), [cbid])
                if (.not.symr) then
                    if (symi) then
                        call cbvalr(rouc, neq, zi4(jsmhc), zi(jsmdi), zi( lddl),&
                                    r8cst, c8cst, zr(jvami1), zr(jvamr2), [cbid])
                    else
                        call cbvalr(rouc, neq, zi4(jsmhc), zi(jsmdi), zi( lddl),&
                                    r8cst, c8cst, zr(jvami2), zr(jvamr2), [cbid])
                    endif
                endif
!
            else if (typmat.eq.'C') then
!         --------------------------
                call cbvalc(rouc, neq, zi4(jsmhc), zi(jsmdi), zi(lddl),&
                            r8cst, c8cst, zc(jvami1), zr(jvamr1), [cbid])
                if (.not.symr) then
                    if (symi) then
                        call cbvalc(rouc, neq, zi4(jsmhc), zi(jsmdi), zi( lddl),&
                                    r8cst, c8cst, zc(jvami1), zr(jvamr2), [cbid])
                    else
                        call cbvalc(rouc, neq, zi4(jsmhc), zi(jsmdi), zi( lddl),&
                                    r8cst, c8cst, zc(jvami2), zr(jvamr2), [cbid])
                    endif
                endif
            endif
!
!
        else if (typres.eq.'C') then
!       --------------------------
            if (typmat .eq. 'R') then
!         --------------------------
                call cbvalr(rouc, neq, zi4(jsmhc), zi(jsmdi), zi(lddl),&
                            r8cst, c8cst, zr(jvami1), [rbid], zc(jvamr1))
                if (.not.symr) then
                    if (symi) then
                        call cbvalr(rouc, neq, zi4(jsmhc), zi(jsmdi), zi( lddl),&
                                    r8cst, c8cst, zr(jvami1), [rbid], zc(jvamr2))
                    else
                        call cbvalr(rouc, neq, zi4(jsmhc), zi(jsmdi), zi( lddl),&
                                    r8cst, c8cst, zr(jvami2), [rbid], zc(jvamr2))
                    endif
                endif
!
            else if (typmat.eq.'C') then
!         --------------------------
                call cbvalc(rouc, neq, zi4(jsmhc), zi(jsmdi), zi(lddl),&
                            r8cst, c8cst, zc(jvami1), [rbid], zc(jvamr1))
                if (.not.symr) then
                    if (symi) then
                        call cbvalc(rouc, neq, zi4(jsmhc), zi(jsmdi), zi( lddl),&
                                    r8cst, c8cst, zc(jvami1), [rbid], zc(jvamr2))
                    else
                        call cbvalc(rouc, neq, zi4(jsmhc), zi(jsmdi), zi( lddl),&
                                    r8cst, c8cst, zc(jvami2), [rbid], zc(jvamr2))
                    endif
                endif
            endif
        endif
!
!
        call jelibe(jexnum(valmi, 1))
        if (.not.symi) call jelibe(jexnum(valmi, 2))
    end do
!
!
!
    call jedetr('&&CBVALE')
    call jedema()
end subroutine
