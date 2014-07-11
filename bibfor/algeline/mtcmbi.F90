subroutine mtcmbi(typmat, lmat, coef, ccoef, lres)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mtconl.h"
#include "asterfort/mtdsc2.h"
#include "asterfort/pteddl.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: lmat, lres
    character(len=*) :: typmat
    complex(kind=8) :: ccoef
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
!     DUPLIQUE LA MATRICE EN METTANT TOUTES LES TERMES A ZERO SAUF
!     LES "LAGRANGE" EN LEUR APPLIQUANT UN COEFFICIENT.
!     -----------------------------------------------------------------
! IN  K* TYPMAT = TYPE DE MATRICE   (R OU C)
! IN  I  LMAT   = POINTEUR DE MATRICE
! IN  I  LRES   = POINTEUR DE MATRICE RESULTAT
!     -----------------------------------------------------------------
!     NBBLIC = NOMBRE DE BLOCS POUR .VALI DE LA MATRICE
!     LGBLOC = LONGUEUR DES BLOCS
!     -----------------------------------------------------------------
    integer :: lgbloc
    real(kind=8) :: const(2)
    character(len=1) :: ch1, typcst
    character(len=8) :: nomddl
    character(len=14) :: nume
    character(len=19) :: matres, noma
    character(len=24) :: valm, valmr
    complex(kind=8) :: czero
    aster_logical :: matsym
!     -----------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iatmai, iatmat, iatrei, iatres, ibid, icoef
    integer :: idebli, iequa, ifinli, ilig, ind, ival
    integer :: jsmdi, jsmhc, kin, lddl, neq
!
    real(kind=8) :: coef, zero
    character(len=24), pointer :: refa(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
!
    if (typmat(1:1) .ne. 'R' .and. typmat(1:1) .ne. 'C') then
        ch1 = typmat(1:1)
        call utmess('F', 'ALGELINE2_6', sk=ch1)
    endif
!
!     --- AFFE_CHAR_CINE ? ---
!
    if (zi(lmat+7) .ne. 0) then
        call utmess('F', 'ALGELINE2_7')
    endif
!
    zero = 0.d0
    czero = dcmplx(zero,zero)
    matsym = .true.
!
    if (zi(lmat+4) .ne. 1) matsym = .false.
    noma = zk24(zi(lmat+1)) (1:19)
    valm = noma//'.VALM'
!
    neq = zi(lres+2)
    call mtdsc2(zk24(zi(lres+1)), 'SMDI', 'L', jsmdi)
    lgbloc = zi(lres+14)
    matres = zk24(zi(lres+1)) (1:19)
    call jeveuo(matres//'.REFA', 'L', vk24=refa)
    call jeveuo(refa(2)(1:14)//'.SMOS.SMHC', 'L', jsmhc)
    call jeveuo(refa(2)(1:14)//'.SMOS.SMDI', 'L', ibid)
    ASSERT(ibid.eq.jsmdi)
!
    valmr = matres//'.VALM'
!
!     --- NOM DE LA NUMEROTATION ASSOCIEE A LA MATRICE ---
    call dismoi('NOM_NUME_DDL', noma, 'MATR_ASSE', repk=nume)
!
!
!     --- TOUTES COMPOSANTES A ZERO SAUF LES LAGRANGES ---
    nomddl = 'LAGR    '
    call wkvect('&&MTCMBI', 'V V I', neq, lddl)
    call pteddl('NUME_DDL', nume, 1, nomddl, neq,&
                list_equa = zi(lddl))
    do i = 0, neq - 1
        zi(lddl+i) = 1 - zi(lddl+i)
    end do
!
!
!
    call jeveuo(jexnum(valmr, 1), 'E', iatres)
    if (.not.matsym) then
        call jeveuo(jexnum(valmr, 2), 'E', iatrei)
    endif
!
    if (typmat(1:1) .eq. 'R') then
        do ival = iatres, iatres + lgbloc - 1
            zr(ival) = zero
        end do
        if (.not.matsym) then
            do ival = iatrei, iatrei + lgbloc - 1
                zr(ival) = zero
            end do
        endif
    else
        do ival = iatres, iatres + lgbloc - 1
            zc(ival) = czero
        end do
    endif
!
    call jeveuo(jexnum(valm, 1), 'L', iatmat)
    if (.not.matsym) then
        call jeveuo(jexnum(valm, 2), 'E', iatmai)
    endif
!
!
    if (typmat(1:1) .eq. 'R') then
        kin = 0
        idebli = 1
        do iequa = 1, neq
            ifinli = zi(jsmdi+iequa-1)
            do ind = idebli, ifinli
                kin = kin + 1
                ilig=zi4(jsmhc-1+kin)
                icoef = min((2-zi(lddl+ilig-1)-zi(lddl+iequa-1)),1)
                zr(iatres+kin-1) = zr(iatres+kin-1) + zr(iatmat+kin-1) *icoef*coef
            end do
            idebli = zi(jsmdi+iequa-1) + 1
        end do
!
!
    else if (typmat(1:1).eq.'C') then
        kin = 0
        idebli = 1
        do iequa = 1, neq
            ifinli = zi(jsmdi+iequa-1)
            do ind = idebli, ifinli
                kin = kin + 1
                ilig=zi4(jsmhc-1+kin)
                icoef = min((2-zi(lddl+ilig-1)-zi(lddl+iequa-1)),1)
                zc(iatres+kin-1) = zc(iatres+kin-1) + zc(iatmat+kin-1) *icoef*ccoef
            end do
            idebli = zi(jsmdi+iequa-1) + 1
        end do
    endif
!
!
    call jelibe(jexnum(valm, 1))
    if (.not.matsym) then
        call jelibe(jexnum(valm, 2))
    endif
    call jelibe(jexnum(valmr, 1))
    if (.not.matsym) then
        call jelibe(jexnum(valmr, 2))
    endif
!
!
!     --- ACTUALISATION DU .CONL ----
    if (typmat(1:1) .eq. 'R') then
        typcst = 'R'
        const(1) = 1.d0
    else
        typcst = 'C'
        const(1) = 1.d0
        const(2) = 1.d0
    endif
    call mtconl(1, typcst, const, [lmat], typmat,&
                lres)
!
    call jedetr('&&MTCMBI')
!
!
    call jedema()
end subroutine
