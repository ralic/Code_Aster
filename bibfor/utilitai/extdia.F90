subroutine extdia(matr, numddl, icode, diag)
    implicit none
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
!***********************************************************************
! 15/03/91    G.JACQUART AMV/P61 47 65 49 41
!***********************************************************************
!
!     FONCTION : EXTRACTION DE LA DIAGONALE D'UNE MATRICE
!
!-----------------------------------------------------------------------
!    MATR   /I/ : NOM DE LA MATRICE
!    NUMDDL /I/ : NUMEROTATION ASSOCIEE A MATR
!    ICODE  /I/ : 2 SI CALCUL TRANSITOIRE DIRECT
!                 1 SI SOUS-STRUCTURATION DYNAMIQUE TRANSITOIRE
!                   SANS DOUBLE PROJECTION
!                 0 SINON
!    DIAG   /O/ : VECTEUR CONTENANT LA DIAGONALE DE MATR
!-----------------------------------------------------------------------
!   Note : if the matrix is complex, return the moduli of the diagonal terms
!
!
!
!
#include "jeveux.h"
#include "asterfort/gettco.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mtdscr.h"
#include "asterfort/typddl.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=24) :: numddl
    character(len=8) :: matr
    real(kind=8) :: diag(*)
    integer :: icode
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    aster_logical :: iscmplx
    integer :: idia, j,  jbloc,   k
    integer :: l, lmat, nbacti, nbbloq, nblagr, nbliai, neq
    character(len=24) :: typmatr
    integer, pointer :: vtypddl(:) => null()
    integer, pointer :: smdi(:) => null()
    integer, pointer :: smde(:) => null()

!
!-----------------------------------------------------------------------
    call jemarq()
    call mtdscr(matr)
    call jeveuo(matr//'           .&INT', 'L', lmat)
!
    call gettco(matr, typmatr)
    iscmplx = (typmatr(1:9).eq.'MATR_ASSE') .and. (typmatr(16:16).eq.'C')
!
    call jeveuo(numddl(1:8)//'      .SMOS.SMDE', 'L', vi=smde)
    neq = smde(1)
!
    AS_ALLOCATE(vi=vtypddl, size=neq)
    call typddl('ACTI', numddl(1:8), neq, vtypddl, nbacti,&
                nbbloq, nblagr, nbliai)
    if (icode .eq. 2) then
        if (nbliai .gt. 0) then
            call utmess('F', 'UTILITAI_76')
        endif
    endif
!
    call jeveuo(numddl(1:8)//'      .SMOS.SMDI', 'L', vi=smdi)
    k = 0
    l = 0
    call jeveuo(jexnum(matr//'           .VALM', 1), 'L', jbloc)
    if (.not.(iscmplx) )then
        do j = 1, neq
            k = k + 1
            if (vtypddl(k) .ne. 0) then
                idia=smdi(k)
                l=l+1
                diag(l)=zr(jbloc-1+idia)
            else if (icode.eq.0.or.icode.eq.2) then
                l=l+1
                diag(l)=0.d0
            endif
        end do
    else 
        do j = 1, neq
            k = k + 1
            if (vtypddl(k) .ne. 0) then
                idia=smdi(k)
                l=l+1
                diag(l)=sqrt(real(zc(jbloc-1+idia))**2+imag(zc(jbloc-1+idia))**2)
            else if (icode.eq.0.or.icode.eq.2) then
                l=l+1
                diag(l)=0.d0
            endif
        end do
    end if
    AS_DEALLOCATE(vi=vtypddl)
!
    call jedema()
end subroutine
