subroutine jgetlmx(nomlu, ival)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: j-pierre.lefebvre at edf.fr
    implicit none
#include "asterf_types.h"
#include "jeveux_private.h"
#include "asterfort/codent.h"
#include "asterfort/jjallc.h"
#include "asterfort/jjcroc.h"
#include "asterfort/jjlide.h"
#include "asterfort/jjlirs.h"
#include "asterfort/jjvern.h"
#include "asterfort/utmess.h"
    character(len=*), intent(in) :: nomlu
    integer, intent(out) :: ival
!     ------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!     ------------------------------------------------------------------
    integer :: ib, ibacol, ic, id, ipgcex
    integer :: ixdeso, ixiadd
    integer :: ixlong, ixlono, ixluti, ixnom, ixnum, jcara
    integer :: jdate, jdocu, jgenr, jhcod, jiadd, jiadm, jlong
    integer :: jlono, jltyp, jluti, jmarq, jorig, jrnom, jtype
    integer :: ltypi, n
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!     ------------------------------------------------------------------
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
!
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
!     ------------------------------------------------------------------
    integer :: iddeso, idiadd, idnom, idlong, idlono, idluti
    integer :: idnum
    parameter    ( iddeso = 1 ,idiadd = 2 , &
     &                          idnom  = 5 ,             idlong = 7 ,&
     &             idlono = 8 , idluti = 9 ,idnum  = 10 )
!     ------------------------------------------------------------------
    character(len=32) :: noml32
    character(len=1) :: genri
    integer :: icre, iret
    aster_logical :: lconst, lcol
! DEB ------------------------------------------------------------------
!
    ipgcex = ipgc
    noml32 = nomlu
!
    ival = -1
    icre = 0
    call jjvern(noml32, icre, iret)
!
    ic = iclas
    if (iret .eq. 0) then
        call utmess('F', 'JEVEUX_26', sk=noml32(1:24))
    else if (iret .eq. 1) then
        lcol = .false.
        ic = iclaos
        id = idatos
        lconst = .true.
        if (noml32(25:32) .ne. '        ') then
            call utmess('F', 'JEVEUX1_09', sk=noml32(1:24))
        endif
    else
        lcol = .true.
        ipgc = -2
        ic = iclaco
        call jjallc(ic, idatco, 'L', ibacol)
        if (noml32(25:32) .ne. '        ') then
            iret = 3
            call jjcroc(noml32(25:32), icre)
            if (idatoc .eq. 0) then
                call utmess('F', 'JEVEUX_30', sk=noml32(1:24))
            endif
        endif
        ixdeso = iszon ( jiszon + ibacol + iddeso )
        id = ixdeso
        ixiadd = iszon ( jiszon + ibacol + idiadd )
        ixlong = iszon ( jiszon + ibacol + idlong )
        ixlono = iszon ( jiszon + ibacol + idlono )
        ixluti = iszon ( jiszon + ibacol + idluti )
        ixnom = iszon ( jiszon + ibacol + idnom )
        ixnum = iszon ( jiszon + ibacol + idnum )
        lconst = (ixlong .eq. 0 )
    endif
    genri = genr ( jgenr(ic) + id )
    ltypi = ltyp ( jltyp(ic) + id )
    if (lconst) then
        ival = long ( jlong(ic) + id )
    else if (iret .eq. 3) then
        ib = jiszon + iadm ( jiadm(ic) + 2*ixlong-1 ) - 1 + idatoc
        ival = iszon (ib)
    endif
!
    if (lcol) then
        call jjlide('JELIBE', noml32(1:24), 2)
    endif
    ipgc = ipgcex
!
end subroutine
