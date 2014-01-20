subroutine rc32mu()
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     LECTURE DU MOT CLE FACTEUR "RESU_MECA_UNIT"
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rc32my.h"
#include "asterfort/rcver1.h"
#include "asterfort/rcveri.h"
#include "asterfort/tbexip.h"
#include "asterfort/tbexv1.h"
#include "asterfort/tbliva.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: ibid, ns(13), nbabsc, jabsc, iret, jmune, jmuno, i, j, k, l, ndim
    integer ::  ncmp, jcorp
    parameter  ( ncmp = 6 )
    real(kind=8) :: prec, momen0, momen1
    complex(kind=8) :: cbid
    logical :: exist
    character(len=8) :: k8b, crit, nocmp(ncmp), tbsig(13)
    character(len=16) :: motclf, valek
    character(len=24) :: abscur
    character(len=24) :: valk(7)
    real(kind=8), pointer :: contraintes(:) => null()
! DEB ------------------------------------------------------------------
    call jemarq()
!
    motclf = 'RESU_MECA_UNIT'
    prec = 1.0d-06
    crit = 'RELATIF'
!
    call getvid(motclf, 'TABL_PRES', iocc=1, scal=tbsig(13), nbret=ns(13))
    if (ns(13) .ne. 0) then
        call rcveri(tbsig(13))
    endif
!
    call getvid(motclf, 'TABL_FX', iocc=1, scal=tbsig(1), nbret=ns(1))
    if (ns(1) .eq. 0) then
        call getvid(motclf, 'TABL_FX_TUBU', iocc=1, scal=tbsig(1), nbret=ns(1))
    endif
    if (ns(1) .ne. 0) call rcver1('MECANIQUE', tbsig(13), tbsig(1))
    call getvid(motclf, 'TABL_FY', iocc=1, scal=tbsig(2), nbret=ns(2))
    if (ns(2) .eq. 0) then
        call getvid(motclf, 'TABL_FY_TUBU', iocc=1, scal=tbsig(2), nbret=ns(2))
    endif
    if (ns(2) .ne. 0) call rcver1('MECANIQUE', tbsig(13), tbsig(2))
    call getvid(motclf, 'TABL_FZ', iocc=1, scal=tbsig(3), nbret=ns(3))
    if (ns(3) .eq. 0) then
        call getvid(motclf, 'TABL_FZ_TUBU', iocc=1, scal=tbsig(3), nbret=ns(3))
    endif
    if (ns(3) .ne. 0) call rcver1('MECANIQUE', tbsig(13), tbsig(3))
!
    call getvid(motclf, 'TABL_MX', iocc=1, scal=tbsig(4), nbret=ns(4))
    if (ns(4) .eq. 0) then
        call getvid(motclf, 'TABL_MX_TUBU', iocc=1, scal=tbsig(4), nbret=ns(4))
    endif
    if (ns(4) .ne. 0) call rcver1('MECANIQUE', tbsig(13), tbsig(4))
    call getvid(motclf, 'TABL_MY', iocc=1, scal=tbsig(5), nbret=ns(5))
    if (ns(5) .eq. 0) then
        call getvid(motclf, 'TABL_MY_TUBU', iocc=1, scal=tbsig(5), nbret=ns(5))
    endif
    if (ns(5) .ne. 0) call rcver1('MECANIQUE', tbsig(13), tbsig(5))
    call getvid(motclf, 'TABL_MZ', iocc=1, scal=tbsig(6), nbret=ns(6))
    if (ns(6) .eq. 0) then
        call getvid(motclf, 'TABL_MZ_TUBU', iocc=1, scal=tbsig(6), nbret=ns(6))
    endif
    if (ns(6) .ne. 0) call rcver1('MECANIQUE', tbsig(13), tbsig(6))
!
    call getvid(motclf, 'TABL_FX_CORP', iocc=1, scal=tbsig(7), nbret=ns(7))
    if (ns(7) .ne. 0) then
        call rcver1('MECANIQUE', tbsig(13), tbsig(7))
    endif
    call getvid(motclf, 'TABL_FY_CORP', iocc=1, scal=tbsig(8), nbret=ns(8))
    if (ns(8) .ne. 0) then
        call rcver1('MECANIQUE', tbsig(13), tbsig(8))
    endif
    call getvid(motclf, 'TABL_FZ_CORP', iocc=1, scal=tbsig(9), nbret=ns(9))
    if (ns(9) .ne. 0) then
        call rcver1('MECANIQUE', tbsig(13), tbsig(9))
    endif
    call getvid(motclf, 'TABL_MX_CORP', iocc=1, scal=tbsig(10), nbret=ns(10))
    if (ns(10) .ne. 0) then
        call rcver1('MECANIQUE', tbsig(13), tbsig(10))
    endif
    call getvid(motclf, 'TABL_MY_CORP', iocc=1, scal=tbsig(11), nbret=ns(11))
    if (ns(11) .ne. 0) then
        call rcver1('MECANIQUE', tbsig(13), tbsig(11))
    endif
    call getvid(motclf, 'TABL_MZ_CORP', iocc=1, scal=tbsig(12), nbret=ns(12))
    if (ns(12) .ne. 0) then
        call rcver1('MECANIQUE', tbsig(13), tbsig(12))
    endif
!
!
    call wkvect('&&RC3200.CORPS', 'V V L', 1, jcorp)
    if (ns(10) .ne. 0) then
        zl(jcorp) = .true.
    else
        zl(jcorp) = .false.
    endif
!
!
!
!
! --- ON RECUPERE L'ABSC_CURV DANS LA TABLE 'TABL_MX'
!
    valek = 'ABSC_CURV       '
    call tbexip(tbsig(4), valek, exist, k8b)
    if (.not. exist) then
        valk (1) = tbsig(4)
        valk (2) = valek
        call utmess('F', 'POSTRCCM_1', nk=2, valk=valk)
    endif
    abscur = '&&RC32MU.ABSC_CURV'
    call tbexv1(tbsig(4), valek, abscur, 'V', nbabsc,&
                k8b)
    call jeveuo(abscur, 'L', jabsc)
!
    AS_ALLOCATE(vr=contraintes, size=nbabsc)
!
    nocmp(1) = 'SIXX'
    nocmp(2) = 'SIYY'
    nocmp(3) = 'SIZZ'
    nocmp(4) = 'SIXY'
    nocmp(5) = 'SIXZ'
    nocmp(6) = 'SIYZ'
!
! --- 13 TABLES A  ( 6 COMPOSANTES + 6 LINEARISEES + 6 M_0 + 6 M_1 )
    ndim = 13 * ( 6 + 6 + 6 + 6 )
    call wkvect('&&RC3200.MECA_UNIT .ORIG', 'V V R', ndim, jmuno)
    call wkvect('&&RC3200.MECA_UNIT .EXTR', 'V V R', ndim, jmune)
!
! --- LES PROFILS DE CONTRAINTES ISSUS DES CALCULS MECANIQUES UNITAIRES
!
    do 10 i = 1, 13
!
        if (ns(i) .eq. 0) goto 10
!
        call tbexip(tbsig(i), valek, exist, k8b)
        if (.not. exist) then
            valk (1) = tbsig(i)
            valk (2) = valek
            call utmess('F', 'POSTRCCM_1', nk=2, valk=valk)
        endif
        do 12 j = 1, ncmp
!
            do 14 k = 1, nbabsc
                call tbliva(tbsig(i), 1, valek, [ibid], zr(jabsc+k-1),&
                            [cbid], k8b, crit, [prec], nocmp(j),&
                            k8b, ibid, contraintes(k), cbid, k8b,&
                            iret)
                if (iret .ne. 0) then
                    valk (1) = tbsig(i)
                    valk (2) = nocmp(j)
                    valk (3) = valek
                    call utmess('F', 'POSTRCCM_44', nk=3, valk=valk, sr=zr( jabsc+k-1))
                endif
14          continue
!
            l = ncmp*(i-1) + j
            zr(jmuno-1+l) = contraintes(1)
            zr(jmune-1+l) = contraintes(nbabsc)
!
            call rc32my(nbabsc, zr(jabsc), contraintes, momen0, momen1)
!
            l = 13*ncmp + ncmp*(i-1) + j
            zr(jmuno-1+l) = momen0 - 0.5d0*momen1
            zr(jmune-1+l) = momen0 + 0.5d0*momen1
!
            l = 2*13*ncmp + ncmp*(i-1) + j
            zr(jmuno-1+l) = momen0
            zr(jmune-1+l) = momen0
!
            l = 3*13*ncmp + ncmp*(i-1) + j
            zr(jmuno-1+l) = 0.5d0*momen1
            zr(jmune-1+l) = 0.5d0*momen1
!
12      continue
!
10  end do
!
    call jedetr(abscur)
    AS_DEALLOCATE(vr=contraintes)
!
    call jedema()
end subroutine
