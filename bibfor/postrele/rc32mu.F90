subroutine rc32mu()
    implicit   none
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'jeveux.h'
    include 'asterc/getvid.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rc32my.h'
    include 'asterfort/rcver1.h'
    include 'asterfort/rcveri.h'
    include 'asterfort/tbexip.h'
    include 'asterfort/tbexv1.h'
    include 'asterfort/tbliva.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    integer :: ibid, ns(13), nbabsc, jabsc, iret, jmune, jmuno, i, j, k, l, ndim
    integer :: jcont, ncmp, jcorp
    parameter  ( ncmp = 6 )
    real(kind=8) :: prec, momen0, momen1
    complex(kind=8) :: cbid
    logical :: exist
    character(len=8) :: k8b, crit, nocmp(ncmp), tbsig(13)
    character(len=16) :: motclf, valek
    character(len=24) :: abscur
    character(len=24) :: valk(7)
    integer :: iarg
! DEB ------------------------------------------------------------------
    call jemarq()
!
    motclf = 'RESU_MECA_UNIT'
    prec = 1.0d-06
    crit = 'RELATIF'
!
    call getvid(motclf, 'TABL_PRES', 1, iarg, 1,&
                tbsig(13), ns(13))
    if (ns(13) .ne. 0) then
        call rcveri(tbsig(13))
    endif
!
    call getvid(motclf, 'TABL_FX', 1, iarg, 1,&
                tbsig(1), ns(1))
    if (ns(1) .eq. 0) call getvid(motclf, 'TABL_FX_TUBU', 1, iarg, 1,&
                                  tbsig(1), ns(1))
    if (ns(1) .ne. 0) call rcver1('MECANIQUE', tbsig(13), tbsig(1))
    call getvid(motclf, 'TABL_FY', 1, iarg, 1,&
                tbsig(2), ns(2))
    if (ns(2) .eq. 0) call getvid(motclf, 'TABL_FY_TUBU', 1, iarg, 1,&
                                  tbsig(2), ns(2))
    if (ns(2) .ne. 0) call rcver1('MECANIQUE', tbsig(13), tbsig(2))
    call getvid(motclf, 'TABL_FZ', 1, iarg, 1,&
                tbsig(3), ns(3))
    if (ns(3) .eq. 0) call getvid(motclf, 'TABL_FZ_TUBU', 1, iarg, 1,&
                                  tbsig(3), ns(3))
    if (ns(3) .ne. 0) call rcver1('MECANIQUE', tbsig(13), tbsig(3))
!
    call getvid(motclf, 'TABL_MX', 1, iarg, 1,&
                tbsig(4), ns(4))
    if (ns(4) .eq. 0) call getvid(motclf, 'TABL_MX_TUBU', 1, iarg, 1,&
                                  tbsig(4), ns(4))
    if (ns(4) .ne. 0) call rcver1('MECANIQUE', tbsig(13), tbsig(4))
    call getvid(motclf, 'TABL_MY', 1, iarg, 1,&
                tbsig(5), ns(5))
    if (ns(5) .eq. 0) call getvid(motclf, 'TABL_MY_TUBU', 1, iarg, 1,&
                                  tbsig(5), ns(5))
    if (ns(5) .ne. 0) call rcver1('MECANIQUE', tbsig(13), tbsig(5))
    call getvid(motclf, 'TABL_MZ', 1, iarg, 1,&
                tbsig(6), ns(6))
    if (ns(6) .eq. 0) call getvid(motclf, 'TABL_MZ_TUBU', 1, iarg, 1,&
                                  tbsig(6), ns(6))
    if (ns(6) .ne. 0) call rcver1('MECANIQUE', tbsig(13), tbsig(6))
!
    call getvid(motclf, 'TABL_FX_CORP', 1, iarg, 1,&
                tbsig(7), ns(7))
    if (ns(7) .ne. 0) then
        call rcver1('MECANIQUE', tbsig(13), tbsig(7))
    endif
    call getvid(motclf, 'TABL_FY_CORP', 1, iarg, 1,&
                tbsig(8), ns(8))
    if (ns(8) .ne. 0) then
        call rcver1('MECANIQUE', tbsig(13), tbsig(8))
    endif
    call getvid(motclf, 'TABL_FZ_CORP', 1, iarg, 1,&
                tbsig(9), ns(9))
    if (ns(9) .ne. 0) then
        call rcver1('MECANIQUE', tbsig(13), tbsig(9))
    endif
    call getvid(motclf, 'TABL_MX_CORP', 1, iarg, 1,&
                tbsig(10), ns(10))
    if (ns(10) .ne. 0) then
        call rcver1('MECANIQUE', tbsig(13), tbsig(10))
    endif
    call getvid(motclf, 'TABL_MY_CORP', 1, iarg, 1,&
                tbsig(11), ns(11))
    if (ns(11) .ne. 0) then
        call rcver1('MECANIQUE', tbsig(13), tbsig(11))
    endif
    call getvid(motclf, 'TABL_MZ_CORP', 1, iarg, 1,&
                tbsig(12), ns(12))
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
        call u2mesk('F', 'POSTRCCM_1', 2, valk)
    endif
    abscur = '&&RC32MU.ABSC_CURV'
    call tbexv1(tbsig(4), valek, abscur, 'V', nbabsc,&
                k8b)
    call jeveuo(abscur, 'L', jabsc)
!
    call wkvect('&&RC32MU.CONTRAINTES', 'V V R', nbabsc, jcont)
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
            call u2mesk('F', 'POSTRCCM_1', 2, valk)
        endif
        do 12 j = 1, ncmp
!
            do 14 k = 1, nbabsc
                call tbliva(tbsig(i), 1, valek, ibid, zr(jabsc+k-1),&
                            cbid, k8b, crit, prec, nocmp(j),&
                            k8b, ibid, zr(jcont+ k-1), cbid, k8b,&
                            iret)
                if (iret .ne. 0) then
                    valk (1) = tbsig(i)
                    valk (2) = nocmp(j)
                    valk (3) = valek
                    call u2mesg('F', 'POSTRCCM_44', 3, valk, 0,&
                                0, 1, zr( jabsc+k-1))
                endif
14          continue
!
            l = ncmp*(i-1) + j
            zr(jmuno-1+l) = zr(jcont)
            zr(jmune-1+l) = zr(jcont+nbabsc-1)
!
            call rc32my(nbabsc, zr(jabsc), zr(jcont), momen0, momen1)
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
    call jedetr('&&RC32MU.CONTRAINTES')
!
    call jedema()
end subroutine
