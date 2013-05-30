subroutine rc32cm()
    implicit   none
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
!     LECTURE DU MOT CLE FACTEUR "CHAR_MECA"
!
!     ------------------------------------------------------------------
!
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterfort/codent.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    integer :: n1, n1t, iocc, ndim, nbchar, nume, jchar
    real(kind=8) :: r8b
    character(len=8) :: knumec
    character(len=16) :: motclf
    integer :: iarg
! DEB ------------------------------------------------------------------
    call jemarq()
!
    motclf = 'CHAR_MECA'
    call getfac(motclf, nbchar)
!
    ndim = 0
    do 10, iocc = 1, nbchar, 1
    call getvis(motclf, 'NUME_CHAR', iocc, iarg, 1,&
                nume, n1)
    ndim = max (ndim, nume)
    10 end do
!
    call jecrec('&&RC3200.VALE_CHAR', 'V V R', 'NO', 'DISPERSE', 'VARIABLE',&
                nbchar)
!
    do 20, iocc = 1, nbchar, 1
!
    call getvis(motclf, 'NUME_CHAR', iocc, iarg, 1,&
                nume, n1)
!
    knumec = 'C       '
    call codent(nume, 'D0', knumec(2:8))
!
!
    call jecroc(jexnom('&&RC3200.VALE_CHAR', knumec))
    call jeecra(jexnom('&&RC3200.VALE_CHAR', knumec), 'LONMAX', 12, ' ')
    call jeecra(jexnom('&&RC3200.VALE_CHAR', knumec), 'LONUTI', 12, ' ')
    call jeveuo(jexnom('&&RC3200.VALE_CHAR', knumec), 'E', jchar)
!
! ------ UN SEUL TENSEUR OU 2 ?
!
    call getvr8(motclf, 'MX', iocc, iarg, 0,&
                r8b, n1t)
!
    if (n1t .ne. 0) then
        call getvr8(motclf, 'FX', iocc, iarg, 1,&
                    zr(jchar-1+1), n1)
        call getvr8(motclf, 'FY', iocc, iarg, 1,&
                    zr(jchar-1+2), n1)
        call getvr8(motclf, 'FZ', iocc, iarg, 1,&
                    zr(jchar-1+3), n1)
        call getvr8(motclf, 'MX', iocc, iarg, 1,&
                    zr(jchar-1+4), n1)
        call getvr8(motclf, 'MY', iocc, iarg, 1,&
                    zr(jchar-1+5), n1)
        call getvr8(motclf, 'MZ', iocc, iarg, 1,&
                    zr(jchar-1+6), n1)
!
    else
        call getvr8(motclf, 'FX_TUBU', iocc, iarg, 1,&
                    zr(jchar-1+1), n1)
        call getvr8(motclf, 'FY_TUBU', iocc, iarg, 1,&
                    zr(jchar-1+2), n1)
        call getvr8(motclf, 'FZ_TUBU', iocc, iarg, 1,&
                    zr(jchar-1+3), n1)
        call getvr8(motclf, 'MX_TUBU', iocc, iarg, 1,&
                    zr(jchar-1+4), n1)
        call getvr8(motclf, 'MY_TUBU', iocc, iarg, 1,&
                    zr(jchar-1+5), n1)
        call getvr8(motclf, 'MZ_TUBU', iocc, iarg, 1,&
                    zr(jchar-1+6), n1)
!
        call getvr8(motclf, 'FX_CORP', iocc, iarg, 1,&
                    zr(jchar-1+7), n1)
        call getvr8(motclf, 'FY_CORP', iocc, iarg, 1,&
                    zr(jchar-1+8), n1)
        call getvr8(motclf, 'FZ_CORP', iocc, iarg, 1,&
                    zr(jchar-1+9), n1)
        call getvr8(motclf, 'MX_CORP', iocc, iarg, 1,&
                    zr(jchar-1+10), n1)
        call getvr8(motclf, 'MY_CORP', iocc, iarg, 1,&
                    zr(jchar-1+11), n1)
        call getvr8(motclf, 'MZ_CORP', iocc, iarg, 1,&
                    zr(jchar-1+12), n1)
    endif
!
    20 end do
!
    call jedema()
end subroutine
