subroutine rscrmo(iocc, nomsd, nomjv)
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/juveca.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsutnu.h'
    include 'asterfort/wkvect.h'
    integer :: iocc
    character(len=*) :: nomsd, nomjv
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
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: ibid, nbnosy, jnume, i, im, isy, nbordt
    integer :: jnosy, iacelk, iret, jmodl, np, nc, n22
    integer :: nbmodl, nbmmod
    real(kind=8) :: prec
    character(len=8) :: k8b, docu, crit
    character(len=16) :: nomsym
    character(len=19) :: nomd2, noch19
    character(len=24) :: knum
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
    nomd2 = nomsd
!
    knum = '&&RSCRMO.NUME_ORDRE'
    call getvr8('RESU', 'PRECISION', iocc, iarg, 1,&
                prec, np)
    call getvtx('RESU', 'CRITERE', iocc, iarg, 1,&
                crit, nc)
    call rsutnu(nomsd, 'RESU', iocc, knum, nbordt,&
                prec, crit, iret)
    if (iret .ne. 0) goto 9999
    call jeveuo(knum, 'L', jnume)
!
    call getvtx('RESU', 'NOM_CHAM', iocc, iarg, 0,&
                k8b, n22)
    if (n22 .ne. 0) then
        nbnosy = - n22
        call wkvect('&&RSCRMO.NOM_SYMBOL', 'V V K16', nbnosy, jnosy)
        call getvtx('RESU', 'NOM_CHAM', iocc, iarg, nbnosy,&
                    zk16(jnosy), n22)
    else
        call jelira(nomd2//'.DESC', 'NOMMAX', nbnosy, k8b)
        if (nbnosy .eq. 0) goto 9999
        call wkvect('&&RSCRMO.NOM_SYMBOL', 'V V K16', nbnosy, jnosy)
        do 10 isy = 1, nbnosy
            call jenuno(jexnum(nomd2//'.DESC', isy), zk16(jnosy-1+isy))
10      continue
    endif
!
    call jeexin(nomjv, iret)
    if (iret .eq. 0) then
        call wkvect(nomjv, 'V V K24', 10, jmodl)
        nbmodl = 0
        nbmmod = 10
        call jeecra(nomjv, 'LONUTI', nbmodl, ' ')
    else
        call jeveuo(nomjv, 'E', jmodl)
        call jelira(nomjv, 'LONUTI', nbmodl, k8b)
        call jelira(nomjv, 'LONMAX', nbmmod, k8b)
    endif
!
    do 20 i = 1, nbordt
        do 24 isy = 1, nbnosy
            nomsym = zk16(jnosy+isy-1)
            call rsexch(' ', nomsd, nomsym, zi(jnume+i-1), noch19,&
                        iret)
            if (iret .eq. 0) then
                call jeexin(noch19//'.DESC', ibid)
                if (ibid .gt. 0) then
                    call jelira(noch19//'.DESC', 'DOCU', ibid, docu)
                else
                    call jelira(noch19//'.CELD', 'DOCU', ibid, docu)
                endif
!
                if (docu(1:4) .eq. 'CHML') then
                    call jeveuo(noch19//'.CELK', 'L', iacelk)
                    do 26 im = 1, nbmodl
                        if (zk24(jmodl+im-1) .eq. zk24(iacelk)) goto 28
26                  continue
                    nbmodl = nbmodl + 1
                    if (nbmodl .gt. nbmmod) then
                        nbmmod = 2 * nbmmod
                        call juveca(nomjv, nbmmod)
                        call jeveuo(nomjv, 'E', jmodl)
                    endif
                    zk24(jmodl+nbmodl-1) = zk24(iacelk)
                    call jeecra(nomjv, 'LONUTI', nbmodl, ' ')
28                  continue
                endif
            endif
24      continue
20  end do
!
    call jedetr(knum)
    call jedetr('&&RSCRMO.NOM_SYMBOL')
!
9999  continue
    call jedema()
end subroutine
