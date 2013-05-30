subroutine afchno(chamn, base, gran, noma, nbnoeu,&
                  nbcpno, desc, lonval, typval, rval,&
                  cval, kval)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/cmpcha.h'
    include 'asterfort/crchno.h'
    include 'asterfort/crprno.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/pteequ.h'
    integer :: nbcpno(*), desc(*)
    real(kind=8) :: rval(*)
    complex(kind=8) :: cval(*)
    character(len=*) :: chamn, gran, noma, base, typval, kval(*)
!--------------------------------------------------------------------
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
!--------------------------------------------------------------------
!
!
!
    character(len=8) :: k8b
    character(len=19) :: chamno
    character(len=1) :: k1bid
    integer :: ncmp, ncmpmx, jcorr2
!
!-----------------------------------------------------------------------
    integer :: i1, ic, idec, ie, iec, ii, inec
    integer :: ino, jj, lnueq, lonval, lprno, lvale, nbnoeu
    integer :: nec, nn, numgd
!-----------------------------------------------------------------------
    call jemarq()
    chamno = chamn
!
    call jenonu(jexnom('&CATA.GD.NOMGD', gran), numgd)
    call jelira(jexnum('&CATA.GD.NOMCMP', numgd), 'LONMAX', ncmpmx, k1bid)
    call dismoi('F', 'NB_EC', gran, 'GRANDEUR', nec,&
                k8b, ie)
!
!     --- CREATION DU CHAMP ---
!
    call crchno(chamno, chamno, gran, noma, base,&
                typval, nbnoeu, lonval)
!
!     --- CONSTRUCTION DU PROF_CHNO ---
!
    call crprno(chamno, base, nbnoeu, lonval)
!
!     --- AFFECTATION DU .PRNO DE L'OBJET PROF_CHNO ---
!
    call jeveuo(chamno//'.PRNO', 'E', lprno)
    ii = 0
    idec = 1
    do 100 ino = 1, nbnoeu
        zi(lprno-1+ (nec+2)*(ino-1)+1) = idec
        zi(lprno-1+ (nec+2)*(ino-1)+2) = nbcpno(ino)
        do 102 inec = 1, nec
            ii = ii + 1
            zi(lprno-1+ (nec+2)*(ino-1)+2+inec) = desc(ii)
102      continue
        idec = idec + nbcpno(ino)
100  end do
!
!     --- AFFECTATION DU .VALE DE L'OBJET CHAMNO ---
!
    call jeveuo(chamno//'.VALE', 'E', lvale)
    call jeveuo(chamno//'.NUEQ', 'E', lnueq)
    do 110 ino = 1, nbnoeu
        i1 = zi(lprno-1+ (nec+2)*(ino-1)+1) + lnueq - 1
        do 112 ic = 1, ncmpmx
            iec = ( ic - 1 ) / 30 + 1
            jj = ic - 30 * ( iec - 1 )
            ii = 2**jj
            nn = iand( desc((ino-1)*nec+iec) , ii )
            if (nn .gt. 0) then
                if (typval(1:1) .eq. 'R') then
                    zr(lvale-1+zi(i1)) = rval((ino-1)*ncmpmx+ic)
                else if (typval(1:1).eq.'C') then
                    zc(lvale-1+zi(i1)) = cval((ino-1)*ncmpmx+ic)
                else if (typval(1:2).eq.'K8') then
                    zk8(lvale-1+zi(i1)) = kval((ino-1)*ncmpmx+ic)
                endif
                i1 = i1 + 1
            endif
112      continue
110  end do
!
!
!     -- CALCUL DE L'OBJET .DEEQ :
    call cmpcha(chamno, '&&AFCHNO.NOMCMP', '&&AFCHNO.CORR1', '&&AFCHNO.CORR2', ncmp,&
                ncmpmx)
    call jeveuo('&&AFCHNO.CORR2', 'L', jcorr2)
    call pteequ(chamno, base, lonval, numgd, ncmp,&
                zi(jcorr2))
    call jedetr('&&AFCHNO.NOMCMP')
    call jedetr('&&AFCHNO.CORR1')
    call jedetr('&&AFCHNO.CORR2')
!
    call jedema()
end subroutine
