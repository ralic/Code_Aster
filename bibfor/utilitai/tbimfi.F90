subroutine tbimfi(nparfi, table, newtab, iret)
    implicit   none
    include 'jeveux.h'
    include 'asterc/getvc8.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/tbextb.h'
    include 'asterfort/wkvect.h'
    integer :: nparfi, iret
    character(len=19) :: table, newtab
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     OPERATEUR  IMPR_TABLE , TRAITEMENT DU MOT CLE FACTEUR "FILTRE"
!     ------------------------------------------------------------------
!
    integer :: ibid, ltitr, jtitr, ititr, ii, ir, ic, ik, ioc, lonmax, lonma1
    integer :: jpafi, jccfi, jvifi, jvrfi, jvcfi, jvkfi, jprfi, jcrfi, l, l1, l2
    integer :: l3, l4, irt
    real(kind=8) :: r8b
    complex(kind=8) :: cbid
    character(len=8) :: k8b
    character(len=80) :: montit
    integer :: iarg
!     ------------------------------------------------------------------
    call jemarq()
!
    call jeexin(table//'.TITR', irt)
    if (irt .ne. 0) then
        call jeveuo(table//'.TITR', 'L', ltitr)
        call jelira(table//'.TITR', 'LONMAX', lonma1, k8b)
        lonmax = lonma1 + nparfi
        call wkvect(newtab//'.TITR', 'V V K80', lonmax, jtitr)
        do 10 ititr = 1, lonma1
            zk80(jtitr+ititr-1) = zk80(ltitr+ititr-1)
10      continue
    else
        lonma1 = 0
        lonmax = lonma1 + nparfi
        call wkvect(newtab//'.TITR', 'V V K80', lonmax, jtitr)
    endif
!
    call wkvect('&&TBIMFI.NOMS_PARA', 'V V K24', nparfi, jpafi)
    call wkvect('&&TBIMFI.CRIT_PARA', 'V V K8', nparfi, jccfi)
    call wkvect('&&TBIMFI.VALE_I', 'V V I', nparfi, jvifi)
    call wkvect('&&TBIMFI.VALE_R', 'V V R', nparfi, jvrfi)
    call wkvect('&&TBIMFI.VALE_C', 'V V C', nparfi, jvcfi)
    call wkvect('&&TBIMFI.VALE_K', 'V V K80', nparfi, jvkfi)
    call wkvect('&&TBIMFI.PRECISION', 'V V R', nparfi, jprfi)
    call wkvect('&&TBIMFI.CRITERE', 'V V K8', nparfi, jcrfi)
!
    ii = -1
    ir = -1
    ic = -1
    ik = -1
!
    do 20 ioc = 1, nparfi
        call getvtx('FILTRE', 'NOM_PARA', ioc, iarg, 1,&
                    zk24(jpafi+ioc-1), l)
        call getvtx('FILTRE', 'CRIT_COMP', ioc, iarg, 1,&
                    zk8(jccfi+ioc-1), l)
        montit = ' '
        call getvis('FILTRE', 'VALE_I', ioc, iarg, 0,&
                    ibid, l1)
        call getvr8('FILTRE', 'VALE', ioc, iarg, 0,&
                    r8b, l2)
        call getvc8('FILTRE', 'VALE_C', ioc, iarg, 0,&
                    cbid, l3)
        call getvtx('FILTRE', 'VALE_K', ioc, iarg, 0,&
                    k8b, l4)
        if (l1 .ne. 0) then
            ii = ii + 1
            call getvis('FILTRE', 'VALE_I', ioc, iarg, 1,&
                        zi(jvifi+ii), l)
            write(montit,1010) zk24(jpafi+ioc-1), zk8(jccfi+ioc-1),&
            zi(jvifi+ii)
        endif
        if (l2 .ne. 0) then
            ir = ir + 1
            call getvr8('FILTRE', 'VALE', ioc, iarg, 1,&
                        zr(jvrfi+ir), l)
            call getvr8('FILTRE', 'PRECISION', ioc, iarg, 1,&
                        zr(jprfi+ir), l)
            call getvtx('FILTRE', 'CRITERE', ioc, iarg, 1,&
                        zk8(jcrfi+ir), l)
            write(montit,1020) zk24(jpafi+ioc-1), zk8(jccfi+ioc-1),&
            zr(jvrfi+ir)
        endif
        if (l3 .ne. 0) then
            ic = ic + 1
            call getvc8('FILTRE', 'VALE_C', ioc, iarg, 1,&
                        zc(jvcfi+ic), l)
            write(montit,1030) zk24(jpafi+ioc-1), zk8(jccfi+ioc-1),&
            zc(jvcfi+ic)
        endif
        if (l4 .ne. 0) then
            ik = ik + 1
            call getvtx('FILTRE', 'VALE_K', ioc, iarg, 1,&
                        zk80(jvkfi+ik), l)
            write(montit,1040) zk24(jpafi+ioc-1), zk8(jccfi+ioc-1),&
            zk80(jvkfi+ik)
        endif
        zk80(jtitr+lonma1+ioc-1) = montit
20  end do
!
    call tbextb(table, 'V', newtab, nparfi, zk24(jpafi),&
                zk8(jccfi), zi(jvifi), zr(jvrfi), zc(jvcfi), zk80(jvkfi),&
                zr(jprfi), zk8(jcrfi), iret)
!
    call jedetr('&&TBIMFI.NOMS_PARA')
    call jedetr('&&TBIMFI.CRIT_PARA')
    call jedetr('&&TBIMFI.VALE_I')
    call jedetr('&&TBIMFI.VALE_R')
    call jedetr('&&TBIMFI.VALE_C')
    call jedetr('&&TBIMFI.VALE_K')
    call jedetr('&&TBIMFI.PRECISION')
    call jedetr('&&TBIMFI.CRITERE')
!
    1010 format('FILTRE -> NOM_PARA: ',a16,' CRIT_COMP: ',a4,' VALE: ',i12)
    1020 format('FILTRE -> NOM_PARA: ',a16,' CRIT_COMP: ',a4,&
     &                                ' VALE: ',1pe12.5)
    1030 format('FILTRE -> NOM_PARA: ',a16,' CRIT_COMP: ',a4,&
     &                                ' VALE: ',1pe12.5,1x,1pe12.5)
    1040 format('FILTRE -> NOM_PARA: ',a16,' CRIT_COMP: ',a4,' VALE: ',a8)
!
    call jedema()
end subroutine
