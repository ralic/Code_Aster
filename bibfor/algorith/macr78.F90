subroutine macr78(nomres, trange, typres)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/gettco.h'
    include 'asterc/getvid.h'
    include 'asterc/getvtx.h'
    include 'asterfort/copmod.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mdgeph.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rscrsd.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsnoch.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/rstran.h'
    include 'asterfort/titre.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vtcreb.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: nomres
    character(len=16) :: typres
    character(len=19) :: trange
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! IN  : NOMRES : NOM UTILISATEUR POUR LA COMMANDE REST_COND_TRAN
! IN  : TYPRES : TYPE DE RESULTAT : 'DYNA_TRANS'
! IN  : TRANGE : NOM UTILISATEUR DU CONCEPT TRAN_GENE AMONT
!       NOMCMD : NOM DE LA COMMANDE : 'REST_COND_TRAN'
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    complex(kind=8) :: cbid
    character(len=8) :: k8b, basemo, mailla, nomin, nomcmp(6), macrel, lintf
    character(len=8) :: nomnol, nogdsi, maya
    character(len=14) :: numddl
    character(len=16) :: concep, champ(8)
    character(len=19) :: kinst, knume, krefe, cham19
    character(len=24) :: chamno, nomcha, numedd, nprno
!      CHARACTER*3  TREDU
    logical :: lredu
    integer :: iarg
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iaconx, iadref, iadrif, iaprno, iarc0, iarch
    integer :: ibid, icmp, idbase, iddl, ie, ierd, im
    integer :: inoe, inu0, inum, iret, iretou, ivale, j
    integer :: jinst, jnocmp, jnume, jordr, jrestr, k, ldnew
    integer :: linst, lnocm2, lnocmp, lpa2, lpar, lrefe, n0
    integer :: n1, nbcham, nbec, nbinst, nbmdef, nbmdyn, nbmode
    integer :: nbndef, nbndyn, nbnoe, nbntot, nbtdyn, nec, neq
    integer :: nes, nmc
    real(kind=8) :: rbid
!-----------------------------------------------------------------------
    data nomcmp   /'DX      ','DY      ','DZ      ',&
     &               'DRX     ','DRY     ','DRZ     '/
!     ------------------------------------------------------------------
    call jemarq()
    nomin = trange
!      TYPRES = 'DYNA_TRANS'
    call gettco(nomin, concep)
!
!     --- RECUPERATION DES ENTITES DU MAILLAGE SUR LESQUELLES ---
!     ---                PORTE LA RESTITUTION                 ---
!      TOUSNO = .TRUE.
!
    lredu = .false.
!      CALL GETVTX ( ' ', 'REDUC' , 1,IARG,1, TREDU, N2 )
!      IF (TREDU.EQ.'OUI') LREDU = .TRUE.
    call getvid(' ', 'MACR_ELEM_DYNA', 1, iarg, 1,&
                macrel, nmc)
    call jeveuo(macrel//'.MAEL_REFE', 'L', iadref)
    basemo = zk24(iadref)
    call rsorac(basemo, 'LONUTI', ibid, rbid, k8b,&
                cbid, rbid, k8b, nbmode, 1,&
                ibid)
    call jeveuo(basemo//'           .REFD', 'L', iadrif)
    numedd = zk24(iadrif+3)
    call dismoi('F', 'NOM_MAILLA', numedd(1:14), 'NUME_DDL', ibid,&
                mailla, iret)
    lintf = zk24(iadrif+4)
    call jelira(jexnum(lintf//'.IDC_LINO', 1), 'LONMAX', nbnoe, k8b)
    call dismoi('F', 'NB_MODES_STA', basemo, 'RESULTAT', nbmdef,&
                k8b, ierd)
!      CALL BMNBMD(BASEMO,'DEFORMEE',NBMDEF)
    nbmdyn = nbmode-nbmdef
    call jelira(macrel//'.LINO', 'LONMAX', nbntot, k8b)
    nec = nbmode/nbntot
    nbndyn = nbmdyn/nec
    nbndef = nbntot-nbndyn
!      NBNDE2 = NBMDEF/NEC
!      CALL ASSERT(NBNDEF.EQ.NBNDE2)
    if (nbmdef .ne. 0) then
        call rsadpa(basemo, 'L', 1, 'NOEUD_CMP', nbmdyn+1,&
                    0, lnocmp, k8b)
        if (zk16(lnocmp) .eq. ' ') then
            lredu=.true.
            nec = nbmode/nbntot
            nbndyn = nbmdyn/nec
            nbndef = nbntot-nbndyn
        else
            k = 1
31          continue
            if ((k+1) .gt. nbmdef) then
                nes = k
                goto 32
            endif
            call rsadpa(basemo, 'L', 1, 'NOEUD_CMP', nbmdyn+k+1,&
                        0, lnocm2, k8b)
            if (zk16(lnocmp)(1:8) .ne. zk16(lnocm2)(1:8)) then
                nes = k
                goto 32
            else
                k=k+1
                goto 31
            endif
32          continue
            nbndef = nbmdef/nes
            nbndyn = nbntot-nbndef
            if (nbmdyn .ne. 0) then
                nec = nbmdyn/nbndyn
            else
                nec = nes
            endif
        endif
    endif
!       CREATION DU TABLEAU NOEUD-COMPOSANTE ASSOCIES AUX MODES
    call wkvect('&&MACR78.NOECMP', 'V V K8', 2*nbmode, jnocmp)
    call jeveuo(macrel//'.LINO', 'L', iaconx)
    if (lredu) then
        nbtdyn = nbntot
    else
        nbtdyn = nbndyn
        do 23 i = nbmdyn+1, nbmode
            call rsadpa(basemo, 'L', 1, 'NOEUD_CMP', i,&
                        0, lnocmp, k8b)
            zk8(jnocmp+2*i-2) = zk16(lnocmp)(1:8)
            zk8(jnocmp+2*i-1) = zk16(lnocmp)(9:16)
23      continue
    endif
!
    do 21 i = 1, nbtdyn
        call jenuno(jexnum(mailla//'.NOMNOE', zi(iaconx+i-1)), nomnol)
        do 22 j = 1, nec
            zk8(jnocmp+2*nec*(i-1)+2*j-2) = nomnol
            zk8(jnocmp+2*nec*(i-1)+2*j-1) = nomcmp(j)
22      continue
21  end do
!        CALL GETVID(' ','NUME_DDL',1,IARG,1,K8B,IBID)
!        IF (IBID.NE.0) THEN
!          CALL GETVID(' ','NUME_DDL',1,1,1,NUMEDD,IBID)
!          NUMEDD = NUMEDD(1:14)//'.NUME'
!        ENDIF
    numddl = numedd(1:14)
    call dismoi('F', 'NB_EQUA', numddl, 'NUME_DDL', neq,&
                k8b, iret)
    call wkvect('&&MACR78.BASE', 'V V R', nbmode*neq, idbase)
    call copmod(basemo, 'DEPL', neq, numddl, nbmode,&
                'R', zr(idbase), cbid)
    call getvtx(' ', 'TOUT_CHAM', 1, iarg, 0,&
                k8b, n0)
    if (n0 .ne. 0) then
        nbcham = 3
        champ(1) = 'DEPL'
        champ(2) = 'VITE'
        champ(3) = 'ACCE'
    else
        call getvtx(' ', 'NOM_CHAM', 1, iarg, 0,&
                    champ, n1)
        if (n1 .ne. 0) then
            nbcham = -n1
            call getvtx(' ', 'NOM_CHAM', 1, iarg, nbcham,&
                        champ, n1)
        else
            call u2mess('A', 'ALGORITH10_93')
            goto 9999
        endif
    endif
    knume = '&&MACR78.NUM_RANG'
    kinst = '&&MACR78.INSTANT'
    call rstran('NON', trange, ' ', 1, kinst,&
                knume, nbinst, iretou)
    if (iretou .ne. 0) then
        call u2mess('F', 'UTILITAI4_24')
    endif
    call jeexin(kinst, iret)
    if (iret .gt. 0) then
        call jeveuo(kinst, 'L', jinst)
        call jeveuo(knume, 'L', jnume)
    endif
!
    call jeexin(trange//'.ORDR', iret)
    if (iret .ne. 0) then
        call jeveuo(trange//'.ORDR', 'L', jordr)
!
    endif
!
!
!     --- CREATION DE LA SD RESULTAT ---
    call rscrsd('G', nomres, typres, nbinst)
!
    call wkvect('&&MACR78.RESTR', 'V V R', nbmode, jrestr)
    call rsexch('F', nomin, 'DEPL', 1, cham19,&
                iret)
    call dismoi('F', 'NOM_MAILLA', cham19, 'CHAMP', ibid,&
                maya, ie)
! ATTENTION MAILLA MAILLAGE DU MACRO_ELEM DE RESTITUTION
!  ET MAYA MAILLAGE DU RESULTAT SUR MODELE SOUS-STRUC-STAT
    call dismoi('F', 'NOM_GD', cham19, 'CHAMP', ibid,&
                nogdsi, ie)
    call dismoi('F', 'NB_EC', nogdsi, 'GRANDEUR', nbec,&
                k8b, ierd)
!
    call dismoi('F', 'PROF_CHNO', cham19, 'CHAMP', ibid,&
                nprno, ie)
    nprno = nprno(1:19)//'.PRNO'
    call jeveuo(jexnum(nprno, 1), 'L', iaprno)
    do 300 i = 1, nbcham
        do 310 iarc0 = 1, nbinst
            inu0 = zi(jnume+iarc0-1)
            inum = zi(jordr+inu0-1)
            iarch = iarc0-1
            call rsexch('F', nomin, champ(i)(1:4), inum, nomcha,&
                        iret)
            nomcha = nomcha(1:19)//'.VALE'
            call jeveuo(nomcha, 'L', ivale)
            do 24 im = 1, nbmode
                nomnol = zk8(jnocmp+2*im-2)
                call jenonu(jexnom(maya//'.NOMNOE', nomnol), inoe)
                if (zk8(jnocmp+2*im-1) .eq. 'DX') icmp = 1
                if (zk8(jnocmp+2*im-1) .eq. 'DY') icmp = 2
                if (zk8(jnocmp+2*im-1) .eq. 'DZ') icmp = 3
                if (zk8(jnocmp+2*im-1) .eq. 'DRX') icmp = 4
                if (zk8(jnocmp+2*im-1) .eq. 'DRY') icmp = 5
                if (zk8(jnocmp+2*im-1) .eq. 'DRZ') icmp = 6
                iddl = zi(iaprno-1+(nbec+2)*(inoe-1)+1)
                zr(jrestr+im-1) = zr(ivale+iddl-1+icmp-1)
24          continue
            call rsexch(' ', nomres, champ(i)(1:4), iarch, chamno,&
                        iret)
            call vtcreb(chamno, numedd, 'G', 'R', neq)
            call jeveuo(chamno(1:19)//'.VALE', 'E', ldnew)
            call mdgeph(neq, nbmode, zr(idbase), zr(jrestr), zr(ldnew))
            call rsnoch(nomres, champ(i)(1:4), iarch)
            if (i .eq. 1) then
                call rsadpa(nomres, 'E', 1, 'INST', iarch,&
                            0, linst, k8b)
                zr(linst) = zr(jinst+iarc0-1)
                call rsadpa(nomin, 'L', 1, 'MODELE', inum,&
                            0, lpa2, k8b)
                call rsadpa(nomres, 'E', 1, 'MODELE', iarch,&
                            0, lpar, k8b)
                zk8(lpar) = zk8(lpa2)
                call rsadpa(nomin, 'L', 1, 'CHAMPMAT', inum,&
                            0, lpa2, k8b)
                call rsadpa(nomres, 'E', 1, 'CHAMPMAT', iarch,&
                            0, lpar, k8b)
                zk8(lpar) = zk8(lpa2)
                call rsadpa(nomin, 'L', 1, 'CARAELEM', inum,&
                            0, lpa2, k8b)
                call rsadpa(nomres, 'E', 1, 'CARAELEM', iarch,&
                            0, lpar, k8b)
                zk8(lpar) = zk8(lpa2)
            endif
310      continue
300  end do
    krefe = nomres
    call wkvect(krefe//'.REFD', 'G V K24', 7, lrefe)
    zk24(lrefe ) = zk24(iadrif)
    zk24(lrefe+1) = zk24(iadrif+1)
    zk24(lrefe+2) = zk24(iadrif+2)
    zk24(lrefe+3) = numedd
    zk24(lrefe+4) = zk24(iadrif+4)
    zk24(lrefe+5) = zk24(iadrif+5)
    zk24(lrefe+6) = zk24(iadrif+6)
    call jelibe(krefe//'.REFD')
!
! --- MENAGE
!
    call jedetr('&&MACR78.NOECMP')
    call jedetr('&&MACR78.BASE')
    call jedetr('&&MACR78.NUM_RANG')
    call jedetr('&&MACR78.INSTANT')
    call jedetr('&&MACR78.RESTR')
!
    call titre()
9999  continue
!
    call jedema()
end subroutine
