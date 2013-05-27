subroutine utchdl(cham19, nomma, nomail, nonoeu, nupo,&
                  nusp, ivari, nocmp1, iddl)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/indik8.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/indiis.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nbec.h'
    include 'asterfort/numel2.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: nupo, ivari, iddl, nusp
    character(len=*) :: cham19, nomma, nomail, nonoeu, nocmp1
! ----------------------------------------------------------------------
!
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: jacques.pellet at edf.fr
! A_UTILI
!
! BUT: RECUPERER UN NUMERO DE DDL DANS UN CHAM_ELEM
! ----------------------------------------------------------------------
! IN  : CHAM19 : NOM DU CHAM_ELEM
! IN  : NOMMA  : NOM DU MAILLAGE
! IN  : NOMAIL : NOM DE LA MAILLE A EXTRAIRE
! IN  : NONOEU : NOM D'UN NOEUD (POUR LES CHAM_ELEM "AUX NOEUDS").
! IN  : NUPO   : NUMERO D'UN POINT (POUR LES CHAM_ELEM "GAUSS").
! IN  : NUSP   : NUMERO DU SOUS_POINT A TESTER SUR LA MAILLE NOMAIL
!                (SI NUSP=0 : IL N'Y A PAS DE SOUS-POINT)
! IN  : NOCMP1 : NOM DU DDL A EXTRAIRE SUR LE POINT CHERCHE
! IN  : IVARI  : NUMERO DE LA CMP (POUR VARI_R)
! OUT : IDDL   : NUMERO DU DDL DANS LE .CELV
!   CONVENTION : IDDL=0 -> ON N'A PAS TROUVE LE DDL CHERCHE
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: ibid, gd, incmp
    integer :: vali(2)
    integer :: iacelk, jceld, nec, icmp, ncmpmx, iancmp, ima
    integer :: ino, iaconx, nbno, ipo, nupo2, igr, iel
    integer :: imolo, jmolo, ispt, jlpt, jlcupt, nbpt, ipt, ico
    integer :: k, iadg, kcmp, cumu, nbspt, adiel, lgcata, ncdyn
    character(len=1) :: aof
    character(len=24) :: valk(2)
    character(len=8) :: k8b, nocmp, nomaiz, nonoez, nommaz, nomgd
    character(len=16) :: nomcmd
    character(len=19) :: noligr, chm19z, ncmp
    logical :: diff, trouve, nogran
!     ------------------------------------------------------------------
!
    call jemarq()
    iddl = 0
!
    call getres(k8b, k8b, nomcmd)
    if (nomcmd .eq. 'TEST_RESU') then
        aof='A'
    else
        aof='F'
    endif
!
    chm19z = cham19
    nomaiz = nomail(1:8)
    nonoez = nonoeu(1:8)
    nommaz = nomma(1:8)
    ncmp = '&&UTCHDL.N_CMP'
    call jeveuo(chm19z//'.CELK', 'L', iacelk)
    noligr = zk24(iacelk) (1:19)
    trouve = .false.
    nogran = .false.
!
!
!
!     1. ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
!     ---------------------------------------------------------
!     CALL CELVER(CHM19Z,'NBSPT_1','STOP',IBID)
!
!
    call jeveuo(chm19z//'.CELD', 'L', jceld)
    gd = zi(jceld)
    call jenuno(jexnum('&CATA.GD.NOMGD', gd), nomgd)
    nec = nbec(gd)
!
!
!     2. ON RECHERCHE LE NUMERO DE LA CMP CHERCHEE : ICMP
!     -------------------------------------------------------
    nocmp = nocmp1
    if (nomgd .eq. 'VARI_R') then
        nocmp = 'VARI'
        icmp = ivari
        if (icmp .le. 0) icmp = 0
    else
        call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx, k8b)
        call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iancmp)
        icmp = indik8(zk8(iancmp),nocmp,1,ncmpmx)
        call wkvect(ncmp, 'V V K8', ncmpmx, incmp)
!
    endif
    if (icmp .eq. 0) then
        valk(1) = nocmp
        valk(2) = nomgd
        call u2mesk(aof, 'UTILITAI5_30', 2, valk)
        iddl=0
        goto 9999
    endif
!
!
!     3. ON VERIFIE LA MAILLE : IMA
!     -----------------------------
    call jenonu(jexnom(nommaz//'.NOMMAI', nomaiz), ima)
    if (ima .le. 0) then
        valk(1) = nomaiz
        valk(2) = nommaz
        call u2mesk(aof, 'UTILITAI5_31', 2, valk)
        iddl=0
        goto 9999
    endif
!
!
!     4. ON VERIFIE LE NOEUD : CALCUL DE NUPO2
!     ------------------------------------------
    if (nonoeu(1:1) .ne. ' ') then
        call dismoi('C', 'TYPE_CHAMP', chm19z, 'CHAMP', ibid,&
                    k8b, ibid)
        if (k8b(1:4) .ne. 'ELNO') then
            call u2mesk(aof, 'UTILITAI5_32', 1, chm19z)
        endif
        call jenonu(jexnom(nommaz//'.NOMNOE', nonoez), ino)
        if (ino .le. 0) then
            valk(1) = nonoez
            valk(2) = nommaz
            call u2mesk(aof, 'ALGORITH_21', 2, valk)
        endif
!        -- ON CHERCHE LE "IPO" CORRESPONDANT A INO:
        call jeveuo(jexnum(nommaz//'.CONNEX', ima), 'L', iaconx)
        call jelira(jexnum(nommaz//'.CONNEX', ima), 'LONMAX', nbno, k8b)
        ipo = indiis(zi(iaconx),ino,1,nbno)
        if (ipo .le. 0) then
            valk(1) = nonoez
            valk(2) = nomaiz
            call u2mesk(aof, 'SOUSTRUC_59', 2, valk)
        endif
        nupo2 = ipo
    else
        nupo2 = nupo
    endif
!
!
!     5. CALCUL DE IGR ET IEL :
!     ------------------------------------------
    call numel2(chm19z, ima, igr, iel)
    if ((igr.le.0) .or. (iel.le.0)) then
        valk(1) = nomaiz
        valk(2) = noligr
        call u2mesk(aof, 'UTILITAI5_34', 2, valk)
    endif
    nbspt = zi(jceld-1+zi(jceld-1+4+igr)+4+4* (iel-1)+1)
    adiel = zi(jceld-1+zi(jceld-1+4+igr)+4+4* (iel-1)+4)
    ncdyn = zi(jceld-1+zi(jceld-1+4+igr)+4+4* (iel-1)+2)
!
!
!
!     6. CALCUL DE IDDL :
!     ------------------------------------------
    imolo = zi(jceld-1+zi(jceld-1+4+igr)+2)
    if (imolo .le. 0) then
        call u2mesk(aof, 'UTILITAI5_35', 1, nomaiz)
    endif
    call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
!
!
!     -- NUMERO DE SOUS_POINT :
!     ------------------------
    if (nusp .eq. 0) then
        ispt = 1
    else
        ispt = nusp
    endif
    if (ispt .gt. nbspt) then
        call u2mess(aof, 'UTILITAI5_36')
        iddl=0
        goto 9999
    endif
    if ((nusp.eq.0) .and. (nbspt.gt.1)) then
        call u2mesi(aof, 'CALCULEL_1', 1, nbspt)
        iddl=0
        goto 9999
    endif
!
!
!     6.1 CAS : NOMGD /= VARI_R :
!     ----------------------------
    if (nomgd .ne. 'VARI_R') then
        diff = (zi(jmolo-1+4).gt.10000)
        nbpt = mod(zi(jmolo-1+4),10000)
!
!       -- SI CHAM_ELEM / ELEM (NBPT=1) ET QUE L'ON NA PAS PRECISE NUPO
!          ON PREND NUPO2=1
        if ((nupo2.eq.0) .and. (nbpt.eq.1)) nupo2=1
!
        if (nupo2 .gt. nbpt) then
            vali(1)=nupo2
            vali(2)=nbpt
            call u2mesi(aof, 'UTILITAI5_37', 2, vali)
            iddl=0
            goto 9999
        endif
        call wkvect('&&UTCHDL.LONG_PT', 'V V I', nbpt, jlpt)
        call wkvect('&&UTCHDL.LONG_PT_CUMU', 'V V I', nbpt, jlcupt)
!
!         -- CALCUL DU NOMBRE DE CMPS POUR CHAQUE POINT
!            ET DU CUMUL SUR LES POINTS PRECEDENTS :
        do 20,ipt = 1,nbpt
        ico = 0
        k = 1
        if (diff) k = ipt
        iadg = jmolo - 1 + 4 + (k-1)*nec + 1
        do 10,kcmp = 1,ncmpmx
        if (exisdg(zi(iadg),kcmp)) then
            ico = ico + 1
            zk8(incmp+ico-1) = zk8(iancmp+kcmp-1)
            if (nocmp .eq. zk8(incmp+ico-1)) trouve = .true.
        endif
10      continue
        zi(jlpt-1+ipt) = ico
20      continue
        if ((.not.trouve) .and. (.not.nogran)) then
            call u2mesk(aof, 'UTILITAI5_38', 1, nocmp)
        endif
!
!
!
        cumu = 0
        do 30,ipt = 1,nbpt
        zi(jlcupt-1+ipt) = cumu
        cumu = cumu + zi(jlpt-1+ipt)
30      continue
!
!
        do 50,ipt = 1,nbpt
        k = 1
        if (diff) k = ipt
        iadg = jmolo - 1 + 4 + (k-1)*nec + 1
        ico = 0
        do 40,kcmp = 1,ncmpmx
        if (exisdg(zi(iadg),kcmp)) then
            ico = ico + 1
!
!
            iddl = adiel - 1 + nbspt*zi(jlcupt-1+ipt) + (ispt-1)*zi(jlpt-1+ipt) + ico
            if ((ipt.eq.nupo2) .and. (kcmp.eq.icmp)) goto 60
!
        endif
40      continue
50      continue
!       -- ON N'A PAS TROUVE LE POINT LE SOUS-POINT OU LA COMPOSANTE :
        iddl=0
        goto 9999
60      continue
!
!
!     6.2 CAS : NOMGD = VARI_R :
!     ----------------------------
    else
        lgcata = zi(jceld-1+zi(jceld-1+4+igr)+3)
! LE CAS ZI(JMOLO-1+4).GT.10000
! N EST PAS PREVU : REALISER L EVOLUTION
        call assert(zi(jmolo-1+4).le.10000)
        nbpt = mod(zi(jmolo-1+4),10000)
        call assert(nbpt.eq.lgcata)
!
        ipt = nupo2
!
        if (icmp .gt. ncdyn) then
            valk(1) = nomaiz
            vali (1) = ncdyn
            vali (2) = icmp
            call u2mesg(aof, 'UTILITAI7_5', 1, valk, 2,&
                        vali, 0, 0.d0)
            iddl=0
            goto 9999
        else
!
            if ((ispt.le.nbspt) .and. (ipt.le.nbpt)) then
                iddl = adiel - 1 + ((ipt-1)*nbspt+ispt-1)*ncdyn + icmp
            else
                call assert(.false.)
            endif
        endif
    endif
!
!
9999  continue
    call jedetr('&&UTCHDL.LONG_PT')
    call jedetr('&&UTCHDL.LONG_PT_CUMU')
    call jedetr('&&UTCHDL.N_CMP')
!
    call jedema()
end subroutine
