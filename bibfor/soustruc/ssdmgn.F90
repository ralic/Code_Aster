subroutine ssdmgn(mag)
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
    implicit none
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getltx.h'
    include 'asterc/getvis.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/indiis.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utlisi.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: mag
! ----------------------------------------------------------------------
!     BUT:
!        - TRAITER LE MOT CLEF "DEFI_GROUP_NO"
!          DE LA COMMANDE DEFI_MAILLAGE.
!        - CREER LES OBJETS :
!            BASE GLOBALE : .GROUPENO
!
!     IN:
!        MAG : NOM DU MAILLAGE QUE L'ON DEFINIT.
!
    character(len=8) :: nomacr, nomail, kbid, mal, nosma, pref
    character(len=24) :: nomgnl, nomgng
    integer :: indi(4)
    logical :: unaun
! ----------------------------------------------------------------------
    character(len=24) :: valk(2)
    integer :: iarg
!
!-----------------------------------------------------------------------
    integer :: i1, i1noe, iadim2, iadime, iagnl, iagno, ialino
    integer :: ianmcr, iawk1, ibid, ied, igno, ii, inol
    integer :: iocc, iret, isma, kk, lgnl, lmail, longt
    integer :: lont, lpref, n, n1, n2, n3, nbgno
    integer :: nbgno2, nbgnot, nbid, nbno, nbnoex, nbsma, nocc
    integer :: nusma
!-----------------------------------------------------------------------
    call jemarq()
    call jeveuo(mag//'.DIME', 'L', iadime)
    call jeveuo(mag//'.DIME_2', 'L', iadim2)
    call jeveuo(mag//'.NOMACR', 'L', ianmcr)
    nbsma= zi(iadime-1+4)
!
!
!
!     --1 (SUR)DIMENSIONNEMENT :
!     --------------------------
    call getfac('DEFI_GROUP_NO', nocc)
!
    nbgnot=0
    lont= 0
    do 2, iocc=1,nocc
    call getvis('DEFI_GROUP_NO', 'INDEX', iocc, iarg, 4,&
                indi, n1)
    if (n1 .eq. 4) then
        unaun=.false.
    else
        call getvtx('DEFI_GROUP_NO', 'GROUP_NO_FIN', iocc, iarg, 1,&
                    kbid, n2)
        call assert(n2.ne.0)
        unaun=.true.
    endif
!
!
!     --1.1 CAS : INDEX, TOUT OU MAILLE :
!     -----------------------------------
    if (.not.unaun) then
        call getvtx('DEFI_GROUP_NO', 'TOUT', iocc, iarg, 1,&
                    kbid, n1)
        call getvtx('DEFI_GROUP_NO', 'SUPER_MAILLE', iocc, iarg, 1,&
                    nosma, n2)
        if (n2 .eq. 1) then
            call jeexin(jexnom(mag//'.SUPMAIL', nosma), iret)
            if (iret .eq. 0) then
                valk(1) = nosma
                valk(2) = mag
                call u2mesk('F', 'SOUSTRUC_26', 2, valk)
            endif
            call jenonu(jexnom(mag//'.SUPMAIL', nosma), nusma)
        endif
!
        do 21, isma=1,nbsma
        if ((n2.eq.1) .and. (nusma.ne.isma)) goto 21
        nomacr= zk8(ianmcr-1+isma)
        call dismoi('F', 'NOM_MAILLA', nomacr, 'MACR_ELEM_STAT', ibid,&
                    mal, ied)
        call jeexin(mal//'.GROUPENO', iret)
        if (iret .eq. 0) goto 21
        call jelira(mal//'.GROUPENO', 'NUTIOC', nbgno, kbid)
        nbgnot= nbgnot+nbgno
        do 211, igno=1,nbgno
        call jelira(jexnum(mal//'.GROUPENO', igno), 'LONMAX', n3, kbid)
        lont= lont+n3
211      continue
21      continue
!
!
!     --3.2 CAS : MAILLE, GROUP_NO_FIN, GROUP_NO_INIT:
!     -----------------------------------------------
    else
        call getvtx('DEFI_GROUP_NO', 'SUPER_MAILLE', iocc, iarg, 1,&
                    nosma, n1)
        call getvtx('DEFI_GROUP_NO', 'GROUP_NO_INIT', iocc, iarg, 1,&
                    nomgnl, n)
!
        call jenonu(jexnom(mag//'.SUPMAIL', nosma), isma)
        nomacr= zk8(ianmcr-1+isma)
        call dismoi('F', 'NOM_MAILLA', nomacr, 'MACR_ELEM_STAT', ibid,&
                    mal, ied)
        call jelira(jexnom(mal//'.GROUPENO', nomgnl), 'LONUTI', n3, kbid)
        nbgnot= nbgnot+1
        lont=lont+n3
    endif
    2 end do
!
!     -- SI LONT = 0 ON S'ARRETE LA. (PLUS BAS : BUG ????)
    if ((lont.eq.0) .or. (nbgnot.eq.0)) goto 9999
!
!
!
!
!     --2 ALLOCATION:
!     ---------------
!     --ON SURDIMENSIONNE LE NOMBRE MAX D'OBJETS DE LA COLLECTION
!       DISPERSEE .GROUPENO :
    nbgno2= 2*nbgnot+20
    call jecrec(mag//'.GROUPENO', 'G V I', 'NOM', 'DISPERSE', 'VARIABLE',&
                nbgno2)
!
!     -- SI LONT = 0 ON S'ARRETE LA.
!     IF ((LONT.EQ.0).OR.(NBGNOT.EQ.0)) GOTO 9999
    call wkvect('&&SSDMGN.WORK1', 'V V I', lont, iawk1)
!
!
!     --3 REMPLISSAGE:
!     ----------------
    do 5, iocc=1,nocc
    unaun=.true.
    call getvis('DEFI_GROUP_NO', 'INDEX', iocc, iarg, 4,&
                indi, n1)
    if (n1 .eq. 4) unaun=.false.
!
!
!       --3.1 CAS : INDEX, TOUT OU MAILLE :
!       -----------------------------------
    if (.not.unaun) then
        call getvtx('DEFI_GROUP_NO', 'TOUT', iocc, iarg, 1,&
                    kbid, n1)
        call getvtx('DEFI_GROUP_NO', 'SUPER_MAILLE', iocc, iarg, 1,&
                    nosma, n2)
        if (n2 .eq. 1) call jenonu(jexnom(mag//'.SUPMAIL', nosma), nusma)
        lpref=0
        call getltx('DEFI_GROUP_NO', 'PREFIXE', iocc, 8, 1,&
                    lpref, nbid)
        call getvis('DEFI_GROUP_NO', 'INDEX', iocc, iarg, 4,&
                    indi, n3)
        lmail=indi(2)-indi(1)+1
        lgnl=indi(4)-indi(3)+1
        lmail=max(lmail,0)
        lgnl=max(lgnl,0)
        longt= lpref+lmail+lgnl
        if (longt .gt. 8) call u2mess('F', 'SOUSTRUC_61')
        if (lpref .gt. 0) call getvtx('DEFI_GROUP_NO', 'PREFIXE', iocc, iarg, 1,&
                                      pref, nbid)
!
        do 51, isma=1,nbsma
        if ((n2.eq.1) .and. (nusma.ne.isma)) goto 51
        nomacr= zk8(ianmcr-1+isma)
        call jenuno(jexnum(mag//'.SUPMAIL', isma), nomail)
        i1noe=zi(iadim2-1+4*(isma-1)+3)
        call jeveuo(nomacr//'.LINO', 'L', ialino)
        call jelira(nomacr//'.LINO', 'LONUTI', nbnoex, kbid)
        call dismoi('F', 'NOM_MAILLA', nomacr, 'MACR_ELEM_STAT', ibid,&
                    mal, ied)
        call jeexin(mal//'.GROUPENO', iret)
        if (iret .eq. 0) then
            nbgno=0
        else
            call jelira(mal//'.GROUPENO', 'NUTIOC', nbgno, kbid)
        endif
        do 511, igno=1,nbgno
        call jelira(jexnum(mal//'.GROUPENO', igno), 'LONMAX', n3, kbid)
        call jeveuo(jexnum(mal//'.GROUPENO', igno), 'L', iagnl)
        call utlisi('INTER', zi(ialino), nbnoex, zi(iagnl), n3,&
                    zi(iawk1), lont, nbno)
!
!
        if (nbno .gt. 0) then
!
!               --3.1.1 CALCUL DE NOMGNG:
!               -------------------------
            call jenuno(jexnum(mal//'.GROUPENO', igno), nomgnl)
            i1=1
            if (lpref .gt. 0) nomgng(i1:i1-1+lpref) = pref( 1:lpref)
            i1= i1+lpref
            if (lmail .gt. 0) nomgng(i1:i1-1+lmail) = nomail(indi(1):indi(2))
            i1= i1+lmail
            if (lgnl .gt. 0) nomgng(i1:i1-1+lgnl) = nomgnl( indi(3):indi(4))
!
!               --3.1.2 RECOPIE DES NUMEROS DE NOEUDS:
!               --------------------------------------
            call jecroc(jexnom(mag//'.GROUPENO', nomgng))
            call jeecra(jexnom(mag//'.GROUPENO', nomgng), 'LONMAX', nbno, kbid)
            call jeecra(jexnom(mag//'.GROUPENO', nomgng), 'LONUTI', nbno, kbid)
            call jeveuo(jexnom(mag//'.GROUPENO', nomgng), 'E', iagno)
            do 5112,ii=1,nbno
            inol=zi(iawk1-1+ii)
            kk= indiis(zi(ialino),inol,1,nbnoex)
            if (kk .eq. 0) call assert(.false.)
            zi(iagno-1+ii)=i1noe+kk
!
5112          continue
        endif
511      continue
51      continue
!
!
!       --3.2 CAS : MAILLE, GROUP_NO_FIN, GROUP_NO_INIT:
!       -----------------------------------------------
    else
        call getvtx('DEFI_GROUP_NO', 'SUPER_MAILLE', iocc, iarg, 1,&
                    nosma, n1)
        call getvtx('DEFI_GROUP_NO', 'GROUP_NO_INIT', iocc, iarg, 1,&
                    nomgnl, n)
        call getvtx('DEFI_GROUP_NO', 'GROUP_NO_FIN', iocc, iarg, 1,&
                    nomgng, n)
!
        call jenonu(jexnom(mag//'.SUPMAIL', nosma), isma)
        i1noe=zi(iadim2-1+4*(isma-1)+3)
        nomacr= zk8(ianmcr-1+isma)
        call jeveuo(nomacr//'.LINO', 'L', ialino)
        call jelira(nomacr//'.LINO', 'LONUTI', nbnoex, kbid)
        call dismoi('F', 'NOM_MAILLA', nomacr, 'MACR_ELEM_STAT', ibid,&
                    mal, ied)
        call jelira(jexnom(mal//'.GROUPENO', nomgnl), 'LONUTI', n3, kbid)
        call jeveuo(jexnom(mal//'.GROUPENO', nomgnl), 'L', iagnl)
        call utlisi('INTER', zi(ialino), nbnoex, zi(iagnl), n3,&
                    zi(iawk1), lont, nbno)
!
        if (nbno .gt. 0) then
            call jecroc(jexnom(mag//'.GROUPENO', nomgng))
            call jeecra(jexnom(mag//'.GROUPENO', nomgng), 'LONMAX', nbno, kbid)
            call jeecra(jexnom(mag//'.GROUPENO', nomgng), 'LONUTI', nbno, kbid)
            call jeveuo(jexnom(mag//'.GROUPENO', nomgng), 'E', iagno)
            do 52,ii=1,nbno
            inol=zi(iawk1-1+ii)
            kk= indiis(zi(ialino),inol,1,nbnoex)
            if (kk .eq. 0) call assert(.false.)
            zi(iagno-1+ii)=i1noe+kk
52          continue
        else
            call u2mesk('A', 'SOUSTRUC_62', 1, nomgng)
        endif
    endif
    5 end do
!
!
    call jedetr('&&SSDMGN.WORK1')
!
9999  continue
    call jedema()
end subroutine
