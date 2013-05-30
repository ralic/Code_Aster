subroutine irdeca(ifi, nbno, prno, nueq, nec,&
                  dg, ncmpmx, vale, nomgd, ncmpgd,&
                  nomsym, numnoe, lresu, nbcput, ncmput,&
                  nive)
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/lxliis.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    integer :: ifi, nbno, prno(*), nueq(*), nec, dg(*), ncmpmx, numnoe(*)
    integer :: nbcput, nive
    real(kind=8) :: vale(*)
    character(len=*) :: nomgd, ncmpgd(*), ncmput(*)
    character(len=*) :: nomsym
    logical :: lresu
!
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
!        ECRITURE D'UN CHAM_NO SUR FICHIER CASTEM, DATASET TYPE 55
!        A VALEURS REELLES
!      ENTREE:
!         IFI   : UNITE LOGIQUE DU FICHIER CASTEM
!         NBNO  : NOMBRE DE NOEUDS DU MAILLAGE
!         PRNO  : OBJET .PRNO DU PROF_CHNO
!         NUEQ  : OBJET .NUEQ DU PROF_CHNO
!         NEC   : NOMBRE D'ENTIERS-CODES
!         DG    : ENTIERS CODES
!         NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR NOMGD
!         VALE  : VALEURS DU CHAM_NO
!         NOMGD : NOM DE LA GRANDEUR  DEPL_R, TEMP_R, SIEF_R, EPSI_R,...
!         NCMPGD: NOMS DES CMP DE LA GRANDEUR
!         NOMSYM: NOM SYMBOLIQUE
!         NUMNOE: NUMERO DES NOEUDS
!         LRESU : =.TRUE. IMPRESSION D4UN CONCEPT RESULTAT
!         NBCPUT: NOMBRE DE CMP DEMANDE PAR L'UTILISATEUR
!         NCMPUT: NOMS DES CMP DEMANDE PAR L'UTILISATEUR
!         NIVE  : NIVEAU IMPRESSION CASTEM 3 OU 10
!
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
    character(len=8) :: nomvar(30)
    character(len=8) :: blanc, nomco
    integer :: nbcmp, ipcmp(30), iutil, iltabl
!      LOGICAL       LTABL(180)
!
!  --- INITIALISATIONS ----
!
!-----------------------------------------------------------------------
    integer :: i, iad, iadr, ibi, ibid, ic, icm
    integer :: icmc, icmcca, icmp, icompt, iec, inno, ino
    integer :: inom, inum, iret, irval, iun, ival, ivari
    integer :: izero, jlast, ncmp
!-----------------------------------------------------------------------
    call jemarq()
!
!      IF (NCMPMX.GT.180) CALL U2MESS('F','PREPOST2_18')
    call wkvect('&&IRDECA.LTABL', 'V V L', ncmpmx, iltabl)
!
    if (.not.lresu) then
        call jeveuo('&&OP0039.LAST', 'E', jlast)
        inum = zi(jlast-1+4) + 1
    else
        inum = 0
    endif
    do 1 i = 1, ncmpmx
        zl(iltabl+i-1)=.false.
 1  end do
    iad = 1
    nbcmp = 0
    blanc = '      '
!
! RECHERCHE DU NOMBRE DE SOUS OBJETS ET DES COMPOSANTES ASSOCIEES
!
    do 2 inno = 1, nbno
        ino = numnoe(inno)
        do 3 iec = 1, nec
            dg(iec)=prno((ino-1)*(nec+2)+2+iec)
 3      continue
!
!        NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
!        IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
!
        ival = prno((ino-1)* (nec+2)+1)
        ncmp = prno((ino-1)* (nec+2)+2)
        if (ncmp .eq. 0) goto 2
!
        if (nbcput .ne. 0) then
            do 30 icm = 1, nbcput
                if (nomgd .eq. 'VARI_R') then
                    call lxliis(ncmput(icm)(2:8), ivari, iret)
                    if ((ncmput(icm)(1:1).ne.'V') .or. (iret.ne.0)) then
                        valk (1) = ncmput(icm)
                        valk (2) = 'VARI_R'
                        call u2mesk('F', 'CALCULEL6_49', 2, valk)
                    endif
                    zl(iltabl+ivari-1)= .true.
                    goto 30
                else
                    do 32 icmp = 1, ncmpmx
                        if (ncmput(icm) .eq. ncmpgd(icmp)) then
                            zl(iltabl+icmp-1)= .true.
                            goto 30
                        endif
32                  continue
                endif
                valk (1) = ncmput(icm)
                valk (2) = nomgd
                call u2mesg('A', 'PREPOST5_25', 2, valk, 0,&
                            0, 0, 0.d0)
30          continue
        else
            do 4 icmp = 1, ncmpmx
                if (exisdg(dg,icmp)) zl(iltabl+icmp-1)= .true.
 4          continue
        endif
 2  end do
!
    do 5 i = 1, ncmpmx
        if (zl(iltabl+i-1)) then
            if (ncmpgd(i) .eq. 'DX') then
                nbcmp = nbcmp+1
                nomvar(iad)='UX'
                ipcmp(iad)= i
                iad = iad+1
            else if (ncmpgd(i).eq.'DY') then
                nbcmp= nbcmp+1
                nomvar(iad)='UY'
                ipcmp(iad)= i
                iad = iad+1
            else if (ncmpgd(i).eq.'DZ') then
                nbcmp= nbcmp+1
                nomvar(iad)='UZ'
                ipcmp(iad)= i
                iad = iad+1
            else if (ncmpgd(i).eq.'DRX') then
                nbcmp= nbcmp+1
                nomvar(iad)='RX'
                ipcmp(iad)= i
                iad = iad+1
            else if (ncmpgd(i).eq.'DRY') then
                nbcmp= nbcmp+1
                nomvar(iad)='RY'
                ipcmp(iad)= i
                iad = iad+1
            else if (ncmpgd(i).eq.'DRZ') then
                nbcmp= nbcmp+1
                nomvar(iad)='RZ'
                ipcmp(iad)= i
                iad = iad+1
            else
                nbcmp = nbcmp+1
                nomco = ncmpgd(i)
                iutil = lxlgut(nomco)
                if (iutil .le. 4) then
                    nomvar(iad) = nomco
                else
                    nomvar(iad) = nomco(1:2) //nomco((iutil-1):iutil)
                endif
                ipcmp(iad)= i
                iad = iad+1
            endif
        endif
 5  end do
!
! ---- ECRITURE DE L'EN-TETE -----
!
    ibid = 2
    iun = 1
    izero = 0
    write (ifi,'(A,I4)')   ' ENREGISTREMENT DE TYPE',ibid
    if (lresu) then
        if (nive .eq. 3) then
            write (ifi,'(A,I4,A,I5,A,I5)') ' PILE NUMERO',ibid,&
            'NBRE OBJETS NOMMES',izero,'NBRE OBJETS',iun
        else if (nive.eq.10) then
            write (ifi,'(A,I4,A,I8,A,I8)') ' PILE NUMERO',ibid,&
            'NBRE OBJETS NOMMES',izero,'NBRE OBJETS',iun
        endif
    else
        if (nive .eq. 3) then
            write (ifi,'(A,I4,A,I5,A,I5)') ' PILE NUMERO',ibid,&
            'NBRE OBJETS NOMMES',iun,'NBRE OBJETS',iun
        else if (nive.eq.10) then
            write (ifi,'(A,I4,A,I8,A,I8)') ' PILE NUMERO',ibid,&
            'NBRE OBJETS NOMMES',iun,'NBRE OBJETS',iun
        endif
! ECRITURE DES OBJETS NOMMES
        write(ifi,'(1X,A8)') nomsym
        if (nive .eq. 3) write(ifi,'(I5)') inum
        if (nive .eq. 10) write(ifi,'(I8)') inum
    endif
!
    if (nive .eq. 3) write(ifi,'(I5,I5,I5)') iun,nbcmp,ibid
    if (nive .eq. 10) write(ifi,'(I8,I8,I8,I8)') iun,nbcmp,ibid,iun
    call wkvect('&&IRDECA.BID', 'V V I', ncmpmx, ibi)
    call wkvect('&&IRDECA.NOM', 'V V K8', nbcmp, inom)
    if (nive .eq. 3) write(ifi,'(16(I5))') iun,nbno,nbcmp
    if (nive .eq. 10) write(ifi,'(10(I8))') iun,nbno,nbcmp
    izero = 0
    do 10 i = 1, nbcmp
        zi(ibi-1+i) = izero
        zk8(inom-1+i) = nomvar(i)
10  end do
    write(ifi,'(16(1X,A4))') (zk8(inom-1+i)(1:4),i=1,nbcmp)
    if (nive .eq. 3) write(ifi,'(16(I5))') (zi(ibi-1+i),i=1,nbcmp)
    if (nive .eq. 10) write(ifi,'(10(I8))') (zi(ibi-1+i),i=1,nbcmp)
    if (lresu) then
        write(ifi,'(1X,A71)') nomsym
    else
        write(ifi,'(1X,A71)') nomgd
    endif
    write(ifi,'(1X,A)') blanc
    if (nive .eq. 10) write(ifi, '(I8)') izero
!
! --- ALLOCATION DES TABLEAUX DE TRAVAIL ---
!
    call wkvect('&&IRDECA.VALE', 'V V R', ncmpmx*nbno, irval)
!
! ---- BOUCLE SUR LES DIVERSES GRANDEURS CASTEM ----
    do 25 ic = 1, ncmpmx*nbno
        zr(irval-1+ic) = 0.0d0
25  end do
    iadr = irval - 1
    do 12 inno = 1, nbno
        ino = numnoe(inno)
        do 17 iec = 1, nec
            dg(iec)=prno((ino-1)*(nec+2)+2+iec)
17      continue
        ival = prno((ino-1)* (nec+2)+1)
        ncmp = prno((ino-1)* (nec+2)+2)
        if (ncmp .eq. 0) goto 12
!
        icompt = 0
        do 14 icmp = 1, ncmpmx
            if (exisdg(dg,icmp)) then
                icompt = icompt+1
                do 13 icmc = 1, nbcmp
                    icmcca = ipcmp (icmc)
                    if (icmp .eq. icmcca) then
                        iadr = irval-1+(icmc-1)*nbno
                        zr(iadr+ino) = vale(nueq(ival-1+icompt))
                        goto 14
                    endif
13              continue
            endif
14      continue
12  end do
    write(ifi,'(3(1X,E21.13E3))') (zr(irval-1+i),i=1,nbcmp*nbno)
    if (.not.lresu) zi(jlast-1+4)=inum
    call jedetr('&&IRDECA.VALE')
    call jedetr('&&IRDECA.BID')
    call jedetr('&&IRDECA.NOM')
    call jedetr('&&IRDECA.LTABL')
    call jedema()
end subroutine
