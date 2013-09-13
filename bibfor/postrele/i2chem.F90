subroutine i2chem(nomail, nbparm)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/i2extf.h"
#include "asterfort/i2imam.h"
#include "asterfort/i2rdli.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utnono.h"
#include "asterfort/wkvect.h"
!
    integer :: nbparm
    character(len=8) :: nomail
! ----------------------------------------------------------------------
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
!
!     OPERATEUR INTE_MAIL_2D, MOT CLE FACTEUR "DEFI_CHEMIN"
!
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: n1, n2, nbtnoe, ifm, niv, ier, nbtm, occ, im, ig, jgrm1, jgrm2
    integer :: existe, numm, jtypm, nbm, jgrma, jmail, in, mi
    integer :: libr1, alstot, jnoe, nig, nid, kmail1, kmail2
    integer :: astrct, jmail1, jmail2, jmail3, aptstr, nbcnx, kchm, sgcour
    integer :: ktypcb, knomma, chm, debchm, finchm, ind, iatyma
    integer :: numse, numm1, numm2, ibid, numno, iret, ideb, trouve
    integer :: vali(7)
    real(kind=8) :: epsi
    logical :: ouvert
    character(len=8) :: k8b, nomcrb, typm, nomma, nomse, nomm1, nomm2
    character(len=16) :: typcrb, opera
    character(len=24) :: conec, type, nommai, nomnoe, noeud, nomgr
    character(len=24) :: grpmai, nchmin, nmail1, nmail2, ntpcrb, nnomma
    character(len=24) :: valk(7)
!     ------------------------------------------------------------------
    call jemarq()
!
    call infniv(ifm, niv)
!
    call getres(nomcrb, typcrb, opera)
    call getvr8(' ', 'PRECISION', scal=epsi, nbret=n2)
!
!----------------------------------------------------------------------
!
!                     D E F I _ C H E M I N
!
!----------------------------------------------------------------------
    conec = nomail//'.CONNEX         '
    type = nomail//'.TYPMAIL        '
    nommai = nomail//'.NOMMAI         '
    nomnoe = nomail//'.NOMNOE         '
    grpmai = nomail//'.GROUPEMA       '
    call dismoi('F', 'NB_NO_MAILLA', nomail, 'MAILLAGE', nbtnoe,&
                k8b, ier)
!
    ier = 0
    nbtm = 0
    do 40,occ = 1,nbparm,1
    call getvtx('DEFI_CHEMIN', 'MAILLE', iocc=occ, nbval=0, nbret=n1)
    call getvtx('DEFI_CHEMIN', 'GROUP_MA', iocc=occ, nbval=0, nbret=n2)
    if (n1 .ne. 0) then
        n1 = -n1
        call wkvect('&&OP0050.MAILLE', 'V V K8', n1, jmail)
        call getvtx('DEFI_CHEMIN', 'MAILLE', iocc=occ, nbval=n1, vect=zk8(jmail))
        do 10,im = 1,n1,1
        nomma = zk8(jmail+im-1)
        call jeexin(jexnom(nommai, nomma), existe)
        if (existe .eq. 0) then
            ier = ier + 1
            valk (1) = 'DEFI_CHEMIN'
            valk (2) = nomma
            call u2mesg('E', 'INTEMAIL_12', 2, valk, 1,&
                        occ, 0, 0.d0)
        else
            call jenonu(jexnom(nomail//'.NOMMAI', nomma), ibid)
            call jeveuo(type, 'L', iatyma)
            jtypm = iatyma - 1 + ibid
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtypm)), typm)
            if (typm .ne. 'SEG2' .and. typm .ne. 'SEG3') then
                ier = ier + 1
                valk (1) = 'DEFI_CHEMIN'
                valk (2) = nomma
                call u2mesg('E', 'INTEMAIL_15', 2, valk, 1,&
                            occ, 0, 0.d0)
            endif
        endif
10      continue
        nbtm = nbtm + n1
        call jedetr('&&OP0050.MAILLE')
    endif
    if (n2 .ne. 0) then
        n2 = -n2
        call wkvect('&&OP0050.GROUP_MA', 'V V K24', n2, jgrma)
        call getvtx('DEFI_CHEMIN', 'GROUP_MA', iocc=occ, nbval=n2, vect=zk24(jgrma))
        do 30,ig = 1,n2,1
        nomgr = zk24(jgrma+ig-1)
        call jenonu(jexnom(grpmai, nomgr), existe)
        if (existe .eq. 0) then
            ier = ier + 1
            valk (1) = 'DEFI_CHEMIN'
            valk (2) = nomgr
            call u2mesg('E', 'INTEMAIL_16', 2, valk, 1,&
                        occ, 0, 0.d0)
        else
            call jelira(jexnom(grpmai, nomgr), 'LONUTI', nbm)
            call jeveuo(jexnom(grpmai, nomgr), 'L', jgrm1)
            do 20,im = 1,nbm,1
            call jeveuo(type, 'L', iatyma)
            jtypm = iatyma - 1 + zi(jgrm1+im-1)
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtypm)), typm)
            if (typm .ne. 'SEG2' .and. typm .ne. 'SEG3') then
                ier = ier + 1
                valk (1) = 'DEFI_CHEMIN'
                valk (2) = nomgr
                call u2mesg('E', 'INTEMAIL_13', 2, valk, 1,&
                            occ, 0, 0.d0)
            endif
20          continue
            nbtm = nbtm + nbm
        endif
30      continue
        call jedetr('&&OP0050.GROUP_MA')
    endif
    40 end do
!
    if (ier .gt. 0) call u2mess('F', 'INTEMAIL_14')
!
    call wkvect('&INTLISTOTAL', 'V V I', nbtm, alstot)
    libr1 = 1
    do 80,occ = 1,nbparm,1
    call getvtx('DEFI_CHEMIN', 'MAILLE', iocc=occ, nbval=0, nbret=n1)
    call getvtx('DEFI_CHEMIN', 'GROUP_MA', iocc=occ, nbval=0, nbret=n2)
    if (n1 .ne. 0) then
        n1 = -n1
        call wkvect('&&OP0050.MAILLE', 'V V K8', n1, jmail3)
        call getvtx('DEFI_CHEMIN', 'MAILLE', iocc=occ, nbval=n1, vect=zk8(jmail3),&
                    nbret=n2)
        do 50,im = 1,n1,1
        call jenonu(jexnom(nommai, zk8(jmail3+im-1)), numm)
        call i2rdli(numm, zi(alstot), libr1)
50      continue
        call jedetr('&&OP0050.MAILLE')
    else
        n2 = -n2
        call wkvect('&&OP0050.GROUP_MA', 'V V K24', n2, jgrma)
        call getvtx('DEFI_CHEMIN', 'GROUP_MA', iocc=occ, nbval=n2, vect=zk24(jgrma))
        do 70,ig = 1,n2,1
        nomgr = zk24(jgrma+ig-1)
        call jelira(jexnom(grpmai, nomgr), 'LONUTI', nbm)
        call jeveuo(jexnom(grpmai, nomgr), 'L', jgrm2)
        do 60,im = 1,nbm,1
        numm = zi(jgrm2+im-1)
        call i2rdli(numm, zi(alstot), libr1)
60      continue
70      continue
        call jedetr('&&OP0050.GROUP_MA')
    endif
    80 end do
    nbtm = libr1 - 1
!
!     --- ON VERIFIE QU'EN 1 NOEUD ON A AU PLUS 2 MAILLES ---
!
    call wkvect('&&OP0050.VERI_MAIL', 'V V I', nbtnoe, jnoe)
    do 90 im = 1, nbtm
        mi = zi(alstot+im-1)
        call i2extf(mi, 1, conec(1:15), type(1:16), nig,&
                    nid)
        zi(jnoe+nig-1) = zi(jnoe+nig-1) + 1
        zi(jnoe+nid-1) = zi(jnoe+nid-1) + 1
90  end do
    ouvert = .false.
    do 100 in = 1, nbtnoe
        if (zi(jnoe+in-1) .eq. 1) ouvert = .true.
100  end do
    do 110 in = 1, nbtnoe
        if (zi(jnoe+in-1) .gt. 2) then
            call jenuno(jexnum(nomnoe, in), nomse)
            valk (1) = nomse
            vali (1) = zi(jnoe+in-1)
            call u2mesg('F', 'INTEMAIL_17', 1, valk, 1,&
                        vali, 0, 0.d0)
        endif
110  end do
    call jedetr('&&OP0050.VERI_MAIL')
!
!     --- CHOIX DE L'ABSCISSE CURVILIGNE 0. ---
!
    numno = 0
    call getvtx(' ', 'NOEUD_ORIG', nbval=0, nbret=n1)
    if (n1 .ne. 0) then
        call getvtx(' ', 'NOEUD_ORIG', scal=noeud, nbret=n1)
        call jenonu(jexnom(nomnoe, noeud), numno)
    else
        call getvtx(' ', 'GROUP_NO_ORIG', nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call getvtx(' ', 'GROUP_NO_ORIG', scal=noeud, nbret=n1)
            call utnono(' ', nomail, 'NOEUD', noeud, k8b,&
                        iret)
            if (iret .eq. 10) then
                call u2mesk('F', 'INTEMAIL_31', 1, noeud)
            else if (iret.eq.1) then
                valk (1) = noeud
                valk (2) = k8b
                call u2mesk('A', 'INTEMAIL_18', 2, valk)
            endif
            call jenonu(jexnom(nomnoe, k8b), numno)
        endif
    endif
!
    if (numno .ne. 0) then
        trouve = 0
        do 120 im = 1, nbtm
            mi = zi(alstot+im-1)
            call i2extf(mi, 1, conec(1:15), type(1:16), nig,&
                        nid)
            if (nig .eq. numno) then
                trouve = trouve + 1
                ideb = im
            endif
            if (nid .eq. numno) then
                trouve = trouve + 1
                ideb = im
            endif
120      continue
        if (ouvert .and. trouve .ne. 1) then
            call u2mess('F', 'INTEMAIL_1')
        endif
        if (trouve .eq. 0) then
            call u2mess('F', 'INTEMAIL_2')
        endif
        call wkvect('&&OP0050.VERI_MAIL', 'V V I', nbtm, jnoe)
        mi = 0
        do 130 im = ideb, nbtm
            mi = mi + 1
            zi(jnoe+mi-1) = zi(alstot+im-1)
130      continue
        do 140 im = 1, ideb - 1
            mi = mi + 1
            zi(jnoe+mi-1) = zi(alstot+im-1)
140      continue
        do 150 im = 1, nbtm
            zi(alstot+im-1) = zi(jnoe+im-1)
150      continue
        call jedetr('&&OP0050.VERI_MAIL')
    endif
!
!        /* CALCUL DES COMPOSSANTES CONNEXES (I.E. CHEMINS) */
!        /*       ET REPERAGE DANS LE MAILLAGE              */
!
    call wkvect('&INTSTRUCT', 'V V I', 2*nbtm, astrct)
    call wkvect('&INTMAIL1', 'V V I', nbtm, jmail1)
    call wkvect('&INTMAIL2', 'V V I', nbtm, jmail2)
    call wkvect('&INTPTSTRUCT', 'V V I', nbtm+1, aptstr)
    call i2imam(conec, type, zi(alstot), nbtm, zi(astrct),&
                zi(aptstr), nbcnx, zi(jmail1), zi(jmail2))
!
!         /* CREATION DES CHAMPS DU CONCEPT PRODUIT */
!
    nchmin = nomcrb//'.CHEMIN'
    nmail1 = nomcrb//'.MAIL1'
    nmail2 = nomcrb//'.MAIL2'
    ntpcrb = nomcrb//'.TYPCOURBE'
    nnomma = nomcrb//'.NOMMAIL'
!
    n1 = zi(aptstr+ (nbcnx+1)-1) - 1
    call jecrec(nchmin, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbcnx)
    call jeecra(nchmin, 'LONT', n1)
!
    call jecrec(nmail1, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbcnx)
    call jeecra(nmail1, 'LONT', n1)
!
    call jecrec(nmail2, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbcnx)
    call jeecra(nmail2, 'LONT', n1)
!
    call wkvect(ntpcrb, 'G V K8', 1, ktypcb)
    zk8(ktypcb) = 'LISTMAIL'
!
    call wkvect(nnomma, 'G V K8', 1, knomma)
    zk8(knomma) = nomail
!
    do 170,chm = 1,nbcnx,1
    debchm = zi(aptstr+chm-1)
    finchm = zi(aptstr+chm) - 1
    n1 = finchm - debchm + 1
    call jecroc(jexnum(nchmin, chm))
    call jeecra(jexnum(nchmin, chm), 'LONMAX', n1)
    call jeveuo(jexnum(nchmin, chm), 'E', kchm)
    call jecroc(jexnum(nmail1, chm))
    call jeecra(jexnum(nmail1, chm), 'LONMAX', n1)
    call jeveuo(jexnum(nmail1, chm), 'E', kmail1)
    call jecroc(jexnum(nmail2, chm))
    call jeecra(jexnum(nmail2, chm), 'LONMAX', n1)
    call jeveuo(jexnum(nmail2, chm), 'E', kmail2)
    do 160,sgcour = debchm,finchm,1
    ind = zi(astrct+sgcour-1)
    if (ind .ne. 0) then
        numse = zi(alstot+ind-1)
        call jenuno(jexnum(nommai, numse), nomse)
        zi(kchm+sgcour-debchm) = zi(alstot+ind-1)
        zi(kmail1+sgcour-debchm) = zi(jmail1+ind-1)
        zi(kmail2+sgcour-debchm) = zi(jmail2+ind-1)
    else
        zi(kchm+sgcour-debchm) = 0
        zi(kmail1+sgcour-debchm) = 0
        zi(kmail2+sgcour-debchm) = 0
    endif
160  continue
    170 end do
    call jedetr('&INTSTRUCT')
    call jedetr('&INTMAIL1')
    call jedetr('&INTMAIL2')
    call jedetr('&INTPTSTRUCT')
    call jedetr('&INTLISTOTAL')
!
    if (niv .ge. 2) then
        write (ifm,1000)
        write (ifm,1010) nbcnx
        do 190,chm = 1,nbcnx,1
        call jelira(jexnum(nchmin, chm), 'LONMAX', nbm)
        call jeveuo(jexnum(nchmin, chm), 'L', kchm)
        call jeveuo(jexnum(nmail1, chm), 'L', kmail1)
        call jeveuo(jexnum(nmail2, chm), 'L', kmail2)
        numse = zi(kchm+nbm-1)
        if (numse .eq. 0) then
            write (ifm,1020) chm
        else
            write (ifm,1030) chm
        endif
        write (ifm,1040)
        do 180,sgcour = 1,nbm,1
        numse = zi(kchm+sgcour-1)
        if (numse .eq. 0) goto 180
        call jenuno(jexnum(nommai, numse), nomse)
        numm1 = zi(kmail1+sgcour-1)
        numm2 = zi(kmail2+sgcour-1)
        if (numm1 .eq. 0 .and. numm2 .eq. 0) then
            write (ifm,1050) nomse
        else if (numm1.eq.0 .and. numm2.ne.0) then
            call jenuno(jexnum(nommai, numm2), nomm2)
            write (ifm,1052) nomse,nomm2
        else if (numm1.ne.0 .and. numm2.ne.0) then
            call jenuno(jexnum(nommai, numm1), nomm1)
            call jenuno(jexnum(nommai, numm2), nomm2)
            write (ifm,1054) nomse,nomm1,nomm2
        else if (numm1.ne.0 .and. numm2.eq.0) then
            call jenuno(jexnum(nommai, numm1), nomm1)
            write (ifm,1052) nomse,nomm1
        endif
180      continue
190      continue
    endif
!
    1000 format (/,1x,'COURBE DEFINIE A PARTIR D''UNE LISTE DE MAILLES')
    1010 format (3x,'COURBE DEFINIE PAR ',i2,' CHEMIN(S)')
    1020 format (5x,'CHEMIN NUMERO ',i2,' DE TYPE "ARC OUVERT"')
    1030 format (5x,'CHEMIN NUMERO ',i2,' DE TYPE "CYCLE"')
! 1040 FORMAT(9X,'MAILLE  ENCADREE PAR MAILLE_1  MAILLE_2')
    1040 format (9x,'MAILLES DEFINISSANT LE CHEMIN:')
    1050 format (10x,a8)
    1052 format (10x,a8,14x,a8)
    1054 format (10x,a8,14x,a8,2x,a8)
!
    call jedema()
end subroutine
