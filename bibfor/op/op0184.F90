subroutine op0184()
    implicit none
!     -----------------------------------------------------------------
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
!     LECTURE D'UN RESULTAT PLEXUS (PRESSION) SUR FICHIER IDEAS
!     LA PRESSION EST CALCULEE PAR PLEXUS SUR DES SEG2 (CONSTANTE)
!     LE MAILLAGE ASTER PEUT COMPORTER DES SEG2, DES SEG3, DES COQUES
!
!     -----------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/calcul.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/pj3da4.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsinfo.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/titre.h"
#include "asterfort/typele.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/ulisop.h"
#include "asterfort/ulopen.h"
#include "asterfort/wkvect.h"
!
!
    integer :: nbv, nbmapl, ibid, ntout, nnume, np, iul
    integer :: vali(2)
    integer :: nbordr, jnume, n1, nlinst, jlist, nbinst, nis, nc, nfor
    integer :: iret, jtitr, nbtitr, ifsig, l, ipas, k, numpas, ino
    integer :: ivar(6), iord, jpres, ima, nbnoas, jnoma, i, nnu
    integer :: iadrno, imp, jdme, ntseg, imamin, jdco, jdno, no1, no2, nutyel
    integer :: jcelv, jceld, idec, nbelgr, liel, iel, iadno, nno
    integer :: jinst, nbtrou, lordr, ntpoi, itest, iad, imapl
    integer :: nbordt, te, nbgr, igr, ier
    real(kind=8) :: rbid, pres, epsi, temps, tref, cm(3), a(3), b(3), la, lb, d2
    real(kind=8) :: d2min
    real(kind=8) :: valr
    complex(kind=8) :: cbid
    character(len=6) :: kar
    character(len=8) :: resu, nomapl, nomast, k8b, listr8, form, crit
    character(len=8) :: nomo, lpain(1), lpaout(1)
    character(len=16) :: nomcmd, concep, nsymb, nomte, k16nom
    character(len=19) :: nomch, ligrmo, chpres, capres
    character(len=24) :: coorn, typma, coorp, mlgcnx, lchin(1), lchout(1)
    character(len=24) :: noliel, chgeom, option, connex
    character(len=80) :: k80b, k80bm, k80bid
    integer :: iarg
!     -----------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    k80bm = ' '
    call getres(resu, concep, nomcmd)
!
!     LECTURE DE LA NUMEROTATION DES DDL
!
    call getvid(' ', 'MAIL_PLEXUS', scal=nomapl, nbret=nbv)
    call dismoi('F', 'NB_MA_MAILLA', nomapl, 'MAILLAGE', nbmapl,&
                k8b, ier)
    call getvid(' ', 'MAILLAGE', scal=nomast, nbret=nbv)
    call dismoi('F', 'NB_NO_MAILLA', nomast, 'MAILLAGE', nbnoas,&
                k8b, ier)
    call wkvect('&&OP0184.NOAST_MAPLEX', 'V V I', nbnoas, jnoma)
    coorn = nomast//'.COORDO    .VALE'
    call jeveuo(coorn, 'L', iadrno)
    coorp = nomapl//'.COORDO    .VALE'
    call jeveuo(coorp, 'L', jdco)
    typma = nomapl//'.TYPMAIL'
    call jeveuo(typma, 'L', jdme)
    mlgcnx = nomapl//'.CONNEX'
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2'), ntseg)
!
! IL PEUT Y AVOIR DES POI1 DANS LES ELEMENTS PLEXUS ON LES IGNORE
!
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1'), ntpoi)
    itest = 0
!
    do 50 ino = 1, nbnoas
        do 10 i = 1, 3
            cm(i) = zr(iadrno+3*ino-3+i-1)
10      continue
        imamin = 0
        d2min = 1.d10
        do 40 imp = 1, nbmapl
            nutyel = zi(jdme+imp-1)
            if (nutyel .eq. ntpoi) then
                itest = -1
                goto 40
            else if (nutyel.eq.ntseg) then
                goto 20
            else
                call u2mess('F', 'UTILITAI3_9')
            endif
20          continue
            call jeveuo(jexnum(mlgcnx, imp), 'L', jdno)
            no1 = zi(jdno)
            no2 = zi(jdno+1)
            do 30 i = 1, 3
                a(i) = zr(jdco+ (no1-1)*3+i-1)
                b(i) = zr(jdco+ (no2-1)*3+i-1)
30          continue
            call pj3da4(cm, a, b, la, lb,&
                        d2)
            if (d2 .lt. d2min) then
                imamin = imp
                d2min = d2
            endif
40      continue
        zi(jnoma-1+ino) = imamin
50  end do
! TEST SUR LA PRESENCE DE MAILLE PONCTUELLE
    if (itest .ne. 0) call u2mess('I', 'UTILITAI3_10')
!
!     --- QUELS SONT LES INSTANTS A RELIRE ---
!
    nbordr = 0
    call getvtx(' ', 'TOUT_ORDRE', scal=k8b, nbret=ntout)
    if (ntout .eq. 0) then
        call getvis(' ', 'NUME_ORDRE', nbval=0, nbret=nnume)
        if (nnume .ne. 0) then
            nbordr = -nnume
            call wkvect('&&OP0184.NUME_ORDRE', 'V V I', nbordr, jnume)
            call getvis(' ', 'NUME_ORDRE', nbval=nbordr, vect=zi(jnume), nbret=n1)
        else
            call getvid(' ', 'LIST_ORDRE', scal=listr8, nbret=nnu)
            if (nnu .ne. 0) then
                call jeveuo(listr8//'.VALE', 'L', jnume)
                call jelira(listr8//'.VALE', 'LONMAX', nbordr)
            else
                call getvid(' ', 'LIST_INST', scal=listr8, nbret=nlinst)
                if (nlinst .ne. 0) then
                    call jeveuo(listr8//'.VALE', 'L', jlist)
                    call jelira(listr8//'.VALE', 'LONMAX', nbordr)
                else
                    call getvr8(' ', 'INST', nbval=0, nbret=nis)
                    if (nis .ne. 0) then
                        nbinst = -nis
                        call wkvect('&&OP0184.INST', 'V V R', nbordr, jlist)
                        call getvr8(' ', 'INST', nbval=nbordr, vect=zr(jlist), nbret=n1)
                    endif
                endif
            endif
        endif
    endif
!
!     --- LECTURE DE LA PRECISION ET DU CRITERE ---
!
    call getvr8(' ', 'PRECISION', scal=epsi, nbret=np)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=nc)
!
!     FORMAT IDEAS OBLIGATOIRE
!
    call getvtx(' ', 'FORMAT', scal=form, nbret=nfor)
!
    if (form .ne. 'IDEAS') then
        call u2mess('F', 'UTILITAI3_11')
    endif
    call jeexin(nomapl//'           .TITR', iret)
    if (iret .eq. 0) then
        call u2mess('F', 'UTILITAI3_12')
    else
        call jeveuo(nomapl//'           .TITR', 'L', jtitr)
        call jelira(nomapl//'           .TITR', 'LONMAX', nbtitr)
        if (nbtitr .ge. 1) then
            if (zk80(jtitr) (10:31) .ne. 'AUTEUR=INTERFACE_IDEAS') then
                call u2mess('F', 'UTILITAI3_12')
            endif
        else
            call u2mess('A', 'UTILITAI3_13')
        endif
    endif
!
!     CREATION DE LA SD RESULTAT
!
    nbordt = max(1,nbordr)
    call rscrsd('G', resu, 'EVOL_CHAR', nbordt)
!
!     CREATION DU CHAMP DE PRESSION
!
    call getvid(' ', 'MODELE', scal=nomo, nbret=nbv)
    chgeom = nomast//'.COORDO'
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpaout(1) = 'PPRES_R'
    chpres = '&&OP0184.CHPRES'
    lchout(1) = chpres
    ligrmo = nomo//'.MODELE'
    option = 'TOU_INI_ELNO'
    call calcul('S', option, ligrmo, 1, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
    call jeveuo(chpres//'.CELD', 'L', jceld)
    call jeveuo(chpres//'.CELV', 'E', jcelv)
    connex = nomast//'.CONNEX'
    noliel = ligrmo//'.LIEL'
    nbgr = nbgrel(ligrmo)
!
    capres = '&&OP0184.PRES'
    call wkvect(capres, 'V V R', nbmapl, jpres)
!
    call getvis(' ', 'UNITE', scal=ifsig, nbret=l)
    k16nom = ' '
    if (ulisop ( ifsig, k16nom ) .eq. 0) then
        call ulopen(ifsig, ' ', ' ', 'NEW', 'O')
    endif
    ipas = 0
!
!     LECTURE DES DATASET DU FICHIER IDEAS
!     ON NE LIT QUE DES CHAMPS CONSTANTS PAR ELEMENTS
!
60  continue
!
    read (ifsig,'(A6)',end=160,err=180) kar
    if (kar .eq. '    56') then
        read (ifsig,'(40A2)',end=180) k80b
        read (ifsig,'(40A2)',end=180) k80b
        read (ifsig,'(40A2)',end=180) k80b
        read (ifsig,'(A80)',end=180) k80b
! LECTURE DE LA VARIABLE PRESSION EN FONCTION DU TYPE
! DE MATERIAU PLEXUS UTILISE
!
!
        if (.not. (&
            k80b(49:52) .eq. 'MULT' .or. k80b(49: 52) .eq. 'EAU ' .or. k80b(49:52) .eq. 'FLUI')) &
        goto 60
        read (ifsig,'(40A2)',end=180) k80bid
        k8b = k80bid(1:8)
    else if (kar.eq.'  2414') then
        read (ifsig,'(1I10)',end=180) ibid
        read (ifsig,'(40A2)',end=180) k80b
        read (ifsig,'(1I10)',end=180) ibid
        if (ibid .ne. 2) goto 60
        read (ifsig,'(40A2)',end=180) k80b
        read (ifsig,'(40A2)',end=180) k80b
        read (ifsig,'(40A2)',end=180) k80b
        read (ifsig,'(A80)',end=180) k80bm
        read (ifsig,'(40A2)',end=180) k80b
    else
        goto 60
    endif
    read (ifsig,'(6I10)',end=180) (ivar(k),k=1,6)
!
!        IVAR(3) : TYPE DE DONNEE =0 POUR UNKNOWN
!        IVAR(5) : TYPE DE DONNEE =2 POUR REELLE
!        IVAR(6) : NOMBRE DE VALEURS PAR ELEMENT =20 ICI POUR MULT
!
!
    if (k80b(49:52) .eq. 'MULT') then
        if (ivar(3) .ne. 0) goto 60
        if (ivar(5) .ne. 2) goto 60
        if (ivar(6) .ne. 20) goto 60
    else if (k80b(49:52).eq.'FLUI') then
        if (ivar(3) .ne. 0) goto 60
        if (ivar(5) .ne. 2) goto 60
        if (ivar(6) .ne. 2) goto 60
    else if (k80bm(49:52).eq.'FLUI') then
        if (ivar(3) .ne. 0) goto 60
        if (ivar(5) .ne. 2) goto 60
        if (ivar(6) .ne. 2) goto 60
    else
        if (ivar(3) .ne. 0) goto 60
        if (ivar(5) .ne. 2) goto 60
        if (ivar(6) .ne. 10) goto 60
    endif
!
!
!        VERIFICATION QUE LE DATASET EST AU BON INSTANT
!
    if (kar .eq. '    56') then
        read (ifsig,'(4I10)',end=180) ibid,ibid,ibid,numpas
        read (ifsig,'(E13.5)',end=180) temps
    else
        read (ifsig,'(8I10)',end=180) ibid
        read (ifsig,'(8I10)',end=180) ibid
        read (ifsig,'(6E13.5)',end=180) temps
        read (ifsig,'(6E13.5)',end=180) rbid
    endif
!
    if (ntout .ne. 0) then
        goto 90
    else
        if (nbordr .ne. 0) then
            if (kar .eq. '  2414') then
                call u2mess('F', 'UTILITAI3_14')
            endif
            do 70 iord = 1, nbordr
                if (zi(jnume+iord-1) .eq. numpas) goto 90
70          continue
        else if (nbinst.ne.0) then
            do 80 iord = 1, nbinst
                tref = zr(jlist+iord-1)
                if (crit(1:4) .eq. 'RELA') then
                    if (abs(tref-temps) .le. abs(epsi*temps)) goto 90
                else if (crit(1:4).eq.'ABSO') then
                    if (abs(tref-temps) .le. abs(epsi)) goto 90
                endif
80          continue
        endif
        goto 60
    endif
90  continue
    ipas = ipas + 1
!
!        LECTURE DES PRESSIONS
!
100  continue
    read (ifsig,'(I10)',end=180) ima
    if (ima .eq. -1) goto 110
    read (ifsig,'(E13.5)',end=180) pres
    zr(jpres-1+ima) = pres
    if (k80bm(49:52) .eq. 'FLUI') then
        goto 100
    else if (k80b(49:52).eq.'MULT') then
        read (ifsig,'(E13.5)',end=180) rbid
        read (ifsig,'(E13.5)',end=180) rbid
        read (ifsig,'(E13.5)',end=180) rbid
    else
        read (ifsig,'(E13.5)',end=180) rbid
    endif
!
    goto 100
110  continue
!
    do 140 igr = 1, nbgr
        idec = zi(jceld-1+zi(jceld-1+4+igr)+8)
        te = typele(ligrmo,igr)
        call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
        if (nomte .eq. 'MEDKTR3' .or. nomte .eq. 'MEDKQU4' .or. nomte .eq. 'MET3SEG3' .or.&
            nomte .eq. 'MEC3TR7H' .or. nomte .eq. 'MEC3QU9H') then
            nbelgr = nbelem(ligrmo,igr)
            call jeveuo(jexnum(noliel, igr), 'L', liel)
            do 130 iel = 1, nbelgr
                ima = zi(liel-1+iel)
                call jeveuo(jexnum(connex, ima), 'L', iadno)
                call jelira(jexnum(connex, ima), 'LONMAX', nno)
                iad = jcelv - 1 + idec - 1 + nno* (iel-1)
                do 120 i = 1, nno
                    ino = zi(iadno-1+i)
                    imapl = zi(jnoma+ino-1)
                    pres = zr(jpres-1+imapl)
!  SUITE AUX CORRECTIONS SUR LE SIGNE DE LA PRESSION
!  ON NE MODIFIE SURTOUT PAS LA PRESSION LUE
!  ==> LES TE SAVENT CE QU4ILS ONT A FAIRE
                    zr(iad+i) = pres
120              continue
130          continue
        endif
140  end do
!
    nsymb = 'PRES'
    call rsexch(' ', resu, nsymb, ipas, nomch,&
                iret)
    if (iret .eq. 100) then
    else if (iret.eq.110) then
        call rsagsd(resu, 0)
        call rsexch(' ', resu, nsymb, ipas, nomch,&
                    iret)
    else
        vali (1) = ipas
        vali (2) = iret
        call u2mesg('F', 'UTILITAI8_7', 0, ' ', 2,&
                    vali, 0, 0.d0)
    endif
    call copisd('CHAMP_GD', 'G', chpres, nomch)
    call rsnoch(resu, nsymb, ipas)
    call rsadpa(resu, 'E', 1, 'INST', ipas,&
                0, jinst, k8b)
    zr(jinst) = temps
    do 150 i = 1, nbmapl
        zr(jpres-1+i) = 0.d0
150  end do
!
    goto 60
!
160  continue
!
    call u2mesg('I', 'UTILITAI8_8', 0, ' ', 0,&
                0, 0, 0.d0)
    call rsorac(resu, 'LONUTI', ibid, rbid, k8b,&
                cbid, epsi, crit, nbordr, 1,&
                nbtrou)
    if (nbordr .le. 0) then
        call u2mess('F', 'UTILITAI2_97')
    endif
    call wkvect('&&OP0184.NUME_ORDR', 'V V I', nbordr, lordr)
    call rsorac(resu, 'TOUT_ORDRE', ibid, rbid, k8b,&
                cbid, epsi, crit, zi(lordr), nbordr,&
                nbtrou)
    do 170 iord = 1, nbordr
        call rsadpa(resu, 'L', 1, 'INST', zi(lordr+iord-1),&
                    0, jinst, k8b)
        vali (1) = zi(lordr+iord-1)
        valr = zr(jinst)
        call u2mesg('I', 'UTILITAI8_9', 0, ' ', 1,&
                    vali, 1, valr)
170  end do
    call u2mesg('I', 'VIDE_1', 0, ' ', 0,&
                0, 0, 0.d0)
!
    call titre()
!
    iul = iunifi( 'RESULTAT' )
    call rsinfo(resu, iul)
!
    goto 190
!
180  continue
    call u2mess('F', 'UTILITAI3_15')
!
190  continue
    call jedema()
!
end subroutine
