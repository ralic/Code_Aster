subroutine sinoz2(modele, pfchno, sigel, signo)
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
!   BUT :  CALCUL DES CONTRAINTES AUX NOEUDS PAR LA METHODE ZZ2
    implicit none
!
!   IN  MODELE   :   NOM DU MODELE
!   IN  PFCHNO   :   PROF_CHNO
!   IN  SIGEL    :   NOM DU CHAMP DE CONTRAINTES AUX POINTS DE GAUSS
!
!  OUT  SIGNO    :   NOM DU CHAMP DE CONTRAINTES AUX NOEUDS
!
! ----------------------- DECLARATIONS --------------------------------
!
#include "jeveux.h"
!
#include "asterfort/assvec.h"
#include "asterfort/celfpg.h"
#include "asterfort/celver.h"
#include "asterfort/copisd.h"
#include "asterfort/crcnct.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/matini.h"
#include "asterfort/mecanb.h"
#include "asterfort/mtcrou.h"
#include "asterfort/predia.h"
#include "asterfort/u2mess.h"
#include "asterfort/utelvf.h"
#include "asterfort/wkvect.h"
#include "asterfort/zzappa.h"
#include "asterfort/zzcala.h"
#include "asterfort/zzcalb.h"
#include "asterfort/zzpoly.h"
    character(len=8) :: modele, kbid, ma, typema, licmp(4), vecass, elrefe
    character(len=8) :: famil
    character(len=14) :: nu14
    character(len=19) :: pfchno
    character(len=16) :: phen
    character(len=19) :: noeub, mo, vecel
    character(len=24) :: signo, sigel, lisvec, typmai, connex, coninv
    character(len=24) :: nomjv, elrfam
    real(kind=8) :: rcmp(4), eps, x(9), y(9), a(9, 9), b(9, 4), diag(9)
    real(kind=8) :: wk1(9, 9), wk2(9)
    integer :: nno, npg, ivf
    logical :: app
!
!
!
!
!-----------------------------------------------------------------------
    integer :: i, iacoor, iad, ialcv, iamav, ianew, ianob
    integer :: ianov, iarepe, iatyma, ibid, ic, icmp, ier
    integer :: iindic, ima, ino, inob, inoma, ipa, jceld
    integer :: jcelv, jcon, jconin, jelfa, jnb, jnoeu, jpa
    integer :: jpami, jprno, jrefe, jrefn, jsig, jval, k
    integer :: nb, nbcmp, nbec, nbma, nbmav, nbn, nbno
    integer :: nbnob, nbnobp, nbnoma, nqua, ntri, num, numav
    integer :: numc, numel, numeq, numgr, numloc
    real(kind=8) :: xino, xinob, xinomi, xmax, xmin, xnorm, yino
    real(kind=8) :: yinob, yinomi, ymax, ymin
!-----------------------------------------------------------------------
    call jemarq()
!
    nomjv = '&&SINOZ2.FFORMES        '
    elrfam = '&&SINOZ2.ELRE_FAMI'
!
!     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
    call celver(sigel, 'NBVARI_CST', 'STOP', ibid)
    call celver(sigel, 'NBSPT_1', 'STOP', ibid)
!
    call dismoi('F', 'PHENOMENE', modele, 'MODELE', ibid,&
                phen, ier)
    if (phen .ne. 'MECANIQUE') then
        call u2mess('F', 'CALCULEL4_83')
    endif
!
    call dismoi('F', 'NOM_MAILLA', sigel(1:19), 'CHAM_ELEM', ibid,&
                ma, ier)
    call dismoi('F', 'NB_NO_MAILLA', ma, 'MAILLAGE', nbno,&
                kbid, ier)
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbma,&
                kbid, ier)
    typmai = ma//'.TYPMAIL'
    connex = ma//'.CONNEX'
    call jeveuo(typmai, 'L', iatyma)
!
!
!   CONSTRUCTION DE LA CONNECTIVITE INVERSE (OBJET TEMPORAIRE)
!     --  OBJET CONINV    = FAMILLE CONTIGUE DE VECTEURS N*IS
    coninv = '&&SINOZ2.CONINV'
    call jecrec(coninv, 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbno)
!
    call wkvect('&&SINOZ2.LONGCONINV', 'V V I', nbno, ialcv)
    do 20 ima = 1, nbma
        iad = iatyma - 1 + ima
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iad)), typema)
        if (typema(1:4) .eq. 'TRIA' .or. typema(1:4) .eq. 'QUAD') then
            call jeveuo(jexnum(connex, ima), 'L', jcon)
            call jelira(jexnum(connex, ima), 'LONMAX', nbn)
            do 10 ino = 1, nbn
                num = zi(jcon-1+ino)
                zi(ialcv-1+num) = zi(ialcv-1+num) + 1
10          continue
        endif
20  end do
!
    do 30,ino = 1,nbno
    call jeecra(jexnum(coninv, ino), 'LONMAX', zi(ialcv-1+ino))
    30 end do
!
    call wkvect('&&SINOZ2.INDIC', 'V V I', nbno, iindic)
!
    do 50 ima = 1, nbma
        iad = iatyma - 1 + ima
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iad)), typema)
        if (typema(1:4) .eq. 'TRIA' .or. typema(1:4) .eq. 'QUAD') then
            call jeveuo(jexnum(connex, ima), 'L', jcon)
            call jelira(jexnum(connex, ima), 'LONMAX', nbn)
            do 40 ino = 1, nbn
                num = zi(jcon-1+ino)
                call jeveuo(jexnum(coninv, num), 'E', jconin)
                nb = zi(iindic-1+num)
                zi(jconin+nb) = ima
                zi(iindic-1+num) = zi(iindic-1+num) + 1
40          continue
        endif
50  end do
    call jedetr('&&SINOZ2.INDIC')
!
!   CONSTRUCTION D'UN VECTEUR DE BOOLEENS SUR LES NOEUDS INDIQUANT
!   L'APPARTENANCE OU NON AU BORD
!
    vecel = '&&NOEUB            '
    call mecanb(modele, vecel)
    vecass = '&&VECASS'
    lisvec = vecel//'.RELR'
!
!     -- POUR POUVOIR APPELER ASSVEC, IL FAUT CREER UN "FAUX"
!        NUME_DDL AVEC UN PROF_CHNO :
    nu14='&&SINOZ2.NUDDL'
    call copisd('PROF_CHNO', 'V', pfchno, nu14//'.NUME')
    call wkvect(nu14//'.NUME.REFN', 'V V K24', 4, jrefn)
    zk24(jrefn-1+1)=ma
    zk24(jrefn-1+2)='DEPL_R'
!
    call assvec('V', vecass, 1, lisvec, 1.d0,&
                nu14, ' ', 'ZERO', 1)
    call detrsd('NUME_DDL', nu14)
!
    noeub = vecass
    call jeveuo(noeub//'.REFE', 'E', jrefe)
    zk24(jrefe+1)=pfchno
!
    call wkvect('&&SINOZ2.NOEUBORD', 'V V L', nbno, jnoeu)
!
    call dismoi('F', 'NB_EC', 'DEPL_R', 'GRANDEUR', nbec,&
                kbid, ier)
    call jeveuo(jexnum(pfchno//'.PRNO', 1), 'L', jprno)
    call jeveuo(noeub//'.VALE', 'L', jval)
    eps = 1.d-06
    nbnob = 0
    do 70 ino = 1, nbno
        numeq = zi(jprno-1+ (nbec+2)* (ino-1)+1)
        nbcmp = zi(jprno-1+ (nbec+2)* (ino-1)+2)
        xnorm = 0.d0
        do 60 icmp = 1, nbcmp
            xnorm = xnorm + zr(jval-1+numeq-1+icmp)**2
60      continue
        if (xnorm .le. eps) then
            zl(jnoeu-1+ino) = .false.
        else
            zl(jnoeu-1+ino) = .true.
            nbnob = nbnob + 1
        endif
70  end do
    call wkvect('&&SINOZ2.NBPATCHMIL', 'V V I', nbno, jpami)
    call wkvect('&&SINOZ2.NUMNB', 'V V I', nbnob, jnb)
    call wkvect('&&SINOZ2.NBPATCH', 'V V I', nbnob, jpa)
    inob = 0
    do 80 ino = 1, nbno
        if (zl(jnoeu-1+ino)) then
            inob = inob + 1
            zi(jnb-1+inob) = ino
        endif
80  end do
    if (inob .ne. nbnob) then
        call u2mess('F', 'CALCULEL4_84')
    endif
!
!    VERIFICATION DES TYPES DE MAILLE
!
    ntri = 0
    nqua = 0
    do 90 ima = 1, nbma
        iad = iatyma - 1 + ima
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iad)), typema)
        if (typema(1:4) .eq. 'TRIA') then
            ntri = ntri + 1
            if (typema(5:5) .ne. '3' .and. typema(5:5) .ne. '6') then
                call u2mess('F', 'CALCULEL4_85')
            endif
        else if (typema(1:4).eq.'QUAD') then
            nqua = nqua + 1
            if (typema(5:5) .ne. '4' .and. typema(5:5) .ne. '8' .and. typema(5:5) .ne. '9') then
                call u2mess('F', 'CALCULEL4_86')
            endif
        endif
90  end do
    if (ntri .ne. 0 .and. nqua .ne. 0) then
        call u2mess('F', 'CALCULEL4_87')
    endif
    if (ntri .eq. 0 .and. nqua .eq. 0) then
        call u2mess('F', 'CALCULEL4_88')
    endif
!
    mo = modele//'.MODELE    '
    call jeveuo(mo//'.REPE', 'L', iarepe)
!
    rcmp(1) = 0.d0
    rcmp(2) = 0.d0
    rcmp(3) = 0.d0
    rcmp(4) = 0.d0
    licmp(1) = 'SIXX'
    licmp(2) = 'SIYY'
    licmp(3) = 'SIZZ'
    licmp(4) = 'SIXY'
    call crcnct('G', signo, ma, 'SIEF_R', 4,&
                licmp, rcmp)
    call jeveuo(signo(1:19)//'.VALE', 'E', jsig)
    call jeveuo(ma//'.COORDO    .VALE', 'L', iacoor)
    call jeveuo(sigel(1:19)//'.CELD', 'L', jceld)
    call jeveuo(sigel(1:19)//'.CELV', 'L', jcelv)
!
! --- RECUPERATION DE L'ELREFE ET DE LA FAMILLE POUR CHAQUE MAILLE
!
    call celfpg(sigel, elrfam, ibid)
    call jeveuo(elrfam, 'L', jelfa)
!
!   BOUCLE SUR LES NOEUDS
!   *********************
!
    ipa = 0
    do 240 ino = 1, nbno
        if (.not.zl(jnoeu-1+ino)) then
            call jelira(jexnum(coninv, ino), 'LONMAX', nbmav)
!
!    TRAITEMENT DES SOMMETS
!
            if (nbmav .gt. 2) then
                ipa = ipa + 1
                call jeveuo(jexnum(coninv, ino), 'L', iamav)
                call wkvect('&&SINOZ2.NOEBOPA', 'V V I', 10*nbmav, ianob)
!
!    INITIALISATION DE LA MATRICE A ET DU SECOND MEMBRE A ZERO
!
                call matini(9, 9, 0.d0, a)
                call matini(9, 4, 0.d0, b)
!
                nbnobp = 0
                ianew = ianob
!
!      PASSAGE EN COORDONNEES LOCALES AU PATCH (ENTRE -1. ET 1.)
!      POUR AMELIORER LE CONDITIONNEMENT DE LA MATRICE A
!
!      CALCUL DE XMIN,XMAX,YMIN,YMAX SUR LE PATCH
!
                xmin = 1.d+10
                xmax = -1.d+10
                ymin = 1.d+10
                ymax = -1.d+10
                do 140 ima = 1, nbmav
                    numav = zi(iamav-1+ima)
                    call jelira(jexnum(connex, numav), 'LONMAX', nbnoma)
                    call jeveuo(jexnum(connex, numav), 'L', ianov)
                    do 130 inoma = 1, nbnoma
                        num = zi(ianov-1+inoma)
                        x(inoma) = zr(iacoor-1+3* (num-1)+1)
                        y(inoma) = zr(iacoor-1+3* (num-1)+2)
                        if (x(inoma) .le. xmin) xmin = x(inoma)
                        if (y(inoma) .le. ymin) ymin = y(inoma)
                        if (x(inoma) .ge. xmax) xmax = x(inoma)
                        if (y(inoma) .ge. ymax) ymax = y(inoma)
130                  continue
140              continue
!
!      BOUCLE SUR LES MAILLES VOISINES
!
                do 160 ima = 1, nbmav
                    numav = zi(iamav-1+ima)
                    numgr = zi(iarepe-1+2*(numav-1)+1)
                    numel = zi(iarepe-1+2*(numav-1)+2)
                    call jelira(jexnum(connex, numav), 'LONMAX', nbnoma)
                    call jeveuo(jexnum(connex, numav), 'L', ianov)
!
!        RECUPERATION DES COORDONNEES DES NOEUDS
!
                    do 150 inoma = 1, nbnoma
                        num = zi(ianov-1+inoma)
!    SI NOEUD BORD
                        if (zl(jnoeu-1+num)) then
                            call zzappa(num, zi(ianew), nbnobp, app)
                            if (.not.app) then
                                nbnobp = nbnobp + 1
                                zi(ianew-1+nbnobp) = num
                            endif
                        endif
                        x(inoma) = zr(iacoor-1+3* (num-1)+1)
                        y(inoma) = zr(iacoor-1+3* (num-1)+2)
150                  continue
!
!        RECUPERATION DES FCTS DE FORME DE L'ELEMENT
                    elrefe = zk16(jelfa-1+numav)(1:8)
                    famil = zk16(jelfa-1+numav)(9:16)
                    call utelvf(elrefe, famil, nomjv, npg, nno)
                    call jeveuo(nomjv, 'L', ivf)
!
!    CALCUL DE LA MATRICE A
!
                    call zzcala(npg, nno, zr(ivf), x, y,&
                                xmin, xmax, ymin, ymax, a)
!
!    CALCUL DU SECOND MEMBRE B
!
                    call zzcalb(numgr, numel, npg, nno, zr(ivf),&
                                zi(jceld), zr(jcelv), x, y, xmin,&
                                xmax, ymin, ymax, b)
!
                    call jedetr(nomjv)
!
160              continue
                ianew = ianob + nbnobp
!
!   PRECONDITIONNEMENT PAR LA DIAGONALE ET RESOLUTION
!
                call predia(a, b, diag, nno)
                call mtcrou(a, b, 9, nno, 4,&
                            wk1, wk2)
!
                do 180 ic = 1, 4
                    do 170 i = 1, nno
                        b(i,ic) = b(i,ic)*diag(i)
170                  continue
180              continue
!
                xino = zr(iacoor-1+3* (ino-1)+1)
                yino = zr(iacoor-1+3* (ino-1)+2)
!
                xino = -1.d0 + 2.d0* (xino-xmin)/ (xmax-xmin)
                yino = -1.d0 + 2.d0* (yino-ymin)/ (ymax-ymin)
!
!   CALCUL DES CONTRAINTES LISSEES AU NOEUD INO
!
                call zzpoly(nno, ino, xino, yino, zr(jsig),&
                            b)
!
!    TRAITEMENT DES NOEUDS BORD DU PATCH
!
                do 200 inob = 1, nbnobp
                    num = zi(ianob-1+inob)
                    do 190 k = 1, nbnob
                        numc = zi(jnb-1+k)
                        if (numc .eq. num) then
                            zi(jpa-1+k) = zi(jpa-1+k) + 1
                        endif
190                  continue
                    xinob = zr(iacoor-1+3* (num-1)+1)
                    yinob = zr(iacoor-1+3* (num-1)+2)
                    xinob = -1.d0 + 2.d0* (xinob-xmin)/ (xmax-xmin)
                    yinob = -1.d0 + 2.d0* (yinob-ymin)/ (ymax-ymin)
                    call zzpoly(nno, num, xinob, yinob, zr(jsig),&
                                b)
200              continue
                call jedetr('&&SINOZ2.NOEBOPA')
!
!    TRAITEMENT DES NOEUDS MILIEUX (NON BORD)
!
                if (nno .ge. 6) then
                    do 220 ima = 1, nbmav
                        numav = zi(iamav-1+ima)
                        call jelira(jexnum(connex, numav), 'LONMAX', nbnoma)
                        call jeveuo(jexnum(connex, numav), 'L', ianov)
!
!       RECUPERATION DU NOEUD MILIEU ASSOCIE AU NOEUD SOMMET DU PATCH
!             (1 SEUL PAR ELEMENT VOISIN)
!
                        do 210 inoma = 1, nbnoma
                            num = zi(ianov-1+inoma)
                            if (num .eq. ino) then
                                numloc = inoma
                            endif
210                      continue
!
                        if (nno .eq. 6) numloc = numloc + 3
                        if (nno .eq. 8) numloc = numloc + 4
                        if (nno .eq. 9) numloc = numloc + 4
                        num = zi(ianov-1+numloc)
                        zi(jpami-1+num) = zi(jpami-1+num) + 1
                        xinomi = zr(iacoor-1+3* (num-1)+1)
                        yinomi = zr(iacoor-1+3* (num-1)+2)
                        xinomi = -1.d0 + 2.d0* (xinomi-xmin)/ (xmax- xmin)
                        yinomi = -1.d0 + 2.d0* (yinomi-ymin)/ (ymax- ymin)
                        call zzpoly(nno, num, xinomi, yinomi, zr(jsig),&
                                    b)
220                  continue
!
                endif
!
!    TRAITEMENT DU NOEUD BARYCENTRE (CAS DES Q9)
!
                if (nno .eq. 9) then
                    do 230 ima = 1, nbmav
                        numav = zi(iamav-1+ima)
                        call jelira(jexnum(connex, numav), 'LONMAX', nbnoma)
                        call jeveuo(jexnum(connex, numav), 'L', ianov)
!
!     RECUPERATION DU NOEUD BARYCENTRE
!
                        num = zi(ianov-1+nbnoma)
                        zi(jpami-1+num) = zi(jpami-1+num) + 1
                        xinomi = zr(iacoor-1+3* (num-1)+1)
                        yinomi = zr(iacoor-1+3* (num-1)+2)
                        xinomi = -1.d0 + 2.d0* (xinomi-xmin)/ (xmax- xmin)
                        yinomi = -1.d0 + 2.d0* (yinomi-ymin)/ (ymax- ymin)
                        call zzpoly(nno, num, xinomi, yinomi, zr(jsig),&
                                    b)
230                  continue
                endif
!
            endif
        endif
240  end do
!
!    MOYENNAGE SUR LES NOEUDS BORD
!
    do 260 i = 1, nbnob
        num = zi(jnb-1+i)
        if (zi(jpa-1+i) .eq. 0) then
            call u2mess('F', 'CALCULEL4_89')
        endif
        do 250 ic = 1, 4
            zr(jsig-1+4* (num-1)+ic) = zr(jsig-1+4* (num-1)+ic)/ zi(jpa-1+i)
250      continue
260  end do
!
!    MOYENNAGE SUR LES NOEUDS NON BORD
!
    do 280 ino = 1, nbno
!    SI PAS NOEUD BORD
        if (.not.zl(jnoeu-1+ino)) then
            if (zi(jpami-1+ino) .eq. 0) then
                zi(jpami-1+ino) = 1
            endif
            do 270 ic = 1, 4
                zr(jsig-1+4* (ino-1)+ic) = zr( jsig-1+4* (ino-1)+ic )/ zi(jpami-1+ino )
270          continue
        endif
280  end do
    call detrsd('CHAMP_GD', '&&VECASS')
    call jedetr('&&SINOZ2.NUMNB')
    call jedetr('&&SINOZ2.NBPATCH')
    call jedetr('&&SINOZ2.NBPATCHMIL')
    call jedetr('&&SINOZ2.NOEUBORD')
    call jedetr('&&SINOZ2.CONINV')
    call jedetr('&&SINOZ2.LONGCONINV')
    call jedetr(elrfam)
!
    call jedema()
end subroutine
