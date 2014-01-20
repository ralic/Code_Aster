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
#include "asterfort/utelvf.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/zzappa.h"
#include "asterfort/zzcala.h"
#include "asterfort/zzcalb.h"
#include "asterfort/zzpoly.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: modele, ma, typema, licmp(4), vecass, elrefe
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
    integer :: i, iacoor, iad,  iamav, ianew, ianob
    integer :: ianov, iarepe, iatyma, ibid, ic, icmp
    integer ::  ima, ino, inob, inoma, ipa, jceld
    integer :: jcelv, jcon, jconin, jelfa,   jpa
    integer ::  jprno, jrefe, jrefn, jsig, jval, k
    integer :: nb, nbcmp, nbec, nbma, nbmav, nbn, nbno
    integer :: nbnob, nbnobp, nbnoma, nqua, ntri, num, numav
    integer :: numc, numel, numeq, numgr, numloc
    real(kind=8) :: xino, xinob, xinomi, xmax, xmin, xnorm, yino
    real(kind=8) :: yinob, yinomi, ymax, ymin
    integer, pointer :: indic(:) => null()
    integer, pointer :: longconinv(:) => null()
    integer, pointer :: nbpatchmil(:) => null()
    logical, pointer :: noeubord(:) => null()
    integer, pointer :: numnb(:) => null()
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
    call dismoi('PHENOMENE', modele, 'MODELE', repk=phen)
    if (phen .ne. 'MECANIQUE') then
        call utmess('F', 'CALCULEL4_83')
    endif
!
    call dismoi('NOM_MAILLA', sigel(1:19), 'CHAM_ELEM', repk=ma)
    call dismoi('NB_NO_MAILLA', ma, 'MAILLAGE', repi=nbno)
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbma)
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
    AS_ALLOCATE(vi=longconinv, size=nbno)
    do ima = 1, nbma
        iad = iatyma - 1 + ima
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iad)), typema)
        if (typema(1:4) .eq. 'TRIA' .or. typema(1:4) .eq. 'QUAD') then
            call jeveuo(jexnum(connex, ima), 'L', jcon)
            call jelira(jexnum(connex, ima), 'LONMAX', nbn)
            do ino = 1, nbn
                num = zi(jcon-1+ino)
                longconinv(num) = longconinv(num) + 1
            end do
        endif
    end do
!
    do ino = 1, nbno
        call jeecra(jexnum(coninv, ino), 'LONMAX', longconinv(ino))
    end do
!
    AS_ALLOCATE(vi=indic, size=nbno)
!
    do ima = 1, nbma
        iad = iatyma - 1 + ima
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iad)), typema)
        if (typema(1:4) .eq. 'TRIA' .or. typema(1:4) .eq. 'QUAD') then
            call jeveuo(jexnum(connex, ima), 'L', jcon)
            call jelira(jexnum(connex, ima), 'LONMAX', nbn)
            do ino = 1, nbn
                num = zi(jcon-1+ino)
                call jeveuo(jexnum(coninv, num), 'E', jconin)
                nb = indic(num)
                zi(jconin+nb) = ima
                indic(num) = indic(num) + 1
            end do
        endif
    end do
    AS_DEALLOCATE(vi=indic)
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
    call assvec('V', vecass, 1, lisvec, [1.d0],&
                nu14, ' ', 'ZERO', 1)
    call detrsd('NUME_DDL', nu14)
!
    noeub = vecass
    call jeveuo(noeub//'.REFE', 'E', jrefe)
    zk24(jrefe+1)=pfchno
!
    AS_ALLOCATE(vl=noeubord, size=nbno)
!
    call dismoi('NB_EC', 'DEPL_R', 'GRANDEUR', repi=nbec)
    call jeveuo(jexnum(pfchno//'.PRNO', 1), 'L', jprno)
    call jeveuo(noeub//'.VALE', 'L', jval)
    eps = 1.d-06
    nbnob = 0
    do ino = 1, nbno
        numeq = zi(jprno-1+ (nbec+2)* (ino-1)+1)
        nbcmp = zi(jprno-1+ (nbec+2)* (ino-1)+2)
        xnorm = 0.d0
        do icmp = 1, nbcmp
            xnorm = xnorm + zr(jval-1+numeq-1+icmp)**2
        end do
        if (xnorm .le. eps) then
            noeubord(ino) = .false.
        else
            noeubord(ino) = .true.
            nbnob = nbnob + 1
        endif
    end do
    AS_ALLOCATE(vi=nbpatchmil, size=nbno)
    AS_ALLOCATE(vi=numnb, size=nbnob)
    call wkvect('&&SINOZ2.NBPATCH', 'V V I', nbnob, jpa)
    inob = 0
    do ino = 1, nbno
        if (noeubord(ino)) then
            inob = inob + 1
            numnb(inob) = ino
        endif
    end do
    if (inob .ne. nbnob) then
        call utmess('F', 'CALCULEL4_84')
    endif
!
!    VERIFICATION DES TYPES DE MAILLE
!
    ntri = 0
    nqua = 0
    do ima = 1, nbma
        iad = iatyma - 1 + ima
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iad)), typema)
        if (typema(1:4) .eq. 'TRIA') then
            ntri = ntri + 1
            if (typema(5:5) .ne. '3' .and. typema(5:5) .ne. '6') then
                call utmess('F', 'CALCULEL4_85')
            endif
        else if (typema(1:4).eq.'QUAD') then
            nqua = nqua + 1
            if (typema(5:5) .ne. '4' .and. typema(5:5) .ne. '8' .and. typema(5:5) .ne. '9') then
                call utmess('F', 'CALCULEL4_86')
            endif
        endif
    end do
    if (ntri .ne. 0 .and. nqua .ne. 0) then
        call utmess('F', 'CALCULEL4_87')
    endif
    if (ntri .eq. 0 .and. nqua .eq. 0) then
        call utmess('F', 'CALCULEL4_88')
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
    do ino = 1, nbno
        if (.not.noeubord(ino)) then
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
                do ima = 1, nbmav
                    numav = zi(iamav-1+ima)
                    call jelira(jexnum(connex, numav), 'LONMAX', nbnoma)
                    call jeveuo(jexnum(connex, numav), 'L', ianov)
                    do inoma = 1, nbnoma
                        num = zi(ianov-1+inoma)
                        x(inoma) = zr(iacoor-1+3* (num-1)+1)
                        y(inoma) = zr(iacoor-1+3* (num-1)+2)
                        if (x(inoma) .le. xmin) xmin = x(inoma)
                        if (y(inoma) .le. ymin) ymin = y(inoma)
                        if (x(inoma) .ge. xmax) xmax = x(inoma)
                        if (y(inoma) .ge. ymax) ymax = y(inoma)
                    end do
                end do
!
!      BOUCLE SUR LES MAILLES VOISINES
!
                do ima = 1, nbmav
                    numav = zi(iamav-1+ima)
                    numgr = zi(iarepe-1+2*(numav-1)+1)
                    numel = zi(iarepe-1+2*(numav-1)+2)
                    call jelira(jexnum(connex, numav), 'LONMAX', nbnoma)
                    call jeveuo(jexnum(connex, numav), 'L', ianov)
!
!        RECUPERATION DES COORDONNEES DES NOEUDS
!
                    do inoma = 1, nbnoma
                        num = zi(ianov-1+inoma)
!    SI NOEUD BORD
                        if (noeubord(num)) then
                            call zzappa(num, zi(ianew), nbnobp, app)
                            if (.not.app) then
                                nbnobp = nbnobp + 1
                                zi(ianew-1+nbnobp) = num
                            endif
                        endif
                        x(inoma) = zr(iacoor-1+3* (num-1)+1)
                        y(inoma) = zr(iacoor-1+3* (num-1)+2)
                    end do
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
                end do
                ianew = ianob + nbnobp
!
!   PRECONDITIONNEMENT PAR LA DIAGONALE ET RESOLUTION
!
                call predia(a, b, diag, nno)
                call mtcrou(a, b, 9, nno, 4,&
                            wk1, wk2)
!
                do ic = 1, 4
                    do i = 1, nno
                        b(i,ic) = b(i,ic)*diag(i)
                    end do
                end do
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
                do inob = 1, nbnobp
                    num = zi(ianob-1+inob)
                    do k = 1, nbnob
                        numc = numnb(k)
                        if (numc .eq. num) then
                            zi(jpa-1+k) = zi(jpa-1+k) + 1
                        endif
                    end do
                    xinob = zr(iacoor-1+3* (num-1)+1)
                    yinob = zr(iacoor-1+3* (num-1)+2)
                    xinob = -1.d0 + 2.d0* (xinob-xmin)/ (xmax-xmin)
                    yinob = -1.d0 + 2.d0* (yinob-ymin)/ (ymax-ymin)
                    call zzpoly(nno, num, xinob, yinob, zr(jsig),&
                                b)
                end do
                call jedetr('&&SINOZ2.NOEBOPA')
!
!    TRAITEMENT DES NOEUDS MILIEUX (NON BORD)
!
                if (nno .ge. 6) then
                    do ima = 1, nbmav
                        numav = zi(iamav-1+ima)
                        call jelira(jexnum(connex, numav), 'LONMAX', nbnoma)
                        call jeveuo(jexnum(connex, numav), 'L', ianov)
!
!       RECUPERATION DU NOEUD MILIEU ASSOCIE AU NOEUD SOMMET DU PATCH
!             (1 SEUL PAR ELEMENT VOISIN)
!
                        do inoma = 1, nbnoma
                            num = zi(ianov-1+inoma)
                            if (num .eq. ino) then
                                numloc = inoma
                            endif
                        end do
!
                        if (nno .eq. 6) numloc = numloc + 3
                        if (nno .eq. 8) numloc = numloc + 4
                        if (nno .eq. 9) numloc = numloc + 4
                        num = zi(ianov-1+numloc)
                        nbpatchmil(num) = nbpatchmil(num) + 1
                        xinomi = zr(iacoor-1+3* (num-1)+1)
                        yinomi = zr(iacoor-1+3* (num-1)+2)
                        xinomi = -1.d0 + 2.d0* (xinomi-xmin)/ (xmax- xmin)
                        yinomi = -1.d0 + 2.d0* (yinomi-ymin)/ (ymax- ymin)
                        call zzpoly(nno, num, xinomi, yinomi, zr(jsig),&
                                    b)
                    end do
!
                endif
!
!    TRAITEMENT DU NOEUD BARYCENTRE (CAS DES Q9)
!
                if (nno .eq. 9) then
                    do ima = 1, nbmav
                        numav = zi(iamav-1+ima)
                        call jelira(jexnum(connex, numav), 'LONMAX', nbnoma)
                        call jeveuo(jexnum(connex, numav), 'L', ianov)
!
!     RECUPERATION DU NOEUD BARYCENTRE
!
                        num = zi(ianov-1+nbnoma)
                        nbpatchmil(num) = nbpatchmil(num) + 1
                        xinomi = zr(iacoor-1+3* (num-1)+1)
                        yinomi = zr(iacoor-1+3* (num-1)+2)
                        xinomi = -1.d0 + 2.d0* (xinomi-xmin)/ (xmax- xmin)
                        yinomi = -1.d0 + 2.d0* (yinomi-ymin)/ (ymax- ymin)
                        call zzpoly(nno, num, xinomi, yinomi, zr(jsig),&
                                    b)
                    end do
                endif
!
            endif
        endif
    end do
!
!    MOYENNAGE SUR LES NOEUDS BORD
!
    do i = 1, nbnob
        num = numnb(i)
        if (zi(jpa-1+i) .eq. 0) then
            call utmess('F', 'CALCULEL4_89')
        endif
        do ic = 1, 4
            zr(jsig-1+4* (num-1)+ic) = zr(jsig-1+4* (num-1)+ic)/ zi(jpa-1+i)
        end do
    end do
!
!    MOYENNAGE SUR LES NOEUDS NON BORD
!
    do ino = 1, nbno
!    SI PAS NOEUD BORD
        if (.not.noeubord(ino)) then
            if (nbpatchmil(ino) .eq. 0) then
                nbpatchmil(ino) = 1
            endif
            do ic = 1, 4
                zr(jsig-1+4* (ino-1)+ic) = zr( jsig-1+4* (ino-1)+ic )/ nbpatchmil(ino )
            end do
        endif
    end do
    call detrsd('CHAMP_GD', '&&VECASS')
    AS_DEALLOCATE(vi=numnb)
    call jedetr('&&SINOZ2.NBPATCH')
    AS_DEALLOCATE(vi=nbpatchmil)
    AS_DEALLOCATE(vl=noeubord)
    call jedetr('&&SINOZ2.CONINV')
    AS_DEALLOCATE(vi=longconinv)
    call jedetr(elrfam)
!
    call jedema()
end subroutine
