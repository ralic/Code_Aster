subroutine xoripe(modele)
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cesexi.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/normev.h"
#include "asterfort/panbno.h"
#include "asterfort/provec.h"
#include "asterfort/utmasu.h"
#include "asterfort/vdiff.h"
#include "asterfort/vecini.h"
#include "asterfort/wkvect.h"
#include "blas/ddot.h"
    character(len=8) :: modele
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
!        ORIENTER LES SOUS-ELEMENTS DE PEAU DES ELEMENTS X-FEM
!      (ET CALCUL DE HEAV SUR LES BORDS COINCIDANT AVEC INTERACE)
!
!  IN/OUT         MODELE    : NOM DE L'OBJET MODELE
!
!     ------------------------------------------------------------------
!
    real(kind=8) :: gbo(3), gpr(3), next(3), norme, lsn
    real(kind=8) :: co(3, 3), ab(3), ac(3), n2d(3), a(3), b(3), c(3)
    integer :: ima, nbma, j, ier, kk, i, jmofis, ifis, jnfis, nfis, iad2
    integer :: jmail, nbmail, iret, jcoor, jm3d, ibid, jvecno
    integer :: numapr, numab, nbnopr, nbnobo, nbnose, nbnott(3)
    integer :: jconx1, jconx2, ino, nuno, jtypma, jtmdim
    integer :: ich, jcesd(5), jcesv(5), jcesl(5), iad, nse, ise, in
    integer :: ndime, icmp, ndim, id(3), intemp, nseori, ifm, niv, nncp
    integer :: s1, s2, jgrp, nmaenr, jlsnv, jlsnd, jlsnl
    integer :: nsignp, nsignm, nsignz, ihe, he, itypma
    integer :: ifiss, nfiss
    character(len=8) :: noma, k8bid, typbo, fiss
    character(len=2) :: kdim
    character(len=19) :: ligrel, chs(5), chlsn
    character(len=24) :: grmape, nomob, vecnor, grp(3)
    character(len=19) :: pintto, cnseto, loncha, heav
    integer :: itypbo
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
!
!     INITIALISATION DU NOMBRE DE SOUS-ELEMENTS RE-ORIENTES
    nseori=0
!
    ligrel = modele//'.MODELE'
!
!     1.RECUPERATION D'INFORMATIONS DANS MODELE
!
    call jeveuo(modele//'.NFIS', 'L', jnfis)
    nfis = zi(jnfis)
    call jeveuo(modele//'.FISS', 'L', jmofis)
!
!     RECUPERATION DU MAILLAGE ASSOCIE AU MODELE :
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                noma, ier)
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                k8bid, ibid)
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                k8bid, ibid)
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo('&CATA.TM.TMDIM', 'L', jtmdim)
    call jeveuo(noma//'.TYPMAIL', 'L', jtypma)
!
    chlsn = '&&XORIPE.CHLSN'
    call celces(modele//'.LNNO', 'V', chlsn)
    call jeveuo(chlsn//'.CESL', 'L', jlsnl)
    call jeveuo(chlsn//'.CESD', 'L', jlsnd)
    call jeveuo(chlsn//'.CESV', 'L', jlsnv)
!
!     ------------------------------------------------------------------
!     I°) CREATION DE LA LISTE DES NUMEROS DES MAILLES DE PEAU ENRICHIES
!     ------------------------------------------------------------------
!
    grmape='&&XORIPE.GRMAPE'
    call wkvect(grmape, 'V V I', nbma, jmail)
!
!     INITIALISATION DU NOMBRE DE MAILLES DE LA LISTE
    nbmail=0
!
    do 20 ifis = 1, nfis
!
        fiss = zk8(jmofis-1 + ifis)
!
!       REMPLISSAGE DE LA LISTE
        grp(1) = fiss//'.MAILFISS.HEAV'
        grp(2) = fiss//'.MAILFISS.CTIP'
        grp(3) = fiss//'.MAILFISS.HECT'
!
!       BOUCLE SUR LES 3 GROUPES : HEAV, CTIP ET HECT
        do 100 kk = 1, 3
!
            call jeexin(grp(kk), iret)
            if (iret .ne. 0) then
                call jeveuo(grp(kk), 'L', jgrp)
                call jelira(grp(kk), 'LONMAX', nmaenr)
!
!           BOUCLE SUR LES MAILLES DE CHAQUE GROUPE
                do 120 i = 1, nmaenr
                    ima = zi(jgrp-1+i)
!             NDIME : DIMENSION TOPOLOGIQUE DE LA MAILLE
                    ndime= zi(jtmdim-1+zi(jtypma-1+ima))
                    if (ndim .eq. ndime+1) then
                        nbmail=nbmail+1
                        zi(jmail-1+nbmail)=ima
                    endif
120              continue
!
            endif
100      continue
!
20  end do
    if (nbmail .eq. 0) goto 999
!
!     ------------------------------------------------------------------
!     II°) RECHERCHE DES MAILLES SUPPORT
!     ------------------------------------------------------------------
    ASSERT(ndim.eq.2.or.ndim.eq.3)
    if (ndim .eq. 2) kdim='2D'
    if (ndim .eq. 3) kdim='3D'
!
    nomob = '&&XORIPE.NU_MAILLE_3D'
!
    call utmasu(noma, kdim, nbmail, zi(jmail), nomob,&
                zr(jcoor), 0, 0, .false.)
    call jeveuo(nomob, 'L', jm3d)
!
!     ------------------------------------------------------------------
!     III°) CREATION DU VECTEUR DES NORMALES SORTANTES
!     ------------------------------------------------------------------
!
    vecnor='&&XORIPE.VECNOR'
    call wkvect(vecnor, 'V V R', nbmail*ndim, jvecno)
!
    do 300 ima = 1, nbmail
!       NUMEROS DES MAILLES PRINCIPALE ET DE BORD
        numab=zi(jmail-1+ima)
        numapr=zi(jm3d-1+ima)
!
!       NOMBRES DE NOEUDS DES MAILLES PRINCIPALE ET DE BORD
        nbnobo=zi(jconx2+numab) - zi(jconx2+numab-1)
        nbnopr=zi(jconx2+numapr) - zi(jconx2+numapr-1)
!
!       GBO : CENTRE DE GRAVITÉ DE LA MAILLE DE BORD
        call vecini(3, 0.d0, gbo)
        do 310 ino = 1, nbnobo
            nuno=zi(jconx1-1+zi(jconx2+numab-1)+ino-1)
            do 311 j = 1, ndim
                gbo(j)=gbo(j)+zr(jcoor-1+3*(nuno-1)+j)/nbnobo
311          continue
310      continue
!
!     GPR : CENTRE DE GRAVITÉ DE LA MAILLE PRICIPALE
        call vecini(3, 0.d0, gpr)
        do 320 ino = 1, nbnopr
            nuno=zi(jconx1-1+zi(jconx2+numapr-1)+ino-1)
            do 321 j = 1, ndim
                gpr(j)=gpr(j)+zr(jcoor-1+3*(nuno-1)+j)/nbnopr
321          continue
320      continue
!
!       NORMALE EXTERIEURE : NEXT = GBO - GPR
        call vecini(3, 0.d0, next)
        call vdiff(3, gbo, gpr, next)
        call normev(next, norme)
!
        do 330 j = 1, ndim
            zr(jvecno-1+ndim*(ima-1)+j)=next(j)
330      continue
!
300  end do
!
!
!
!
!     ------------------------------------------------------------------
!     IV°) ORIENTATION DES SOUS-ELEMENTS
!     ------------------------------------------------------------------
!
    chs(1) = '&&XORIPE.PINTTO'
    chs(2) = '&&XORIPE.CNSETO'
    chs(3) = '&&XORIPE.LONCHA'
    chs(4) = '&&XORIPE.HEAV'
!
    pintto = modele//'.TOPOSE.PIN'
    cnseto = modele//'.TOPOSE.CNS'
    loncha = modele//'.TOPOSE.LON'
    heav = modele//'.TOPOSE.HEA'
!
    call celces(pintto, 'V', chs(1))
    call celces(cnseto, 'V', chs(2))
    call celces(loncha, 'V', chs(3))
    call celces(heav, 'V', chs(4))
!
    do 40 ich = 1, 4
        call jeveuo(chs(ich)//'.CESD', 'L', jcesd(ich))
        call jeveuo(chs(ich)//'.CESV', 'E', jcesv(ich))
        call jeveuo(chs(ich)//'.CESL', 'L', jcesl(ich))
40  end do
!
    do 400 ima = 1, nbmail
        do 401 j = 1, ndim
            next(j)=zr(jvecno-1+ndim*(ima-1)+j)
401      continue
!
        numab =zi(jmail-1+ima)
! --- CA NE SERT A RIEN DE RECUPERER NDIME CAR ON A SELECTIONNÉ NUMAB
! --- TEL QUE NDIME = NDIM-1 (BOUCLE 120)
        ndime= zi(jtmdim-1+zi(jtypma-1+numab))
        nfiss = zi(jcesd(4)-1+5+4*(numab-1)+2)
        numapr=zi(jm3d-1+ima)
        nbnopr=zi(jconx2+numapr) - zi(jconx2+numapr-1)
!
        itypma=zi(jtypma-1+numapr)
        call panbno(itypma, nbnott)
        if (ndim .eq. 2) then
            itypbo=zi(jtypma-1+numab)
            call jenuno(jexnum('&CATA.TM.NOMTM', itypbo), typbo)
            nbnobo=zi(jconx2+numab) - zi(jconx2+numab-1)
            nbnose=nbnobo
        else
            nbnose=3
        endif
!
!       RECUPERATION DE LA SUBDIVISION LA MAILLE DE PEAU EN NIT
!       SOUS-ELEMENTS
        call cesexi('S', jcesd(3), jcesl(3), numab, 1,&
                    1, 1, iad)
        nse=zi(jcesv(3)-1+iad)
!
!         BOUCLE SUR LES NSE SOUS-ELEMENTS
        do 420 ise = 1, nse
!
!         CO(J,IN) : JEME COORDONNEE DU INEME SOMMET DU SOUS-ELEMENT
            do 421 in = 1, nbnose
                icmp=nbnose*(ise-1)+in
                call cesexi('S', jcesd(2), jcesl(2), numab, 1,&
                            1, icmp, id( in))
                ino=zi(jcesv(2)-1+id(in))
                if (ino .lt. 1000) then
                    nuno=zi(jconx1-1+zi(jconx2+numab-1)+ino-1)
!
                    do 422 j = 1, ndim
                        co(j,in)=zr(jcoor-1+3*(nuno-1)+j)
422                  continue
                else if (ino.gt.1000 .and. ino.lt.2000) then
                    do 423 j = 1, ndim
                        icmp=ndim*(ino-1000-1)+j
                        call cesexi('S', jcesd(1), jcesl(1), numab, 1,&
                                    1, icmp, iad)
                        co(j,in)=zr(jcesv(1)-1+iad)
423                  continue
                endif
421          continue
!
            do 430 j = 1, ndim
                a(j) = co(j,1)
                b(j) = co(j,2)
                if (ndim .eq. 3) c(j) = co(j,3)
430          continue
            if (ndim .eq. 2) then
                a(3) = 0.d0
                b(3) = 0.d0
            endif
!
!         NORMALE AU SOUS-ELEMENT 2D
            call vecini(3, 0.d0, ab)
            call vdiff(3, b, a, ab)
            call vecini(3, 0.d0, n2d)
            if (ndim .eq. 3) then
                call vecini(3, 0.d0, ac)
                call vdiff(3, c, a, ac)
                call provec(ab, ac, n2d)
            else if (ndim.eq.2) then
                n2d(1) = ab(2)
                n2d(2) = -ab(1)
                n2d(3) = 0
            endif
            call normev(n2d, norme)
!
!
!         PRODUIT SCALAIRE DES NORMALES : N2D.NEXT
            if (ddot(ndim,n2d,1,next,1) .lt. 0.d0) then
!           ON INVERSE LES SOMMETS S1 ET S2
!           (ON INVERSE 1 ET 2 EN 2D
                s1 = ndim-1
                s2 = ndim
!            ON INVERSE 2 ET 3 EN 3D)
                nseori=nseori+1
                intemp=zi(jcesv(2)-1+id(s1))
                zi(jcesv(2)-1+id(s1))=zi(jcesv(2)-1+id(s2))
                zi(jcesv(2)-1+id(s2))=intemp
            endif
!
!         ON MODIFIE HEAVISIDE SI BORD COINCIDANT AVEC INTERFACE
!         RECUPERATION DE LA VALEUR DE LA FONCTION HEAVISIDE
            do 450 ifiss = 1, nfiss
                ihe = ise
                call cesexi('S', jcesd(4), jcesl(4), numab, 1,&
                            ifiss, ihe, iad)
                he=zi(jcesv(4)-1+iad)
                ASSERT(he.eq.- 1.or.he.eq.1.or.he.eq.0.or.he.eq.99)
                if (he .eq. 99) then
!             VERIF QUE C'EST NORMAL
                    ASSERT(nse.eq.1)
!             SIGNE LEVEL SET SUR LA MAILLE PRINCIPALE
                    nsignp=0
                    nsignm=0
                    nsignz=0
!             LSN SUR LES NOEUDS SOMMETS DE LA MAILLE PRINCIPALE
                    do 440 ino = 1, nbnott(1)
                        call cesexi('S', jlsnd, jlsnl, numapr, ino,&
                                    ifiss, 1, iad2)
!                NUNO=ZI(JCONX1-1+ZI(JCONX2+NUMAPR-1)+INO-1)
                        lsn = zr(jlsnv-1+iad2)
                        if (lsn .gt. 0.d0) nsignp = nsignp +1
                        if (lsn .eq. 0.d0) nsignz = nsignz +1
                        if (lsn .lt. 0.d0) nsignm = nsignm +1
440                  continue
                    ASSERT(nsignz.ne.0)
!             REMARQUE : LES DEUX TESTS SUIVANTS NE SONT PAS CORRECTS
!             VOIR FICHE 13265
!              ASSERT(NSIGNP+NSIGNM.NE.0)
!              ASSERT(NSIGNP*NSIGNM.EQ.0)
!             ON ECRIT HE
                    if (nsignp .gt. 0) zi(jcesv(4)-1+iad)= 1
                    if (nsignm .gt. 0) zi(jcesv(4)-1+iad)=-1
                endif
450          continue
!
420      continue
!
400  end do
!
!     ON SAUVE LES NOUVEAUX CHAM_ELEM MODIFIES A LA PLACE DES ANCIENS
    call cescel(chs(2), ligrel, 'TOPOSE', 'PCNSETO', 'OUI',&
                nncp, 'G', cnseto, 'F', ibid)
    call cescel(chs(4), ligrel, 'TOPOSE', 'PHEAVTO', 'OUI',&
                nncp, 'G', heav, 'F', ibid)
!     ------------------------------------------------------------------
!     FIN
!     ------------------------------------------------------------------
!
    call jedetr('&&XORIPE.NU_MAILLE_3D')
    call jedetr('&&XORIPE.VECNOR')
!
999  continue
!
    write(ifm,*)'NOMBRE DE SOUS-ELEMENTS DE PEAU RE-ORIENTES :',nseori
!
    call jedetr('&&XORIPE.GRMAPE')
!
    call jedema()
end subroutine
