subroutine xlagsp(noma, nomo, fiss, algola, ndim,&
                  nliseq)
!
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
! person_in_charge: samuel.geniaut at edf.fr
!
! aslint: disable=W1306
    implicit none
#include "jeveux.h"
!
#include "asterc/getexm.h"
#include "asterc/getvtx.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/conare.h"
#include "asterfort/detrsd.h"
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
#include "asterfort/utimsd.h"
#include "asterfort/wkvect.h"
#include "asterfort/xlag2c.h"
#include "asterfort/xlagsc.h"
#include "asterfort/xxmmvd.h"
    character(len=8) :: noma, nomo, fiss
    integer :: ndim
    integer :: algola
    character(len=19) :: nliseq
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (PREPARATION)
!
! CHOIX DE L'ESPACE DES LAGRANGES POUR LE CONTACT
!                   (VOIR BOOK VI 15/07/05) :
!    - DETERMINATION DES NOEUDS
!    - CREATION DES RELATIONS DE LIAISONS ENTRE LAGRANGE
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NOMA   : NOM DE L'OBJET MAILLAGE
! IN  ALGOLA : TYPE DE CREATION DES RELATIONS DE LIAISONS ENTRE LAGRANGE
! OUT NLISEQ : LISTE RELATIONS EGALITE
!
!
!
!
    integer :: nbcmp
    parameter    (nbcmp = 12)
!
    integer :: nbno, nbar, nbarto, itypma
    integer :: ar(12, 3), na, nb, nunoa, mxar
    integer :: nunob, nunom, nunoaa, nunobb
    integer :: ia, iia, ia1, ia2, i, k, iret, ima, jma
    integer :: jconx1, jconx2, jmail
    integer :: npil
    real(kind=8) :: c(ndim), cc(ndim)
    character(len=8) :: k8bid, typma
    integer :: ifm, niv
    character(len=19) :: tabno, tabint, tabcri
    integer :: jtabno, jtabin, jtabcr
    integer :: zxbas, zxain
    real(kind=8) :: lon, dist1, dist2
    logical :: lmulti
    character(len=19) :: chsoe, chslo, chsba, chsai
    integer :: jcesl2, jcesl3, jcesl4, jcesl5
    integer :: jcesd2, jcesd3, jcesd4, jcesd5
    integer :: jcesv2, jcesv3, jcesv4, jcesv5
    integer :: iad2, iad3, iad4, ninter, pint, ifiss
    character(len=24) :: grp(3), gr
    integer :: nmaenr, ienr, jgrp, jxc, ier, jnbpt
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
!
! --- INITIALISATIONS
!
    chsoe = '&&XLAGSP.CHSOE'
    chslo = '&&XLAGSP.CHSLO'
    chsba = '&&XLAGSP.CHSBA'
    chsai = '&&XLAGSP.CHSAI'
!
! --- ON TRANSFORME LE CHAMP TOPOFAC.LO EN CHAMP SIMPLE
!
    call celces(nomo//'.TOPOFAC.LO', 'V', chslo)
    call jeveuo(chslo//'.CESD', 'L', jcesd2)
    call jeveuo(chslo//'.CESV', 'L', jcesv2)
    call jeveuo(chslo//'.CESL', 'L', jcesl2)
!
! --- ON TRANSFORME LE CHAMP TOPOFAC.AI EN CHAMP SIMPLE
!
    zxain = xxmmvd('ZXAIN')
    call celces(nomo//'.TOPOFAC.AI', 'V', chsai)
    call jeveuo(chsai//'.CESD', 'L', jcesd3)
    call jeveuo(chsai//'.CESV', 'L', jcesv3)
    call jeveuo(chsai//'.CESL', 'L', jcesl3)
!
! --- ON TRANSFORME LE CHAMP TOPOFAC.OE EN CHAMP SIMPLE
!
    call celces(nomo//'.TOPOFAC.OE', 'V', chsoe)
    call jeveuo(chsoe//'.CESD', 'L', jcesd4)
    call jeveuo(chsoe//'.CESV', 'L', jcesv4)
    call jeveuo(chsoe//'.CESL', 'L', jcesl4)
!
! --- ON TRANSFORME LE CHAMP TOPOFAC.BA EN CHAMP SIMPLE
!
    zxbas = xxmmvd('ZXBAS')
    call celces(nomo//'.TOPOFAC.BA', 'V', chsba)
    call jeveuo(chsba//'.CESD', 'L', jcesd5)
    call jeveuo(chsba//'.CESV', 'L', jcesv5)
    call jeveuo(chsba//'.CESL', 'L', jcesl5)
!
! --- RECUPERATION DE DONNEES RELATIVES AU MAILLAGE
!
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbno,&
                k8bid, iret)
    call jeveuo(noma(1:8)//'.TYPMAIL', 'L', jma)
    call jeveuo(noma(1:8)//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma(1:8)//'.CONNEX', 'LONCUM'), 'L', jconx2)
! --- RECUPERATION DES MAILLES DU MODELE
    call jeveuo(nomo//'.MAILLE', 'L', jmail)
! --- LE MULTI-HEAVISIDE EST-IL ACTIF ?
    call jeexin(nomo//'.FISSNO    .CELD', ier)
    if (ier .ne. 0) then
        lmulti = .true.
    else
        lmulti = .false.
        ifiss = 1
    endif
! --- RECUPERATION DU COMPTAGE DES FISSURES VUES PAR LES MAILLES
    if (lmulti) call jeveuo('&&XCONTA.NBSP', 'E', jnbpt)
!
! --- DIMENSIONNEMENT DU NOMBRE MAXIMUM D'ARETES COUPEES PAR LA FISSURE
! --- PAR LE NOMBRE DE NOEUDS DU MAILLAGE (AUGMENTER SI NECESSAIRE)
!
    mxar = nbno
!
    nbarto = 0
    ASSERT(nbcmp.eq.zxbas)
    tabno = '&&XLAGSP.TABNO'
    tabint = '&&XLAGSP.TABINT'
    tabcri = '&&XLAGSP.TABCRI'
!
!
! --- CREATION OBJETS DE TRAVAIL
! --- TABNO  : COL 1,2     : NOEUDS EXTREMITE
!            : COL 3       : NOEUD MILIEU
! --- TABINT : COL 1,2(,3) : COORDONNEES DU POINT D'INTERSECTION
!
    call wkvect(tabno, 'V V I', 3*mxar, jtabno)
    call wkvect(tabint, 'V V R', ndim*mxar, jtabin)
    call wkvect(tabcri, 'V V R', 1*mxar, jtabcr)
!
! --- CREATION DE LA LISTE DES ARETES COUPEES
!
!
    grp(1) = fiss//'.MAILFISS.HEAV'
    grp(2) = fiss//'.MAILFISS.CTIP'
    grp(3) = fiss//'.MAILFISS.HECT'
!
! --- REPERAGE NUM LOCAL DE FISSURE POUR CHAQUE MAILLE
! --- ENRICHIE
!
    do 10 k = 1, 3
        call jeexin(grp(k), iret)
        if (iret .eq. 0) goto 10
        call jeveuo(grp(k), 'L', jgrp)
        call jelira(grp(k), 'LONMAX', nmaenr)
!
! --- BOUCLE SUR LES MAILLES DU GROUPE
!
        do 11 ienr = 1, nmaenr
            ima = zi(jgrp-1+ienr)
            if (lmulti) then
                zi(jnbpt-1+ima) = zi(jnbpt-1+ima)+1
            endif
11      continue
10  end do
!
! --- RECUP MAILLES DE CONTACT
!
    gr = fiss//'.MAILFISS.CONT'
    call jeexin(gr, iret)
    if (iret .eq. 0) goto 99
    call jeveuo(gr, 'L', jgrp)
    call jelira(gr, 'LONMAX', nmaenr)
!
! --- BOUCLE SUR LES MAILLES DE CONTACT
!
    do 100 ienr = 1, nmaenr
        ima = zi(jgrp-1+ienr)
        if (lmulti) ifiss = zi(jnbpt-1+ima)
        itypma = zi(jma-1+ima)
        call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
        call jeveuo(nomo(1:8)//'.XFEM_CONT', 'L', jxc)
!
! --- RECUPERATION DU NOMBRE DE POINT D'INTERSECTIONS
!
        call cesexi('C', jcesd2, jcesl2, ima, 1,&
                    ifiss, 1, iad2)
        ninter = zi(jcesv2-1+iad2)
! --- NINTER DOIT DÉPENDRE DE LA FISS QUI COUPE SI ELEMENT XH2C,3C OU 4C
!          IF (LMULTI) THEN
!            IF (ENR.EQ.'XH2C'.OR.ENR.EQ.'XH3C'.OR.ENR.EQ.'XH4C') THEN
!              CALL CESEXI('S',JCESD2,JCESL2,IMA,1,1,4,IAD2)
!              IF (ZI(JCESV2-1+IAD2).NE.ZI(JNBPT-1+IMA)) NINTER = 0
!            ENDIF
!          ENDIF
!
! --- RECUPERATION DE LA CONNECTIVITE DES ARETES
!
        call conare(typma, ar, nbar)
!
! --- BOUCLE SUR LES POINTS D'INTERSECTIONS
!
        do 110 pint = 1, ninter
!
! --- NUMERO DE L'ARETE INTERSECTEES
!
            call cesexi('S', jcesd3, jcesl3, ima, 1,&
                        ifiss, zxain*(pint-1)+ 1, iad3)
            ASSERT(iad3.gt.0)
            ia=nint(zr(jcesv3-1+iad3))
! - SI PILOTAGE ET NOEUD INTERSECTE, ON L AJOUTE
            if (getexm('PILOTAGE','DIRE_PILO') .eq. 1) then
                call getvtx('PILOTAGE', 'DIRE_PILO', 1, iarg, 0,&
                            k8bid, npil)
                npil=-npil
                if (npil .ge. 1) then
                    if (ia .eq. 0) then
                        call cesexi('S', jcesd3, jcesl3, ima, 1,&
                                    ifiss, zxain*(pint-1)+2, iad3)
                        na=nint(zr(jcesv3-1+iad3))
                        nb=na
                    else
                        na = ar(ia,1)
                        nb = ar(ia,2)
                    endif
                endif
! --- SI CE N'EST PAS UNE ARETE COUPEE, ON SORT
            else
                if (ia .eq. 0) goto 110
                na = ar(ia,1)
                nb = ar(ia,2)
            endif
!
! --- RECUPERATION DES NOEUDS
!
            nunoa = zi(jconx1-1+zi(jconx2+ima-1)+na-1)
            nunob = zi(jconx1-1+zi(jconx2+ima-1)+nb-1)
            nunom=0
!
! --- EST-CE QUE L'ARETE EST DEJA VUE ?
!
            do 120 i = 1, nbarto
!             ARETE DEJA VUE
                if (nunoa .eq. zi(jtabno-1+3*(i-1)+1) .and. nunob .eq. zi( jtabno-1+3*(i-1)+2)) &
                goto 110
                if (nunoa .eq. zi(jtabno-1+3*(i-1)+2) .and. nunob .eq. zi( jtabno-1+3*(i-1)+1)) &
                goto 110
120          continue
!
! --- NOUVELLE ARETE
!
            nbarto = nbarto+1
            ASSERT(nbarto.lt.mxar)
            zi(jtabno-1+3*(nbarto-1)+1) = nunoa
            zi(jtabno-1+3*(nbarto-1)+2) = nunob
            zi(jtabno-1+3*(nbarto-1)+3) = nunom
            do 130 i = 1, ndim
                call cesexi('S', jcesd4, jcesl4, ima, 1,&
                            ifiss, ndim*(pint- 1)+i, iad4)
                ASSERT(iad4.gt.0)
                c(i) = zr(jcesv4-1+iad4)
                zr(jtabin-1+ndim*(nbarto-1)+i) = c(i)
130          continue
!
110      continue
!
100  continue
99  continue
!
!
! --- CRITERE POUR DEPARTAGER LES ARETES HYPERSTATIQUES:
!     LONGUEUR DE FISSURE CONTROLÏ¿ŒE, I.E.
!     SOMME DES LONGUEURS DES ARETES DES FACETTES
!     DE CONTACT CONNECTEES A CHAQUE ARETE
!
    do 200 ia = 1, nbarto
        nunoa = zi(jtabno-1+3*(ia-1)+1)
        nunob = zi(jtabno-1+3*(ia-1)+2)
        nunom = zi(jtabno-1+3*(ia-1)+3)
        do 210 i = 1, ndim
            c(i)=zr(jtabin-1+ndim*(ia-1)+i)
210      continue
        dist1=r8maem()
        dist2=r8maem()
        ia1=0
        ia2=0
!
        do 220 iia = 1, nbarto
            nunoaa = zi(jtabno-1+3*(iia-1)+1)
            nunobb = zi(jtabno-1+3*(iia-1)+2)
            if ((nunoa.eq.nunoaa.and.nunob.ne.nunobb) .or.&
                ( nunoa.eq.nunobb.and.nunob.ne.nunoaa)) then
!           NUNOA CONNECTE LES DEUX ARETES
                do 300 i = 1, ndim
                    cc(i)=zr(jtabin-1+ndim*(iia-1)+i)
300              continue
                lon=0.d0
                do 310 i = 1, ndim
                    lon = lon+(cc(i)-c(i))*(cc(i)-c(i))
310              continue
                lon=sqrt(lon)
                if (lon .lt. dist1) then
                    dist1=lon
                    ia1=iia
                endif
            endif
            if ((nunoa.ne.nunoaa.and.nunob.eq.nunobb) .or.&
                ( nunoa.ne.nunobb.and.nunob.eq.nunoaa)) then
!           NUNOB CONNECTE LES DEUX ARETES
                do 320 i = 1, ndim
                    cc(i)=zr(jtabin-1+ndim*(iia-1)+i)
320              continue
                lon=0.d0
                do 330 i = 1, ndim
                    lon = lon+(cc(i)-c(i))*(cc(i)-c(i))
330              continue
                lon=sqrt(lon)
                if (lon .lt. dist2) then
                    dist2=lon
                    ia2=iia
                endif
            endif
220      continue
        lon=0.d0
        if (ia2 .ne. 0) then
            lon=lon+dist2
        endif
        if (ia1 .ne. 0) then
            lon=lon+dist1
        endif
!
        zr(jtabcr-1+1*(ia-1)+1)=lon
!
200  end do
!
! --- CREATION DES LISTES DES RELATIONS DE LIAISONS ENTRE LAGRANGE
!
    call xlagsc(ndim, nbno, nbarto, mxar, algola,&
                jtabno, jtabin, jtabcr, fiss, nliseq)
!
! --- SI LE MULTI-HEAVISIDE EST ACTIF, ON CREE UNE SD SUPPLEMENTAIRE
! --- CONTENANT LE NUMÉROS DE LAGRANGIEN CORESPONDANT.
!
    if (lmulti) call xlag2c(nomo, nliseq, jnbpt)
!
! --- DESTRUCTION DES OBJETS TEMPORAIRES
!
    call jedetr(tabno)
    call jedetr(tabint)
    call jedetr(tabcri)
    call detrsd('CHAM_ELEM_S', chsoe)
    call detrsd('CHAM_ELEM_S', chslo)
    call detrsd('CHAM_ELEM_S', chsai)
!
! --- AFFICHAGE LISTE REL. LINEAIRES
!
    if (niv .ge. 2) then
        write(ifm,*) '<XFEM  > LISTE DES RELATIONS LINEAIRES'
        call utimsd(ifm, -1, .true., .true., nliseq,&
                    1, ' ')
    endif
!
    call jedema()
end subroutine
