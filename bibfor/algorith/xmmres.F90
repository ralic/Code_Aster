subroutine xmmres(depdel, modele, veasse, cnsinr)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/conare.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/nmchex.h"
#include "asterfort/normev.h"
#include "asterfort/vecini.h"
#include "asterfort/wkvect.h"
#include "asterfort/xmafr1.h"
#include "asterfort/xmmred.h"
#include "asterfort/xxmmvd.h"
#include "blas/ddot.h"
!
    character(len=19) :: cnsinr
    character(len=19) :: veasse(*)
    character(len=19) :: depdel
    character(len=8) :: modele
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEM - POST-TRAITEMENT)
!
! CREER LE CHAM_NO_S POUR L ARCHIVAGE DU CONTACT PAR NMARCH
!
! ----------------------------------------------------------------------
!
!
! IN  DEPDEL : DEPLACEMENT AU PAS DE TEMPS COURANT
! IN  MODELE : NOM DU MODELE
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! OUT CNSINR : CHAM_NO_S POUR L'ARCHIVAGE DU CONTACT
!
!
!
!
    integer :: ifm, niv
    integer :: jcesv1, jcesv2, jcesv3, jcesv4, jcesv5
    integer :: jcesl1, jcesl2, jcesl3, jcesl4, jcesl5, jtmdim, ndime
    integer :: jcesd1, jcesd2, jcesd3, jcesd4, jcesd5
    integer :: jcont, jconl, jfrot, jfrol, jdepl, jlagv
    integer :: jcesk1, nbma, ima, iad, i, ia, in, iadb, k
    integer :: ninter, jmai, itypma, nuno, jconx1, jconx2, j, ndim
    integer :: jdepv, jcnsvr, jcnslr
    integer :: nsom, nosom(2), ar(12, 3), nbar, zresu
    integer :: zxain
    integer :: jxc
    integer ::  nbno
    character(len=8) :: noma, typma
    character(len=19) :: depdes, depcn, fcont, fconts
    character(len=19) :: fctcn, ffrot, ffrots, ffrocn, lagcn
    character(len=19) :: faclos, aintes, pintes, basecs
    character(len=19) :: faclon, ainter, pinter, baseco
    character(len=19) :: lst, lstno
    character(len=24) :: dejcal
    integer :: jdejca
!
    real(kind=8) :: xyz(3), jeu, ff(2), mult, cont
    real(kind=8) :: saut(3), glit(3), p(3, 3), n(3), gli, lagfro(3)
    real(kind=8) :: lagsf, rr, rnxyz(3), rn, alpha, longar, levels
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: rnx, rny, rnz, rtxyz(3), rtgx, rtgy, rtgz, rtax, rtay, rtaz
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- INITIALISATIONS
!
    faclos = '&&XMMRES.FACLOS'
    aintes = '&&XMMRES.AINTES'
    pintes = '&&XMMRES.PINTES'
    basecs = '&&XMMRES.BASECS'
    lstno = '&&XMMRES.LSTNO'
    cont = 0.d0
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma)
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
!
! --- ACCES SD CONTACT
!
    zresu = cfmmvd('ZRESU')
    zxain = xxmmvd('ZXAIN')
    call nmchex(veasse, 'VEASSE', 'CNELTC', fcont)
    call nmchex(veasse, 'VEASSE', 'CNELTF', ffrot)
!
! --- ACCES SD XFEM
!
    faclon = modele(1:8)//'.TOPOFAC.LO'
    ainter = modele(1:8)//'.TOPOFAC.AI'
    pinter = modele(1:8)//'.TOPOFAC.OE'
    baseco = modele(1:8)//'.TOPOFAC.BA'
    lst = modele(1:8)//'.LTNO'
    depdes = '&&XMMRES.DEPDES'
    depcn = '&&XMMRES.DEPCN'
    lagcn = '&&XMMRES.LAGCN'
    fconts = '&&XMMRES.CONT_S'
    fctcn = '&&MMMRES.FCTCN'
    ffrots = '&&MMMRES.FROT_S'
    ffrocn = '&&MMMRES.FROTCN'
!
! --- TRANSFORMATION DES CHAM_ELEM EN CHAM_ELEM_S
!
    call celces(faclon, 'V', faclos)
    call celces(ainter, 'V', aintes)
    call celces(pinter, 'V', pintes)
    call celces(baseco, 'V', basecs)
    call celces(lst, 'V', lstno)
!
! --- ACCES AUX CHAM_ELEM_S
!
    call jeveuo(faclos//'.CESD', 'L', jcesd1)
    call jeveuo(aintes//'.CESD', 'L', jcesd2)
    call jeveuo(pintes//'.CESD', 'L', jcesd3)
    call jeveuo(basecs//'.CESD', 'L', jcesd4)
    call jeveuo(lstno//'.CESD', 'L', jcesd5)
!
    call jeveuo(faclos//'.CESV', 'L', jcesv1)
    call jeveuo(aintes//'.CESV', 'L', jcesv2)
    call jeveuo(pintes//'.CESV', 'L', jcesv3)
    call jeveuo(basecs//'.CESV', 'L', jcesv4)
    call jeveuo(lstno//'.CESV', 'L', jcesv5)
!
    call jeveuo(faclos//'.CESL', 'L', jcesl1)
    call jeveuo(aintes//'.CESL', 'L', jcesl2)
    call jeveuo(pintes//'.CESL', 'L', jcesl3)
    call jeveuo(basecs//'.CESL', 'L', jcesl4)
    call jeveuo(lstno//'.CESL', 'L', jcesl5)
!
    ASSERT(zi(jcesd1).eq.zi(jcesd2))
    ASSERT(zi(jcesd1).eq.zi(jcesd3))
!
! --- DONNEES RELATIVES AU MAILLAGE
!
    call jeveuo(faclos//'.CESK', 'L', jcesk1)
    noma = zk8(jcesk1-1+1)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call jeveuo(noma//'.TYPMAIL', 'L', jmai)
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
    call jeveuo('&CATA.TM.TMDIM', 'L', jtmdim)
!
! --- SD TEMPORAIRE POUR VERIF NOEUDS DEJA CALCULES
!
    dejcal = '&&XMMRES.DEJCAL'
    call wkvect(dejcal, 'V V I', nbno, jdejca)
!
! --- TRANSFORMATION ET REDUCTION DES CHAM_NOEU
!
    call xmmred(ndim, depdel, depdes, lagcn, depcn,&
                fcont, fconts, fctcn, ffrot, ffrots,&
                ffrocn)
!
! --- ACCES CHAM_NO
!
    call jeveuo(lagcn//'.CNSV', 'L', jlagv)
    call jeveuo(depcn//'.CNSV', 'L', jdepv)
    call jeveuo(depcn//'.CNSL', 'L', jdepl)
    call jeveuo(fctcn//'.CNSV', 'L', jcont)
    call jeveuo(fctcn//'.CNSL', 'L', jconl)
    call jeveuo(ffrocn//'.CNSV', 'L', jfrot)
    call jeveuo(ffrocn//'.CNSL', 'L', jfrol)
!
! --- ACCES AU CHAM_NO_S VALE_CONT
!
    call jeveuo(cnsinr//'.CNSV', 'E', jcnsvr)
    call jeveuo(cnsinr//'.CNSL', 'E', jcnslr)
!
! --- BOUCLE SUR LES MAILLES
!
    do 100 ima = 1, nbma
!
        itypma=zi(jmai-1+ima)
        call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
!       NDIME : DIMENSION TOPOLOGIQUE DE LA MAILLE
        ndime= zi(jtmdim-1+itypma)
!
!       ON ZAPPE LES MAILLES DE BORD (MAILLES NON PRINCIPALES)
        if (ndim .ne. ndime) goto 100
!
        call conare(typma, ar, nbar)
!
        call cesexi('C', jcesd1, jcesl1, ima, 1,&
                    1, 1, iad)
        if (iad .le. 0) goto 100
!
        call jeveuo(modele//'.XFEM_CONT', 'L', jxc)
        ninter = zi(jcesv1-1+iad)
!
!       BOUCLE SUR LES POINTS D'INTERSECTION
        do 120 i = 1, ninter
            call cesexi('S', jcesd2, jcesl2, ima, 1,&
                        1, zxain*(i-1)+1, iad)
            ASSERT(iad.gt.0)
            ia = nint(zr(jcesv2-1+iad))
            ASSERT(ia.le.nbar)
            call cesexi('S', jcesd2, jcesl2, ima, 1,&
                        1, zxain*(i-1)+2, iad)
            ASSERT(iad.gt.0)
            in = nint(zr(jcesv2-1+iad))
            call cesexi('S', jcesd2, jcesl2, ima, 1,&
                        1, zxain*(i-1)+3, iad)
            ASSERT(iad.gt.0)
            longar =zr(jcesv2-1+iad)
            call cesexi('S', jcesd2, jcesl2, ima, 1,&
                        1, zxain*(i-1)+4, iad)
            ASSERT(iad.gt.0)
            alpha = zr(jcesv2-1+iad)
!
!         ADRESSE DU DEBUT MEMOIRE DE LA BASE COVARIANTE
            call cesexi('S', jcesd4, jcesl4, ima, 1,&
                        1, ndim*ndim*(i-1)+1, iadb)
            ASSERT(iadb.gt.0)
!
!         RECUPERATION DES COORDONNES DU POINT D'INTERSECTION
            xyz(3)=0.d0
            do 121 j = 1, ndim
                call cesexi('S', jcesd3, jcesl3, ima, 1,&
                            1, ndim*(i-1)+j, iad)
                ASSERT(iad.gt.0)
                xyz(j)=zr(jcesv3-1+iad)
121         continue
!
!         RECUP DES NUMEROS GLOBAUX DES NOEUDS DE L'ARETE/NOEUD SOMMET
            if (in .gt. 0) then
                nsom = 1
                nosom(1) = zi(jconx1-1+zi(jconx2+ima-1)+in-1)
                ff(1)=1.d0
                mult=0.5d0
            else if (ia.gt.0) then
                nsom = 2
                nosom(1)=zi(jconx1-1+zi(jconx2+ima-1)+ar(ia,1)-1)
                nosom(2)=zi(jconx1-1+zi(jconx2+ima-1)+ar(ia,2)-1)
                ff(1) = 1.d0 - alpha/longar
                ff(2) = alpha/longar
                mult=1.0d0
            else
                goto 120
            endif
!
!         RECUPERATION DU NUMERO DU NOEUD OU EST STOCKE LE LAGS_C
            if (zi(jxc) .eq. 1 .or. zi(jxc) .eq. 3) then
!           CAS LAGRANGES AUX NOEUDS
!           VALE_CONT SUR LE PREMIER NOEUD DE L'ARETE
                if (ia .ne. 0) then
                    nuno=zi(jconx1-1+zi(jconx2+ima-1)+ar(ia,1)-1)
                else if (in.ne.0) then
                    nuno=zi(jconx1-1+zi(jconx2+ima-1)+in-1)
                endif
            endif
!         VERIFICATION SI ON A DEJA ENREGISTRE CE POINT
            if (zi(jdejca+nuno-1) .eq. 1) goto 120
!         RACINE DE R AU POINT D'INTERSECTION
            levels = 0.d0
            do 125 k = 1, nsom
                call cesexi('C', jcesd5, jcesl5, ima, k,&
                            1, 1, iad)
!         C'EST FAUX MAIS C'ETAIT DEJA COMME CA
!         IL FAUT RECUPERER LES FF DE FORMES
                levels = levels + zr(jcesv5-1+iad)
125         continue
            rr =sqrt(abs(levels))
!
!         RECUP NORMALE, VECTEURS TANGENTS AU POINT D'INTERSECTION
            n(3)=0.d0
            tau1(3)=0.d0
!
            do 130 j = 1, ndim
                n(j) =zr(jcesv4-1+iadb-1+j)
                tau1(j)=zr(jcesv4-1+iadb-1+ndim+j)
                if (ndim .eq. 3) tau2(j)=zr(jcesv4-1+iadb-1+2*ndim+j)
130         continue
!
!         INDICATEUR DE CONTACT
!         IMPOSSIBLE A REMPLIR SI L'INTEGRATION N'EST PAS AUX NOEUDS
!
!         JEU
            call vecini(ndim, 0.d0, saut)
            do 140 j = 1, ndim
                do 141 k = 1, nsom
!             DDL HEAVISIDE
                    if (zl(jdepl-1+2*ndim*(nosom(k)-1)+j)) then
                        saut(j) = saut(j) - 2.d0 * ff(k) * zr(jdepv-1+ 2*ndim*(nosom(k)-1)+j)
                    endif
!             DDL ASYMPTOTIQUE
                    if (zl(jdepl-1+2*ndim*(nosom(k)-1)+ndim+j)) then
                        saut(j) = saut(j) - 2.d0 * ff(k) * rr * zr(jdepv-1+2*ndim*(nosom(k)-1)+nd&
                                  &im+j)
                    endif
141             continue
140         continue
            jeu = ddot(ndim,n,1,saut,1)
!
!         RNX, RNY, RNZ SONT LES 3 COMPOSANTES DE RNXYZ
            call vecini(3, 0.d0, rnxyz)
            do 144 j = 1, ndim
                do 145 k = 1, nsom
                    rnxyz(j) = rnxyz(j) + ff(k) * mult * zr(jcont-1+3* ndim*(nosom(k)-1)+j)
!             DDL HEAVISIDE
                    if (zl(jconl-1+3*ndim*(nosom(k)-1)+ndim+1)) then
                        rnxyz(j) = rnxyz(j) - ff(k) * mult * zr(jcont- 1+3*ndim*(nosom(k)-1)+ndim&
                                   &+j)
                    endif
!             DDL ASYMPTOTIQUE
                    if (zl(jconl-1+3*ndim*(nosom(k)-1)+2*ndim+1)) then
                        rnxyz(j) = rnxyz(j) - ff(k) * rr * mult * zr(jcont-1+3*ndim*(nosom(k)-1)+&
                                   &2*ndim+j)
                    endif
145             continue
144         continue
            call normev(rnxyz, rn)
            rnx = rnxyz(1)
            rny = rnxyz(2)
            rnz = rnxyz(3)
!
!         GLIX, GLIY, GLI
            call vecini(3, 0.d0, glit)
            call xmafr1(ndim, n, p)
            do 150 j = 1, ndim
                do 151 k = 1, ndim
                    glit(j)=glit(j)+p(j,k)*saut(k)
151             continue
150         continue
            call normev(glit, gli)
!
!         RTAX, RTAY, RTAZ, RTGX, RTGY, RTGZ
            call vecini(3, 0.d0, rtxyz)
            call vecini(3, 0.d0, lagfro)
!
            do 164 j = 1, ndim
                do 163 k = 1, nsom
                    lagfro(j) = lagfro(j) + ff(k) * zr(jlagv-1+ndim*( nosom(k)-1)+2) * tau1(j)
                    if (ndim .eq. 3) lagfro(j) = lagfro(j) + ff(k) * zr(jlagv-1+ndim*(nosom(k)-1)&
                                                 &+3) * tau2(j)
163             continue
164         continue
!
            do 161 j = 1, ndim
                do 162 k = 1, nsom
                    rtxyz(j) = rtxyz(j) + ff(k) * mult * zr(jfrot-1+3* ndim*(nosom(k)-1)+j)
!             DDL HEAVISIDE
                    if (zl(jfrol-1+3*ndim*(nosom(k)-1)+ndim+1)) then
                        rtxyz(j) = rtxyz(j) - ff(k) * mult * zr(jfrot- 1+3*ndim*(nosom(k)-1)+ndim&
                                   &+j)
                    endif
!             DDL ASYMPTOTIQUE
                    if (zl(jfrol-1+3*ndim*(nosom(k)-1)+2*ndim+1)) then
                        rtxyz(j) = rtxyz(j) - ff(k) * rr * mult * zr(jfrot-1+3*ndim*(nosom(k)-1)+&
                                   &2*ndim+j)
                    endif
162             continue
161         continue
!
!         NORME DU SEMI-MULTIPLICATEUR DE LAGRANGE DU FROTTEMENT
            call normev(lagfro, lagsf)
!
            if (lagsf .ge. 0.999d0) then
!           LE POINT EST GLISSANT
                rtgx = rtxyz(1)
                rtgy = rtxyz(2)
                rtgz = rtxyz(3)
                rtax = 0.d0
                rtay = 0.d0
                rtaz = 0.d0
            else
!           LE POINT EST ADHERENT
                rtax = rtxyz(1)
                rtay = rtxyz(2)
                rtaz = rtxyz(3)
                rtgx = 0.d0
                rtgy = 0.d0
                rtgz = 0.d0
            endif
!
!         LAGRANGES AUX NOEUDS, VALE_CONT SUR LE PREMIER NOEUD DE L'AR
            zr(jcnsvr-1+zresu*(nuno-1)+1)=cont
            zr(jcnsvr-1+zresu*(nuno-1)+2)=jeu
            zr(jcnsvr-1+zresu*(nuno-1)+3)=rn
            zr(jcnsvr-1+zresu*(nuno-1)+4)=rnx
            zr(jcnsvr-1+zresu*(nuno-1)+5)=rny
            zr(jcnsvr-1+zresu*(nuno-1)+6)=rnz
            zr(jcnsvr-1+zresu*(nuno-1)+9)=gli
            zr(jcnsvr-1+zresu*(nuno-1)+10)=rtax
            zr(jcnsvr-1+zresu*(nuno-1)+11)=rtay
            zr(jcnsvr-1+zresu*(nuno-1)+12)=rtaz
            zr(jcnsvr-1+zresu*(nuno-1)+13)=rtgx
            zr(jcnsvr-1+zresu*(nuno-1)+14)=rtgy
            zr(jcnsvr-1+zresu*(nuno-1)+15)=rtgz
            zr(jcnsvr-1+zresu*(nuno-1)+16)= rnx + rtax + rtgx
            zr(jcnsvr-1+zresu*(nuno-1)+17)= rny + rtay + rtgy
            zr(jcnsvr-1+zresu*(nuno-1)+18)= rnz + rtaz + rtgz
            zr(jcnsvr-1+zresu*(nuno-1)+19)= sqrt((rnx+rtax+rtgx)**2 +&
            (rny+rtay+rtgy)**2 + (rnz+rtaz+rtgz)**2)
            zr(jcnsvr-1+zresu*(nuno-1)+25)=xyz(1)
            zr(jcnsvr-1+zresu*(nuno-1)+26)=xyz(2)
            zr(jcnsvr-1+zresu*(nuno-1)+27)=xyz(3)
!
            zl(jcnslr-1+zresu*(nuno-1)+1)=.true.
            zl(jcnslr-1+zresu*(nuno-1)+2)=.true.
            zl(jcnslr-1+zresu*(nuno-1)+3)=.true.
            zl(jcnslr-1+zresu*(nuno-1)+4)=.true.
            zl(jcnslr-1+zresu*(nuno-1)+5)=.true.
            if (ndim .eq. 3) zl(jcnslr-1+zresu*(nuno-1)+6)=.true.
            zl(jcnslr-1+zresu*(nuno-1)+9)=.true.
            zl(jcnslr-1+zresu*(nuno-1)+10)=.true.
            zl(jcnslr-1+zresu*(nuno-1)+11)=.true.
            if (ndim .eq. 3) zl(jcnslr-1+zresu*(nuno-1)+12)=.true.
            zl(jcnslr-1+zresu*(nuno-1)+13)=.true.
            zl(jcnslr-1+zresu*(nuno-1)+14)=.true.
            if (ndim .eq. 3) zl(jcnslr-1+zresu*(nuno-1)+15)=.true.
            zl(jcnslr-1+zresu*(nuno-1)+16)=.true.
            zl(jcnslr-1+zresu*(nuno-1)+17)=.true.
            if (ndim .eq. 3) zl(jcnslr-1+zresu*(nuno-1)+18)=.true.
            zl(jcnslr-1+zresu*(nuno-1)+19)=.true.
            zl(jcnslr-1+zresu*(nuno-1)+25)=.true.
            zl(jcnslr-1+zresu*(nuno-1)+26)=.true.
            zl(jcnslr-1+zresu*(nuno-1)+27)=.true.
            zi(jdejca+nuno-1) = 1
!
120     continue
!
100 end do
!
    call jedetr(fcont)
    call detrsd('CHAMP', fconts)
    call detrsd('CHAMP', fctcn)
    call jedetr(ffrot)
    call jedetr(dejcal)
    call detrsd('CHAMP', ffrots)
    call detrsd('CHAMP', ffrocn)
    call detrsd('CHAMP', depdes)
    call detrsd('CHAMP', depcn)
    call detrsd('CHAMP', lagcn)
!
    call jedema()
end subroutine
