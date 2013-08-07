subroutine xptfon(noma, ndim, nmafon, cnslt, cnsln,&
                  cnxinv, jmafon, nxptff, jfon, nfon,&
                  jbas, jtail, fiss, goinop, listpt,&
                  orient)
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/cengra.h"
#include "asterfort/cnocns.h"
#include "asterfort/confac.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/intfac.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/normev.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xextre.h"
#include "asterfort/xfabor.h"
#include "asterfort/xnorme.h"
#include "asterfort/xtailm.h"
    integer :: nmafon, jmafon, jfon, nfon, jbas, jtail, nxptff
    character(len=8) :: noma, fiss
    character(len=19) :: cnslt, cnsln, cnxinv, listpt
    logical :: orient, goinop
!     ------------------------------------------------------------------
! person_in_charge: samuel.geniaut at edf.fr
!
!       RECHERCHE DES POINTS DU FOND DE FISSURE DANS LE CADRE DE XFEM
!
!  ENTREES :
!     NOMA         :    NOM DE L'OBJET MAILLAGE
!     NMAFON       :    NOMBRE DE MAILLES DE LA ZONE FOND DE FISSURE
!     JMAFON       :    MAILLES DE LA ZONE FOND DE FISSURE
!     NXPTFF       :    NOMBRE MAXIMUM DE POINTS DU FOND DE FISSURE
!     CNSLT,CNSLN  :    LEVEL-SETS
!     CNXINV       :    CONNECTIVITE INVERSE
!     FISS         :    SD FISS_XFEM (POUR RECUP DES GRADIENTS)
!     GOINOP       :    .TRUE. SI OPOO10 AVEC UPWIND-SIMPLEXE/GRILLE/3D
!                       .FALSE. SINON
!
!  SORTIES :
!     JFON         :   ADRESSE DES POINTS DU FOND DE FISSURE
!     JBAS         :   ADRESSE DES DIRECTIONS DE PROPAGATION
!     NFON         :   NOMBRE DE POINTS DU FOND DE FISSURE
!     LISTPT       :   LISTE DES INDICES DES POINTS DU FOND DESORDONNES
!     ORIENT       :   .TRUE.  SI LE FOND PEUT ETRE ORIENTE
!                      .FALSE. SI LE FOND NE PEUT PAS ETRE ORIENTE
!
!     ------------------------------------------------------------------
!
    integer :: ipt, ima, i, j, ibid, ndim, ino, k, ifq, iret, ifm, niv
    integer :: nmaabs, nbf, nbnoma, nuno, nunoa, nunob, nunoc, nunod
    integer :: fa(6, 4), ibid3(12, 3), vecind(5)
    integer :: jconx1, jconx2, jcoor, jltsv, jlnsv, jma
    integer :: jlsn, jlst, jglsn, jglst, igeom, jgt, jgn, itypma
    integer :: indipt, jbord, jborl, jdirol, jnvdir, jlistp
    integer :: nbfacb, iptbor(2), nbptma, ndime, indptf(3), codret
    integer :: nunopa, nunopb, nunopc, nunopd
    integer :: snuno, pnuno, inuno, snunop, pnunop, inunop
    real(kind=8) :: m(3), p(3), gln(3), glt(3), coorg(3), vectn(12)
    real(kind=8) ::  normi
    character(len=8) :: typma, k8b, nommai, alias
    character(len=19) :: grlt, chgrt, grln, chgrn
    logical :: fabord, indic
! ----------------------------------------------------------------------
    call jemarq()
    call infdbg('XFEM', ifm, niv)
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    call jeveuo(noma//'.TYPMAIL', 'L', jma)
!
    call jeveuo(cnslt//'.CNSV', 'L', jltsv)
    call jeveuo(cnsln//'.CNSV', 'L', jlnsv)
!
!     GRADIENT LST
    if (goinop) then
        grlt = fiss//'.GRI.GRLTNO'
    else
        grlt = fiss//'.GRLTNO'
    endif
    chgrt = '&&XPTFON.GRLN'
    call cnocns(grlt, 'V', chgrt)
    call jeveuo(chgrt//'.CNSV', 'L', jgt)
!
!     GRADIENT LSN
    if (goinop) then
        grln = fiss//'.GRI.GRLNNO'
    else
        grln = fiss//'.GRLNNO'
    endif
    chgrn = '&&XPTFON.GRLT'
    call cnocns(grln, 'V', chgrn)
    call jeveuo(chgrn//'.CNSV', 'L', jgn)
!
    call wkvect('&&XPTFON.PTBORD', 'V V L', nxptff, jbord)
!
!     VECTEUR PERMETTANT DE SAVOIR SI LE VECTEUR DE DIRECTION DE
!     PROPAGATION (VDIR) A ETE RECALCULE OU NON AUX POINTS
!     EXTREMITES DE FONFIS
    call wkvect('&&XPTFON.LBORD', 'V V L', nxptff, jborl)
!
!     VECTEUR CONTENANT LES VDIR INITIAUX (CAD SANS MODIFICATION
!     DES VECTEURS AUX POINTS EXTREMITES DE FONFIS)
    call wkvect('&&XPTFON.VDIROL', 'V V R', 3*nxptff, jdirol)
!
!     VECTEUR CONTENANT 0 OU 1 AUX POINTS EXTREMITES DE FONFIS:
!     0: LE PRODUIT SCALAIRE ENTRE LA NORMALE A LA FACE DE BORD ET
!        LE VDIR INITIAL ESI INFERIEUR A 0
!     1: LE PRODUIT SCALAIRE EST SUPERIEUR OU EGAL A 0
    call wkvect('&&XPTFON.NVDIR', 'V V I', nxptff, jnvdir)
!
    do 10 i = 1, nxptff
        zl(jbord-1+i) = .false.
        zl(jborl-1+i) = .false.
10  end do
!
!     COMPTEUR : NOMBRE DE POINTS DE FONFIS TROUVES
    ipt = 0
!
    orient = .true.
!     LISTE DES INDICES DES POINTS DU FOND DESORDONNES
    call wkvect(listpt, 'V V I', 2*nmafon, jlistp)
!
!     BOUCLE SUR LES MAILLES DE MAFOND
    do 100 ima = 1, nmafon
!
        nmaabs = zi(jmafon-1+(ima-1)+1)
        itypma = zi(jma-1+nmaabs)
        call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
        call dismoi('F', 'DIM_TOPO', typma, 'TYPE_MAILLE', ndime,&
                    k8b, iret)
!
!       ON SE RECREE UN ENVIRONNEMENT COMME DANS UN TE
!       POUR LSN, LST, GRLST, GRLST ET IGEOM
!       AFIN DE POUVOIR APPELER INTFAC
        nbnoma = zi(jconx2+nmaabs) - zi(jconx2+nmaabs-1)
        call wkvect('&&XPTFON.LSN', 'V V R', nbnoma, jlsn)
        call wkvect('&&XPTFON.LST', 'V V R', nbnoma, jlst)
        call wkvect('&&XPTFON.GRLSN', 'V V R', nbnoma*ndim, jglsn)
        call wkvect('&&XPTFON.GRLST', 'V V R', nbnoma*ndim, jglst)
        call wkvect('&&XPTFON.IGEOM', 'V V R', nbnoma*ndim, igeom)
        do 110 ino = 1, nbnoma
            nuno=zi(jconx1-1+zi(jconx2+nmaabs-1)+ino-1)
            zr(jlsn-1+ino) = zr(jlnsv-1+nuno)
            zr(jlst-1+ino) = zr(jltsv-1+nuno)
            do 120 j = 1, ndim
                zr(jglsn-1+ndim*(ino-1)+j) = zr(jgn-1+ndim*(nuno-1)+j)
                zr(jglst-1+ndim*(ino-1)+j) = zr(jgt-1+ndim*(nuno-1)+j)
                zr(igeom-1+ndim*(ino-1)+j) = zr(jcoor-1+3*(nuno-1)+j)
120          continue
110      continue
!
!       CALCUL DU CENTRE DE GRAVITE DE LA MAILLE
        call cengra(noma, nmaabs, coorg)
!
        call confac(typma, ibid3, ibid, fa, nbf)
!
!       INITIALISATIONS
        nbfacb = 0
        iptbor(1)= 0
        iptbor(2)= 0
        nbptma = 0
        do 130 i = 1, 3
            indptf(i)=0
130      continue
!
!       BOUCLE SUR LES FACES
        do 200 ifq = 1, nbf
!         TYPE DE FACE
            if (fa(ifq,4) .eq. 0) then
                alias = 'TR3'
            else
                alias='QU4'
            endif
!
            indipt=0
!         RECHERCHE DES INTERSECTION ENTRE LE FOND DE FISSURE ET LA FACE
            call intfac(noma, nmaabs, ifq, fa, nbnoma,&
                        zr(jlst), zr(jlsn), ndim, 'OUI', jglsn,&
                        jglst, igeom, m, indptf, gln,&
                        glt, codret)
!
            if (codret .eq. 0) goto 200
!
!         VERIFICATION SI CE POINT A DEJA ETE TROUVE
            nunoa = zi(jconx1-1+zi(jconx2+nmaabs-1)+fa(ifq,1)-1)
            nunob = zi(jconx1-1+zi(jconx2+nmaabs-1)+fa(ifq,2)-1)
            nunoc = zi(jconx1-1+zi(jconx2+nmaabs-1)+fa(ifq,3)-1)
            if (alias .eq. 'QU4') then
                nunod = zi(jconx1-1+zi(jconx2+nmaabs-1)+fa(ifq,4)-1)
            endif
!
            do 220 j = 1, ipt
                indic=.false.
!
!           VERIFICATION POUR UN POINT INTERIEUR (cf Doc R7.02.12)
                if ((indptf(1).eq.3) .and. (int(zr(jfon-1+11*(j-1)+9)) .eq.3)) then
                    nunopa = int(zr(jfon-1+11*(j-1)+5))
                    nunopb = int(zr(jfon-1+11*(j-1)+6))
                    nunopc = int(zr(jfon-1+11*(j-1)+7))
                    snuno=nunoa+nunob+nunoc
                    pnuno=nunoa*nunob*nunoc
                    inuno=nunoa*nunob+nunoa*nunoc+nunob*nunoc
                    snunop=nunopa+nunopb+nunopc
                    pnunop=nunopa*nunopb*nunopc
                    inunop=nunopa*nunopb+nunopa*nunopc+nunopb*nunopc
                    if (alias .eq. 'QU4') then
                        nunopd = int(zr(jfon-1+11*(j-1)+8))
                        snuno=nunoa+nunob+nunoc+nunod
                        pnuno=nunoa*nunob*nunoc*nunod
                        inuno=nunoa*nunob+nunoa*nunoc+nunob*nunoc+&
                        nunoa*nunod+nunob*nunod+nunoc*nunod
                        snunop=nunopa+nunopb+nunopc+nunopd
                        pnunop=nunopa*nunopb*nunopc*nunopd
                        inunop=nunopa*nunopb+nunopa*nunopc+nunopb*&
                        nunopc+ nunopa*nunopd+nunopb*nunopd+nunopc*&
                        nunopd
                    endif
                    if ((snuno.eq.snunop) .and. (pnuno.eq.pnunop) .and. ( inuno.eq.inunop)) then
                        indic=.true.
                    endif
!           VERIFICATION POUR UN POINT ARETE (cf Doc R7.02.12)
                    else if ((indptf(1).eq.2).and. (int(zr(jfon-1+11*(j-1)&
                +9)).eq.2)) then
                    snuno=indptf(2)+indptf(3)
                    pnuno=(indptf(2))*(indptf(3))
                    nunopa = int(zr(jfon-1+11*(j-1)+10))
                    nunopb = int(zr(jfon-1+11*(j-1)+11))
                    snunop=nunopa+nunopb
                    pnunop=nunopa*nunopb
                    if ((snuno.eq.snunop) .and. (pnuno.eq.pnunop)) then
                        indic=.true.
                    endif
!           VERIFICATION POUR UN POINT SOMMET (cf Doc R7.02.12)
                    else if ((indptf(1).eq.1).and. (int(zr(jfon-1+11*(j-1)&
                +9)).eq.1)) then
                    nunopa = int(zr(jfon-1+11*(j-1)+10))
                    if (indptf(2) .eq. nunopa) then
                        indic=.true.
                    endif
                endif
!
                if (indic) then
!               CALCUL DE LA TAILLE MAX DE LA MAILLE IMA ET MISE A
!               JOUR DU VECTEUR DES TAILLES DE MAILLES
                    call xtailm(ndim, glt, nmaabs, typma, jcoor,&
                                jconx1, jconx2, j, jtail)
                    if (ndim .ne. 3) goto 200
                    indipt = j
                    goto 300
                endif
220          continue
!
!         CE POINT N'A PAS DEJA ETE TROUVE, ON LE GARDE
            ipt = ipt+1
!         AUGMENTER NXPTFF
            ASSERT(ipt.le.nxptff)
!
!         STOCKAGE DES COORDONNEES DU POINT M,
!         DE LA BASE LOCALE (GRADIENT DE LSN ET LST) ET
!         DES NUMEROS DES SOMMETS DE LA FACE CONTENANT M
            do 230 k = 1, ndim
                zr(jfon-1+11*(ipt-1)+k) = m(k)
                zr(jbas-1+2*ndim*(ipt-1)+k) = gln(k)
                zr(jbas-1+2*ndim*(ipt-1)+k+ndim)= glt(k)
230          continue
            do 231 k = 1, 3
                zr(jfon-1+11*(ipt-1)+4+k) = zi( jconx1-1+zi(jconx2+ nmaabs-1)+fa(ifq,k)-1 )
                zr(jfon-1+11*(ipt-1)+8+k) = indptf(k)
231          continue
            if (alias .eq. 'QU4') then
                zr(jfon-1+11*(ipt-1)+8) = zi( jconx1-1+zi(jconx2+ nmaabs-1)+fa(ifq,4)-1 )
            else
                zr(jfon-1+11*(ipt-1)+8) = 0
            endif
!
            indipt = ipt
!
!         CALCUL DE LA TAILLE MAX DE LA MAILLE IMA
            call xtailm(ndim, glt, nmaabs, typma, jcoor,&
                        jconx1, jconx2, ipt, jtail)
!
            if (ndim .ne. 3) goto 200
!
300          continue
!
!         ON VERIFIE SI LA FACE COURANTE EST UNE FACE DE BORD
!         CELA N'A DE SENS QU'EN 3D
            call xfabor(noma, cnxinv, nunoa, nunob, nunoc,&
                        fabord)
!
!         SI LA FACE EST UNE FACE DE BORD ON PREND SA NORMALE
            if (fabord) then
                if (.not. zl(jbord-1+indipt)) then
                    zl(jbord-1+indipt) = .true.
                endif
                if (ndim .eq. 3) then
                    call xnorme(indipt, iptbor, vectn, nbfacb, nunoa,&
                                nunob, nunoc, jcoor, coorg)
                endif
            endif
!
!         STOCKAGE DES INDICES DES POINTS DU FOND APPARTENANT A
!         LA MAILLE
            if (ndime .eq. 3) then
                if (nbptma .eq. 0) then
                    zi(jlistp-1+2*(ima-1)+1) = indipt
                    nbptma = nbptma+1
                    vecind(nbptma) = indipt
!
                    elseif ((nbptma.eq.1) .and. (indipt.ne.zi(jlistp-1+2*(&
                ima-1)+1))) then
                    zi(jlistp-1+2*(ima-1)+2) = indipt
                    nbptma = nbptma+1
                    vecind(nbptma) = indipt
!
                else if (nbptma.gt.1) then
                    do 240 j = 1, nbptma
                        if (indipt .eq. vecind(j)) goto 400
240                  continue
!             ORIENTATION DU FOND PAS POSSIBLE A REALISER
                    orient = .false.
!
                    nbptma = nbptma+1
                    vecind(nbptma) = indipt
!
!             IMPRESSION DES POINTS DU FOND APPARTENANT A LA MAILLE
                    if (ifq .eq. nbf) then
                        call jenuno(jexnum(noma//'.NOMMAI', nmaabs), nommai)
                        call u2mesg('I', 'XFEM_51', 1, nommai, 1,&
                                    nbptma, 0, 0.d0)
!
                        write(ifm,797)
                        797           format(7x,'X',13x,'Y',13x,'Z')
                        do 250 j = 1, nbptma
                            p(1) = zr(jfon-1+11*(vecind(j)-1)+1)
                            p(2) = zr(jfon-1+11*(vecind(j)-1)+2)
                            p(3) = zr(jfon-1+11*(vecind(j)-1)+3)
                            write(ifm,798)(p(k),k=1,3)
                            798             format(2x,3(e12.5,2x))
250                      continue
                    endif
                endif
            endif
400          continue
!       FIN BOUCLE SUR LES FACES DE LA MAILLE
200      continue
!
!       CALCUL DES VECTEURS DE PROPAGATION AUX EXTREMITES
        if ((ndim.eq.3) .and. (nbfacb.ne.0)) then
            call xextre(iptbor, vectn, nbfacb, jbas, jborl,&
                        jdirol, jnvdir)
        endif
!
!       DESTRUCTION DES VECTEURS LOCAUX A LA MAILLE
        call jedetr('&&XPTFON.LSN')
        call jedetr('&&XPTFON.LST')
        call jedetr('&&XPTFON.GRLSN')
        call jedetr('&&XPTFON.GRLST')
        call jedetr('&&XPTFON.IGEOM')
!
!     FIN BOUCLE SUR LES MAILLES
100  end do
!
!     NORMALISATION DES NOUVEAUX VECTEURS DE DIRECTION DE
!     PROPAGATION
    if (ndim .eq. 3) then
        do 500 k = 1, nxptff
            if (zl(jbord-1+k)) then
                call normev(zr(jbas-1+6*(k-1)+4), normi)
            endif
500      continue
!
    endif
!
    nfon = ipt
!
!     IMPRESSION DE TOUS LES POINTS DE FOND DE FISSURE
!     DANS LE CAS OU IL N'EST PAS POSSIBLE DE LES ORDONNER
    if (.not.orient) then
        call u2mess('A', 'XFEM_52')
        call u2mess('I', 'XFEM_35')
        write(ifm,897)
        897   format(7x,'X',13x,'Y',13x,'Z')
        do 600 i = 1, nfon
            p(1) = zr(jfon-1+11*(i-1)+1)
            p(2) = zr(jfon-1+11*(i-1)+2)
            p(3) = zr(jfon-1+11*(i-1)+3)
            write(ifm,898)(p(k),k=1,3)
            898     format(2x,3(e12.5,2x))
600      continue
    endif
!
!
    call jedetr(chgrn)
    call jedetr(chgrt)
    call jedetr('&&XPTFON.PTBORD')
    call jedetr('&&XPTFON.LBORD')
    call jedetr('&&XPTFON.VDIROL')
    call jedetr('&&XPTFON.NVDIR')
    call jedema()
end subroutine
