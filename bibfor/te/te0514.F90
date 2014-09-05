subroutine te0514(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/iselli.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/loncar.h"
#include "asterfort/matini.h"
#include "asterfort/ndcent.h"
#include "asterfort/padist.h"
#include "asterfort/teattr.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/xdecou.h"
#include "asterfort/xdecov.h"
#include "asterfort/xdecqu.h"
#include "asterfort/xdecqv.h"
#include "asterfort/xdivte.h"
#include "asterfort/xxmmvd.h"
#include "asterfort/lteatt.h"
!
    character(len=16) :: option, nomte
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
!.......................................................................
!
!       CALCUL DES DONNEES TOPOLOGIQUES CONCERNANT CONCERNANT
!       LA DECOUPE DES ELEMENTS POUR L'INTEGRATION AVEC X-FEM
!                   (VOIR BOOK IV 27/10/04)
!
!  OPTION : 'TOPOSE' (X-FEM TOPOLOGIE DES SOUS-ELEMENTS)
!
!  ENTREES  ---> OPTION : OPTION DE CALCUL
!           ---> NOMTE  : NOM DU TYPE ELEMENT
!
!......................................................................
!
!
    character(len=8) :: elp, noma, typma, enr
    integer :: igeom, jlsn, ifisc, nfisc, ptmaxi, pmmaxi
    integer :: jout1, jout2, jout3, jout4, jout5, zintmx, nfimax
    integer :: iadzi, iazk24, nno, nnn, jfisco, nsemax
    integer :: ninter, nit, nse, nnose, ise2, ncomph, ncompp, ncompc
    integer :: npts, cnse(6, 10), i, j, k, it, npi, ipt, ise, in, ni, cpt
    integer :: ndim, ibid, ndime, iad, jtab(7), jtab2(2), vali(2)
    integer :: nnc, npm, nmilie, pmmax, nmfis
    integer :: iret, nfiss, ifiss, ncomb, ninmax, nmmax
    parameter(ptmaxi=6,zintmx=5,pmmaxi=17,nsemax=6,nfimax=10)
    parameter(ninmax=44,nmmax=65)
    real(kind=8) :: nmil(3, 7), txlsn(7), ainter(ptmaxi*zintmx)
    real(kind=8) :: newpt(3), p(3), lonref, pinter(3*ptmaxi)
    real(kind=8) :: pmilie(3*pmmaxi), heav(nsemax*nfimax)
    real(kind=8) :: xg(3), cridist
    parameter(cridist=1.d-7)
    integer :: fisco(2*nfimax), fisc(2*nfimax), zxain, ai
    integer :: ndoubl(ninmax*(2**nfimax)), ndoub2(ninmax*(2**nfimax))
    integer :: ndoub3(nmmax*(2**nfimax))
    aster_logical :: deja, ajn
!
!......................................................................
!     LES TABLEAUX FISC, FISCO, NDOUBL, NDOUB2, PMILIE, PINTER ONT ETE
!     ALLOUE DE FACON STATIQUE POUR OPTIMISER LE CPU (CAR LES APPELS A
!     WKVECT DANS LES TE SONT COUTEUX).
!
    ASSERT(option.eq.'TOPOSE')
    call vecini(51, 0.d0, pmilie)
    call vecini(20, 0.d0, ainter)
    call jemarq()
!
    call elref1(elp)
    call elrefe_info(fami='RIGI', ndim=ndime, nno=nno)
!
    call tecael(iadzi, iazk24)
    noma=zk24(iazk24)(1:8)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)(1:8)
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
!
!     ATTENTION, NE PAS CONFONDRE NDIM ET NDIME  !!
!     NDIM EST LA DIMENSION DU MAILLAGE
!     NDIME EST DIMENSION DE L'ELEMENT FINI
!     PAR EXEMPLE, POUR LES ELEMENT DE BORDS D'UN MAILLAGE 3D :
!     NDIME = 2 ALORS QUE NDIM = 3
!
!
! calcul du nombre maximum de points milieux crees par le sous-decoupage
    if (ndime .eq. 3) then
        pmmax=63
    else if (ndime .eq. 2) then
        pmmax=10
    else if (ndime.eq. 1) then
        pmmax=2
    endif
!
!     RECUPERATION DES ENTREES / SORTIE
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PLEVSET', 'L', jlsn)
    call jevech('PPINTTO', 'E', jout1)
    call jevech('PCNSETO', 'E', jout2)
    call jevech('PHEAVTO', 'E', jout3)
    call jevech('PLONCHA', 'E', jout4)
!
    call teattr('S', 'XFEM', enr, ibid)
!
!     MODELISATION AXISYMETRIQUE AVEC ELEMENTS QUADRATIQUES INTERDITE
    if (lteatt('AXIS','OUI') .and. .not.iselli(elp)) then
        call utmess('F', 'XFEM_76')
    endif
!
    if ((ibid.eq.0) .and. (enr.eq.'XH' .or.enr.eq.'XHT'.or.enr.eq.'XT' .or.enr.eq.'XHC')&
        .and. .not.iselli(elp)) then
        call jevech('PPMILTO', 'E', jout5)
    endif
!
    call tecach('OOO', 'PHEAVTO', 'E', iret, nval=7,&
                itab=jtab)
    ncomph = jtab(2)
    nfiss = jtab(7)
!
    do 9 i = 1, 2*nfimax
        fisco(i) = 0
        fisc(i) = 0
  9 end do
    if (nfiss .gt. 1) then
        call jevech('PFISCO', 'L', jfisco)
        do 10 i = 1, 2*nfiss
            fisco(i) = zi(jfisco+i-1)
 10     continue
    endif
!
    call tecach('OOO', 'PPINTTO', 'E', iret, nval=2,&
                itab=jtab2)
    ncompp = jtab2(2)/ndim
    call tecach('OOO', 'PCNSETO', 'E', iret, nval=2,&
                itab=jtab2)
    ncompc = jtab2(2)
!
! calcul du nombre de noeuds centraux
!
! initilisation : cas sans noeuds centraux
    nnc=0
    call matini(3, 7, 0.d0, nmil)
    call vecini(7, 0.d0, txlsn)
!
! si l'element est incomplet, on calcule les informations concernant les les noeuds centraux
    if ((elp.eq.'QU8') .or. (elp.eq.'H20') .or. (elp.eq.'P13') .or. (elp.eq.'P15')) then
        call ndcent(igeom, ndim, zr(jlsn), nmil, txlsn,&
                    nnc)
        end if
!
        npi=0
        zxain = xxmmvd('ZXAIN')
        npm=0
        ajn=.false.
        nmfis=0
!
!     CALCUL D'UNE LONGUEUR CARACTERISTIQUE DE L'ELEMENT
        call loncar(ndim, typma, zr(igeom), lonref)
!
!     ON SUBDIVISE L'ELEMENT PARENT EN NIT SOUS-ELEMENTS
        call xdivte(elp, zi(jout2), nit, nnose)
        cpt = nit
!     PROBLEME DE DIMENSSIONNEMENT DANS LES CATALOGUES
        ASSERT(ncompc.ge.nnose*ncomph)
!
        vali(1)=nfimax
        vali(2)=nfiss
        if (nfiss .gt. nfimax) then
            call utmess('F', 'XFEM2_6', ni=2, vali=vali)
        endif
!
!     BOUCLE SUR LES FISSURES
!
        do 90 ifiss = 1, nfiss
!
            do 91 i = 1, ifiss
                fisc(i)=0
 91         continue
            ifisc = ifiss
            nfisc = 0
 80         continue
            if (fisco(2*ifisc-1) .gt. 0) then
                nfisc = nfisc+1
                fisc(2*(nfisc-1)+2) = fisco(2*ifisc)
                ifisc = fisco(2*ifisc-1)
                fisc(2*(nfisc-1)+1) = ifisc
                goto 80
            endif
!
!
!       BOUCLE SUR LES NIT TETRAS
            do 100 it = 1, nit
!
!         DECOUPAGE EN NSE SOUS-ELEMENTS
                call vecini(18, 0.d0, pinter)
                nmilie = 0
                ninter = 0
                npts = 0
                do 89 i = 1, nsemax*nfimax
                    heav(i)=0.d0
 89             continue
!
                if (.not.iselli(elp)) then
                    call xdecqu(nnose, it, ndim, zi(jout2), jlsn,&
                                igeom, pinter, ninter, npts, ainter,&
                                pmilie, nmilie, nmfis, nmil, txlsn)
!
                    call xdecqv(nnose, it, zi(jout2), zr(jlsn), igeom,&
                                ninter, npts, ainter, nse, cnse,&
                                heav, nsemax, pinter, zr(jout1))
                else
                    call xdecou(ndim, elp, nno, nnose, it,&
                                zr(jout1), zi(jout2), zr(jlsn), fisc, igeom,&
                                nfiss, ifiss, pinter, ninter, npts,&
                                ainter, lonref, nfisc)
                    call xdecov(ndim, elp, nno, nnose, it,&
                                zr(jout1), zi(jout2), zi(jout3), ncomph, zr(jlsn),&
                                fisc, igeom, nfiss, ifiss, pinter,&
                                ninter, npts, ainter, nse, cnse,&
                                heav, nfisc, nsemax)
                endif
!
! ----- - BOUCLE SUR LES NINTER POINTS D'INTER : ARCHIVAGE DE PINTTO
                do 200 ipt = 1, ninter
                    do 210 j = 1, ndim
                        newpt(j)=pinter(ndim*(ipt-1)+j)
210                 continue
!
!           VERIF SI EXISTE DEJA DANS PINTTO
                    deja=.false.
                    do 220 i = 1, npi
                        do 221 j = 1, ndim
                            p(j) = zr(jout1-1+ndim*(i-1)+j)
221                     continue
                        if (padist(ndim,p,newpt) .lt. (lonref*cridist)) then
                            deja = .true.
                            ni=i
                        endif
220                 continue
                    ai=nint(ainter(zxain*(ipt-1)+1))
                    if (ai .eq. 0) go to 200
                    if (.not.deja) then
                        npi=npi+1
                        ni =npi
!             NOMBRE TOTAL DE PT D'INTER LIMITE A LA TAILLE DE LA CARTE
                        ASSERT(npi.le.ncompp)
!             ARCHIVAGE DE PINTTO
                        do 230 j = 1, ndim
                            zr(jout1-1+ndim*(npi-1)+j)=newpt(j)
230                     continue
!
!             MISE A JOUR DU CNSE (TRANSFORMATION DES 100 EN 1000...)
                        do 240 ise = 1, nse
                            do 241 in = 1, ndime+1
                                if (cnse(ise,in) .eq. 100+ipt) cnse(ise,in)= 1000+ni
241                         continue
240                     continue
                    else
                        do 114 ise = 1, nse
                            do 115 in = 1, ndime+1
                                if (cnse(ise,in) .eq. 100+ipt) cnse(ise,in)= 1000+ni
115                         continue
114                     continue
                    endif
200             continue
!
!
! ------- BOUCLE SUR LES NMILIE POINTS MILIEUX : ARCHIVAGE DE PMILTO
                if (.not.iselli(elp)) then
                    do 300 ipt = 1, nmilie
                        do 310 j = 1, ndim
                            newpt(j)=pmilie(ndim*(ipt-1)+j)
310                     continue
!             VERIF SI EXISTE DEJA DANS PMILTO
                        deja=.false.
                        do 320 i = 1, npm
                            do 321 j = 1, ndim
                                p(j) = zr(jout5-1+ndim*(i-1)+j)
321                         continue
                            if (padist(ndim,p,newpt) .lt. (lonref*cridist)) then
                                deja = .true.
                                ni=i
                            endif
320                     continue
                        if (.not.deja) then
                            npm=npm+1
!               NOMBRE TOTAL DE POINTS MILIEUX LIMITE A PMMAX
                            ASSERT(npm.le.pmmax)
!               ARCHIVAGE DE PMILTO
                            do 330 j = 1, ndim
                                zr(jout5-1+ndim*(npm-1)+j)=pmilie(ndim*(ipt-1)+j)
330                         continue
!
!               MISE A JOUR DU CNSE (TRANSFORMATION DES 200 EN 2000...)
                            do 340 ise = 1, nse
                                do 341 in = 1, nnose
                                    if (cnse(ise,in) .eq. 200+ipt) cnse(ise, in)=2000+npm
341                             continue
340                         continue
                        else
                            do 350 ise = 1, nse
                                do 351 in = 1, nnose
                                    if (cnse(ise,in) .eq. 200+ipt) cnse(ise, in)=2000+ni
351                             continue
350                         continue
                        endif
!
300                 continue
                endif
!
! ------- BOUCLE SUR LES NSE SOUS-ELE : ARCHIVAGE DE PCNSETO, PHEAVTO
                do 120 ise = 1, nse
                    if (ise .eq. 1) then
                        ise2=it
                    else
                        cpt=cpt+1
                        ise2=cpt
                    endif
!         NOMBRE TOTAL DE SOUS-ELEMENTS LIMITE A LA TAILLE DE LA CARTE
                    ASSERT(cpt.le.ncomph)
!           ARCHIVAGE DE PHEAVTO
                    do 125 i = 1, ifiss
                        zi(jout3-1+ncomph*(i-1)+ise2)= nint(heav(ifiss*(&
                    ise-1)+i))
125                 continue
!           ARCHIVAGE DE PCNSETO
                    do 121 in = 1, nnose
                        zi(jout2-1+nnose*(ise2-1)+in)=cnse(ise,in)
121                 continue
!
120             continue
!
100         continue
            nit = cpt
!
 90     end do
!
!     ARCHIVAGE DE LONCHAM SOUS ELEMENTS
        zi(jout4-1+1)=nit
!
!     ARCHIVAGE DE LONCHAM POINTS D'INTERSECTION
        zi(jout4-1+2)=npi
!
!     SERT UNIQUEMENT POUR LA VISU
!
!     POUR COMPTER LES NOEUDS EN DOUBLE (EN POST-TRAITEMENT)
!     NCOMB COMBINAISONS POSSIBLES
        ncomb = 2**nfiss
!     ON VA JUSQU'A NCOMPP, NOMBRE MAX DE POINTS D'INTERSECTION
        ASSERT(ncompp.le.ninmax)
        do 776 i = 1, ninmax*(2**nfimax)
            ndoub2(i)=0
776     end do
!     POUR COMPTER LES PT D'INTER EN DOUBLE (EN POST-TRAITEMENT)
!     ON VA JUSQU'A NNO+1 POUR PRENDRE EN COMPTE LE 9EME NOEUD DU QUAD8
        ASSERT((nno+1).le.ninmax)
        do 777 i = 1, ninmax*(2**nfimax)
            ndoubl(i)=0
777     end do
!     POUR COMPTER LES PT MILIEU EN DOUBLE (EN POST-TRAITEMENT)
!     ON VA JUSQU'A NNO+NNC POUR PRENDRE EN COMPTE LES NOEUDS CENTRAUX
        do 778 i = 1, nmmax*(2**nfimax)
            ndoub3(i)=0
778     end do
!
        do 130 ise = 1, nit
            do 127 in = 1, nnose
                i = zi(jout2-1+nnose*(ise-1)+in)
                if (i .lt. 1000) then
! ----- ON SE PLACE EN BASE 2, LA DIMENSSION DU PB EST NFISS
! ----- POUR CHAQUE FISSURE H=-1 OU 0=>0
!                           H=+1     =>1
                    iad = ncomb*(i-1)
                    do 128 ifiss = 1, nfiss
                        if (zi(jout3-1+ncomph*(ifiss-1)+ise) .eq. 1) then
                            iad = iad + 2**(nfiss-ifiss)
                        endif
128                 continue
                    ndoubl(iad+1) = 1
                else if (i.gt.1000.and.i.lt.2000) then
                    iad = ncomb*(i-1001)
                    do 129 ifiss = 1, nfiss
                        if (zi(jout3-1+ncomph*(ifiss-1)+ise) .eq. 1) then
                            iad = iad + 2**(nfiss-ifiss)
                        endif
129                 continue
                    ndoub2(iad+1) = 1
                else if (i.gt.2000.and.i.lt.3000) then
                    iad = ncomb*(i-2001)
                    do 779 ifiss = 1, nfiss
                        if (zi(jout3-1+ncomph*(ifiss-1)+ise) .eq. 1) then
                            iad = iad + 2**(nfiss-ifiss)
                        endif
779                 continue
                    ndoub3(iad+1) = 1
                endif
127         continue
130     end do
!
!     NOMBRE DE NOUVEAUX POINTS : NNN
        nnn = 0
!  -  ON AJOUTE LES PT D'INTER QUI NE SONT PAS DES NOEUDS
!  -  AUTANT DE FOIS QU'IL Y A DE COMBINAISON D'HEAVISIDE DIFFERENTES
        do 600 i = 1, ncomb*ncompp
            nnn = nnn + ndoub2(i)
600     end do
!
!  -  ON AJOUTE LES NOEUDS, AUTANT DE FOIS QU'IL Y A DE COMBINAISON
!  -  D'HEAVISIDE DIFFERENTES.
!     ON VA JUSQU'A NNO+NNC POUR PRENDRE EN COMPTE LES NOEUDS CENTRAUX
        do 500 i = 1, ncomb*(nno+nnc)
            nnn = nnn + ndoubl(i)
500     end do
!
        if (.not.iselli(elp)) then
!  - CAS QUADRATIQUE, ON AJOUTE LES NOUVEAUX NOEUDS MILIEUX DES SE
!  -  AUTANT DE FOIS QU'IL Y A DE COMBINAISON D'HEAVISIDE DIFFERENTES
!        nnn=nnn+nmfis+npm
            do 700 i = 1, ncomb*nmmax
                nnn = nnn + ndoub3(i)
700         end do
        endif
!
        zi(jout4-1+3)=nnn
        zi(jout4-1+4)=0
!   stockage des noeuds centraux servant a definir la connectivite sur sous-tetra courant
        call vecini(3, 0.d0, xg)
!   pour chaque noeud central
        do 401 j = 1, nnc
            deja=.false.
!     recuperation des coordonnees de refrence du noeud central courant
            do 23 k = 1, ndim
                xg(k)=nmil(k,j)
 23         continue
!     pour chaque noued de chaque sous-tetra
            do 400 i = 1, nit*nnose
!       si le noeud courant du sous-tetra courant est le noeud central courant
                if (zi(jout2-1+i) .eq. (nno+j)) then
!         s'il n'a pas deja ete ajoute, on ajoute le noeud central courant
                    if (.not. deja) then
                        deja=.true.
                        npm=npm+1
                        do 402 k = 1, ndim
                            zr(jout5+(npm-1)*ndim+k-1)=xg(k)
402                     continue
                        end if
!         NOMBRE TOTAL DE POINTS MILIEUX LIMITE A PMMAX
                        ASSERT(npm.le.pmmax)
!         stockage du noeud central cournant
                        zi(jout2-1+i)=3000+npm
                    endif
400                 continue
401                 continue
!
!     ARCHIVAGE DE LONCHAM POINTS MILIEUX
!
                    if (.not.iselli(elp)) then
                        zi(jout4-1+4)=npm
                    endif
!
                    call jedema()
                end subroutine
