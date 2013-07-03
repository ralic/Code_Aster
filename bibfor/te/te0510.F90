subroutine te0510(option, nomte)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/dismoi.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/iselli.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/reeref.h"
#include "asterfort/teattr.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/vecini.h"
#include "asterfort/xcface.h"
#include "asterfort/xcfaq2.h"
#include "asterfort/xxmmvd.h"
#include "blas/ddot.h"
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
!       CALCUL DES DONNÉES TOPOLOGIQUES CONCERNANT LES INTERSECTIONS
!              DES ÉLÉMENTS ENRICHIS ET DU PLAN DE LA FISSURE
!
!
!  OPTION : 'TOPOFA' (X-FEM TOPOLOGIE DES FACETTES DE CONTACT)
!
!  ENTREES  ---> OPTION : OPTION DE CALCUL
!           ---> NOMTE  : NOM DU TYPE ELEMENT
!
!......................................................................
!
!
    character(len=8) :: elp, typma, noma, k8bid
    integer :: igeom, jlsn, jlst, jgrlsn, jgrlst
    integer :: jout1, jout2, jout3, jout4, jout5, jout6, jout7
    integer :: iadzi, iazk24
    integer :: ninter, nface, cface(5, 3), ar(12, 3), nbar, nmaabs
    integer :: i, j, k, jj, nnop
    real(kind=8) :: nd(3), grlt(3), tau1(3), tau2(3), norme, ps
    real(kind=8) :: norm2, ptree(3), ptref(3), rbid, rbid6(6), rbid3(3, 3)
    real(kind=8) :: ff(20), dfdi(20, 3), lsn
    integer :: ndim, ibid, nptf, nbtot, nfiss, jtab(7), iret
    logical :: lbid, elim, elim2
    integer :: zxain, ifiss, ncompp, ncompa, ncompb, ncompc
    integer :: jfisco, jfiss, kfiss, kcoef, ncomph, he, hescl, hmait
    integer :: nfisc, ifisc, nfisc2, nn, vali(2)
    character(len=16) :: enr
!
!     ALLOCATION DES OBJETS TEMPORAIRES A UNE TAILLE SUFFISANTE
!     (N'EST PAS EXACTEMENT LA TAILLE DES OBJETS EN SORTIE)
    integer :: ptmaxi
    parameter    (ptmaxi=7)
    real(kind=8) :: pinter(ptmaxi*3)
!
    integer :: zxainx
    parameter    (zxainx=5)
    real(kind=8) :: ainter(ptmaxi*zxainx)
!
    integer :: nfimax
    parameter    (nfimax=10)
    integer :: fisc(2*nfimax), fisco(2*nfimax)
!
    integer :: nbmax
    parameter    (nbmax=18)
    integer :: pthea(nfimax*nbmax)
!
!......................................................................
!     LES TABLEAUX FISC, FISCO, PTHEA, PINTER, AINTER ONT ETE ALLOUE DE
!     FACON STATIQUE POUR OPTIMISER LE CPU (CAR LES APPELS A WKVECT
!     DANS LES TE SONT COUTEUX).
!
    call assert(option.eq.'TOPOFA')
!
    call jemarq()
!
    zxain = xxmmvd('ZXAIN')
    call assert(zxain.eq.zxainx)
!
    call elref1(elp)
    call elref4(elp, 'RIGI', ndim, nnop, ibid,&
                ibid, ibid, ibid, ibid, ibid)
!
!     RECUPERATION DES ENTRÉES / SORTIE
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
    call jevech('PGRADLN', 'L', jgrlsn)
    call jevech('PGRADLT', 'L', jgrlst)
!
    call jevech('PPINTER', 'E', jout1)
    call jevech('PAINTER', 'E', jout2)
    call jevech('PCFACE', 'E', jout3)
    call jevech('PLONCHA', 'E', jout4)
    call jevech('PBASECO', 'E', jout5)
    call jevech('PGESCLA', 'E', jout6)
!
    call tecael(iadzi, iazk24)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)(1:8)
    noma=zk24(iazk24)
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                k8bid, iret)
    nmaabs=zi(iadzi)
!
    call conare(typma, ar, nbar)
    call teattr(nomte, 'S', 'XFEM', enr, ibid)
    if (enr .eq. 'XH1' .or. enr .eq. 'XH2' .or. enr .eq. 'XH3' .or. enr .eq. 'XH4') then
! --- PAS D'ELEMENTS COUPÉES PLUSIEURS FOIS SANS CONTACT POUR L'INSTANT
        goto 999
    endif
    call tecach('NOO', 'PLST', 'L', 7, jtab,&
                iret)
!     NOMBRE DE FISSURES
    nfiss = jtab(7)
    vali(1)=nfimax
    vali(2)=nfiss
    if (nfiss .gt. nfimax) call u2mesi('F', 'XFEM2_6', 2, vali)
    do 70 i = 1, 2*nfimax
        fisco(i)=0
        fisc(i)=0
70  end do
    nn=nfimax*nbmax
    do 72 i = 1, nn
        pthea(i)=0
72  end do
    if (nfiss .gt. 1) then
        call jevech('PFISCO', 'L', jfisco)
        do 73 i = 1, 2*nfiss
            fisco(i)=zi(jfisco-1+i)
73      continue
        call jevech('PHEAVFA', 'E', jout7)
        call tecach('OOO', 'PHEAVFA', 'E', 2, jtab,&
                    iret)
        ncomph = jtab(2)
    endif
!
!     DIMENSIONS DES GRANDEURS DANS LA CARTE
    call tecach('OOO', 'PPINTER', 'E', 2, jtab,&
                iret)
    ncompp = jtab(2)
    call tecach('OOO', 'PGESCLA', 'E', 2, jtab,&
                iret)
    call assert(jtab(2).eq.ncompp)
    call tecach('OOO', 'PAINTER', 'E', 2, jtab,&
                iret)
    ncompa = jtab(2)
    call tecach('OOO', 'PBASECO', 'E', 2, jtab,&
                iret)
    ncompb = jtab(2)
    call tecach('OOO', 'PCFACE', 'E', 2, jtab,&
                iret)
    ncompc = jtab(2)
!
! --- BOUCLE SUR LES FISSURES
!
    do 10 ifiss = 1, nfiss
        nface = 0
        nptf = 0
! ----------------------------------------------------------------------
!       RECHERCHE DES INTERSECTIONS ARETES-FISSURE
!       ET DÉCOUPAGE EN FACETTES
        do 81 i = 1, 2*nfiss
            fisc(i)=0
81      continue
        ifisc = ifiss
        nfisc = 0
80      continue
        if (fisco(2*ifisc-1) .gt. 0) then
!       STOCKAGE DES FISSURES SUR LESQUELLES IFISS SE BRANCHE
            nfisc = nfisc+1
            fisc(2*(nfisc-1)+2) = fisco(2*ifisc)
            ifisc = fisco(2*ifisc-1)
            fisc(2*(nfisc-1)+1) = ifisc
            goto 80
        endif
!
        nfisc2 = 0
        do 20 jfiss = ifiss+1, nfiss
!       STOCKAGE DES FISSURES QUI SE BRANCHENT SUR IFISS
            kfiss = fisco(2*jfiss-1)
            do 30 i = nfisc+1, nfisc+nfisc2
                if (fisc(2*(i-1)+1) .eq. kfiss) then
                    nfisc2 = nfisc2 + 1
                    fisc(2*(nfisc+nfisc2-1)+1) = jfiss
                endif
30          continue
            if (kfiss .eq. ifiss) then
                nfisc2 = nfisc2 + 1
                fisc(2*(nfisc+nfisc2-1)+1) = jfiss
            endif
20      continue
!
        if (.not.iselli(elp) .and. ndim .le. 2) then
            call xcfaq2(jlsn, jlst, jgrlsn, igeom, noma,&
                        nmaabs, pinter, ninter, ainter, nface,&
                        nptf, cface, nbtot)
        else
            call xcface(elp, zr(jlsn), zr(jlst), jgrlsn, igeom,&
                        enr, nfiss, ifiss, fisc, nfisc,&
                        noma, nmaabs, pinter, ninter, ainter,&
                        nface, nptf, cface)
            nbtot=ninter
        endif
        if (nfiss .gt. 1 .and. nbtot .gt. 0) then
            do 109 i = 1, nbtot*nfiss
                pthea(i)=0
109          continue
        endif
!       ARCHIVAGE DE PINTER, AINTER, GESCLA, GMAITR ET BASECO
!
        do 110 i = 1, nbtot
            do 111 j = 1, ndim
                ptree(j)=pinter(ndim*(i-1)+j)
                zr(jout6-1+ncompp*(ifiss-1)+ndim*(i-1)+j) = ptree(j)
111          continue
!    ON TRANFORME LES COORDONNÉES RÉELES EN COORD. DANS L'ÉLÉMENT DE REF
            call reeref(elp, lbid, nnop, ibid, zr(igeom),&
                        ptree, 1, lbid, ndim, rbid,&
                        rbid, rbid, ibid, ibid, ibid,&
                        ibid, ibid, ibid, rbid, rbid3,&
                        'NON', ptref, ff, dfdi, rbid3,&
                        rbid6, rbid3)
!
            do 112 jj = 1, ndim
                zr(jout1-1+ncompp*(ifiss-1)+ndim*(i-1)+jj) = ptref(jj)
112          continue
            do 113 j = 1, zxain-1
                zr(jout2-1+ncompa*(ifiss-1)+zxain*(i-1)+j)= ainter(&
                zxain*(i-1)+j)
113          continue
!
!     CALCUL DE LA BASE COVARIANTE AUX POINTS D'INTERSECTION
!     ND EST LA NORMALE À LA SURFACE : GRAD(LSN)
!     TAU1 EST LE PROJETÉ DE GRAD(LST) SUR LA SURFACE
!     TAU2 EST LE PRODUIT VECTORIEL : ND ^ TAU1
!
!       INITIALISATION TAU1 POUR CAS 2D
            tau1(3)=0.d0
            call vecini(3, 0.d0, nd)
            call vecini(3, 0.d0, grlt)
!
            do 114 j = 1, ndim
                do 115 k = 1, nnop
                    nd(j) = nd(j) + ff(k)*zr(jgrlsn-1+ndim*(nfiss*(k- 1)+ifiss-1)+j)
                    grlt(j) = grlt(j) + ff(k)*zr(jgrlst-1+ndim*(nfiss* (k-1)+ifiss-1)+j)
115              continue
114          continue
!
            call normev(nd, norme)
            ps=ddot(ndim,grlt,1,nd,1)
            do 116 j = 1, ndim
                tau1(j)=grlt(j)-ps*nd(j)
116          continue
!
            call normev(tau1, norme)
!
            if (norme .lt. 1.d-12) then
!           ESSAI AVEC LE PROJETE DE OX
                tau1(1)=1.d0-nd(1)*nd(1)
                tau1(2)=0.d0-nd(1)*nd(2)
                if (ndim .eq. 3) tau1(3)=0.d0-nd(1)*nd(3)
                call normev(tau1, norm2)
                if (norm2 .lt. 1.d-12) then
!             ESSAI AVEC LE PROJETE DE OY
                    tau1(1)=0.d0-nd(2)*nd(1)
                    tau1(2)=1.d0-nd(2)*nd(2)
                    if (ndim .eq. 3) tau1(3)=0.d0-nd(2)*nd(3)
                    call normev(tau1, norm2)
                endif
                call assert(norm2.gt.1.d-12)
            endif
            if (ndim .eq. 3) then
                call provec(nd, tau1, tau2)
            endif
!
            do 117 j = 1, ndim
                zr(jout5-1+ncompb*(ifiss-1)+ndim*ndim*(i-1)+j)&
                =nd(j)
                zr(jout5-1+ncompb*(ifiss-1)+ndim*ndim*(i-1)+j+ndim)=&
                tau1(j)
                if (ndim .eq. 3) zr( jout5-1+ncompb*(ifiss-1)+ndim* ndim*(i-1)+j+2*ndim)=tau2(j )
117          continue
!
            if (nfiss .gt. 1) then
!    CALCUL DES FONCTIONS HEAVISIDE AUX POINTS D'INTER
                do 130 jfiss = 1, nfiss
                    lsn = 0
                    do 131 k = 1, nnop
                        lsn = lsn + ff(k) * zr(jlsn-1+nfiss*(k-1)+ jfiss)
131                  continue
                    if (abs(lsn) .gt. 1.d-11) then
                        pthea(nfiss*(i-1)+jfiss) = nint(sign(1.d0,lsn) )
                    endif
130              continue
            endif
!
110      continue
!
!     ARCHIVAGE DE CFACE ET DE HEAVFA
        do 120 i = 1, nface
            do 121 j = 1, nptf
                zi(jout3-1+ncompc*(ifiss-1)+nptf*(i-1)+j)=cface(i,j)
121          continue
            if (nfiss .gt. 1) then
                elim = .false.
                elim2= .false.
                do 122 jfiss = 1, nfiss
                    if (jfiss .eq. ifiss) then
!    ESCLAVE = -1, MAITRE = +1
                        hescl = -1
                        hmait = +1
                    else
                        he = 0
                        do 123 j = 1, nptf
                            if (pthea(nfiss*(cface(i,j)-1)+jfiss) .ne. 0 .and.&
                                pthea(nfiss*(cface(i,j)-1)+ jfiss) .ne. he .and. he .ne. 0) then
                                elim = .true.
                            endif
                            if (he .eq. 0) he=pthea(nfiss*(cface(i,j)-1)+ jfiss )
!
123                      continue
!
!    ESCLAVE = HE, MAITRE = HE
                        hescl = he
                        hmait = he
!    ON MODIFIE LA VALEUR DANS LE CAS DE FONCTION JONCTION
                        kfiss = jfiss
124                      continue
                        if (fisco(2*(kfiss-1)+1) .gt. 0 .and. he .ne. 0) then
                            kcoef = fisco(2*(kfiss-1)+2)
                            kfiss = fisco(2*(kfiss-1)+1)
                            if (kfiss .eq. ifiss) then
!    SI ON SE BRANCHE DU COTÉ MAITRE, MISE À ZÉRO DU COTÉ ESCLAVE
                                if (kcoef .eq. -1) hescl = 0
!    SI ON SE BRANCHE DU COTÉ ESCLAVE, MISE À ZÉRO DU COTÉ MAITRE
                                if (kcoef .eq. 1) hmait = 0
                            else
!    SI PAS BRANCHÉ ET PAS DU BON CÔTÉ, MISE À ZÉRO DES DEUX CÔTÉS
                                he = 0
                                do 125 j = 1, nptf
                                    if (he .eq. 0) he=pthea( nfiss*( cface(i,j)-1)+kfiss)
125                              continue
                                if (kcoef*he .eq. 1) then
                                    hescl = 0
                                    hmait = 0
                                    elim = .false.
                                endif
                            endif
                            goto 124
                        endif
                        elim2 = elim2.or.elim
                    endif
                    zi(jout7-1+ncomph*(nfiss*(ifiss-1)+jfiss-1)+2*i-1)&
                    = hescl
                    zi(jout7-1+ncomph*(nfiss*(ifiss-1)+jfiss-1)+2*i)&
                    = hmait
122              continue
                if (elim2) then
                    call u2mesk('A', 'XFEM_45', 1, nomte)
                    goto 998
                endif
            endif
120      continue
!
!     ARCHIVAGE DE LONCHAM
!
        zi(jout4+3*(ifiss-1)-1+2)=nface
998      continue
        zi(jout4+3*(ifiss-1)-1+1)=ninter
!
        zi(jout4+3*(ifiss-1)-1+3)=nptf
!
        if (nfiss .eq. 1) then
            do 710 i = 1, 2*nfimax
                fisco(i)=0
710          continue
        endif
!
!
10  end do
!
999  continue
!
    call jedema()
end subroutine
