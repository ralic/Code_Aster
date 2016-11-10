subroutine te0514(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/dismoi.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/iselli.h"
#include "asterfort/jevech.h"
#include "asterfort/loncar.h"
#include "asterfort/matini.h"
#include "asterfort/ndcent.h"
#include "asterfort/padist.h"
#include "asterfort/provec.h"
#include "asterfort/reeref.h"
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
#include "asterfort/xelrex.h"
#include "asterfort/xnormv.h"
#include "asterfort/xxmmvd.h"
#include "asterfort/lteatt.h"
#include "asterfort/ltequa.h"
!
    character(len=16) :: option, nomte
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8) :: elp, noma, typma, enr, enr2, elrese(3), typsma
    integer :: igeom, jlsn, ifisc, nfisc, ptmaxi, pmmaxi
    integer :: jpintt, jcnset, jheavt, jlonch, jpmilt, zintmx, nfimax
    integer :: iadzi, iazk24, nno, nnn, jfisco, nsemax, jout6
    integer :: ninter, nit, nse, nnose, ise2, ncomph, ncompp, ncompc
    integer :: npts, cnse(6, 10), i, j, k, it, npi, ipt, ise, in, ni, cpt
    integer :: ndim, ibid, ndime, iad, jtab(7), jtab2(2), vali(2)
    integer :: nnc, npm, nmilie, nmfis, nbar, ar(12,3)
    integer :: iret, nfiss, ifiss, ncomb, ninmax, nmmax, nbars, ars(12,3)
    integer :: a1, a2, b1, b2, ncompm, exit(2), joncno, jonact(8)
    parameter(ptmaxi=6,zintmx=5,pmmaxi=17,nsemax=6,nfimax=10)
    parameter(ninmax=44,nmmax=264)
    real(kind=8) :: nmil(3, 7), txlsn(28), ainter(ptmaxi*zintmx), rainter(4)
    real(kind=8) :: newpt(3), p(3), lonref, pinter(3*ptmaxi), lsn(3)
    real(kind=8) :: pmilie(3*pmmaxi), heav(nsemax*nfimax), u(3), v(3), normal(3)
    real(kind=8) :: xg(3), cridist, xref(81), ff(27), ptref(3), norme
    parameter(cridist=1.d-9)
    integer :: fisco(2*nfimax), fisc(2*nfimax), zxain, ai, nnos
    integer :: ndoubl(ninmax*(2**nfimax)), ndoub2(ninmax*(2**nfimax))
    integer :: ndoub3(nmmax*(2**nfimax)), coupe(nfimax)
    aster_logical :: deja, ajn, cut, lconnec_ok, pre1, jonc
!
    data            elrese /'SEG3','TRIA6','TETRA10'/
!......................................................................
!     LES TABLEAUX FISC, FISCO, NDOUBL, NDOUB2, PMILIE, PINTER ONT ETE
!     ALLOUE DE FACON STATIQUE POUR OPTIMISER LE CPU (CAR LES APPELS A
!     WKVECT DANS LES TE SONT COUTEUX).
!
    nomte=nomte
!
    ASSERT(option.eq.'TOPOSE')
    call vecini(51, 0.d0, pmilie)
    call vecini(30, 0.d0, ainter)
    call vecini(4, 0.d0, rainter)
!
    call elref1(elp)
    call elrefe_info(fami='RIGI', ndim=ndime, nno=nno, nnos=nnos)
!
    call tecael(iadzi, iazk24, noms=0)
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
!     RECUPERATION DES ENTREES / SORTIE
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PLEVSET', 'L', jlsn)
    call jevech('PPINTTO', 'E', jpintt)
    call jevech('PCNSETO', 'E', jcnset)
    call jevech('PHEAVTO', 'E', jheavt)
    call jevech('PLONCHA', 'E', jlonch)
!
    call teattr('S', 'XFEM', enr, ibid)
!
    if ((ibid.eq.0) .and. ltequa(elp,enr)) then
         call jevech('PPMILTO', 'E', jpmilt)
         call tecach('OOO', 'PPMILTO', 'E', iret, nval=2,&
                     itab=jtab2)
         ncompm = jtab2(2)/ndim
    endif
!
    call tecach('OOO', 'PHEAVTO', 'E', iret, nval=7,&
                itab=jtab)
    ncomph = jtab(2)
    nfiss = jtab(7)
!
!   recuperation de PAINTTO pour les elements principaux
    jout6 = 0
    if (ndim.eq.ndime) then
       call jevech('PAINTTO', 'E', jout6)
    endif
!
!   Le modèle est-il HM-XFEM
    call teattr('C', 'MODTHM', enr2, iret)
    pre1=(iret.eq.0)
    joncno = 1
    if (pre1 .and.(enr(1:4).eq.'XH2C'.or.enr(1:4).eq.'XH3C')) then
       call jevech('PJONCNO', 'E', joncno)
       do i = 1, 20
          zi(joncno-1+i) = 0
       end do
    endif
!
    do 9 i = 1, nfimax
        coupe(i) = 0
        fisco(2*i-1) = 0
        fisco(2*i) = 0
        fisc(2*i-1) = 0
        fisc(2*i) = 0
  9 continue
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
    call vecini(28, 0.d0, txlsn)
!
! si l'element est incomplet, on calcule les informations concernant les les noeuds centraux
    if ((elp.eq.'QU8') .or. (elp.eq.'H20') .or. (elp.eq.'P13') .or. (elp.eq.'P15')) then
        call ndcent(igeom, ndim, zr(jlsn), nfiss, nmil, txlsn,&
                    nnc)
    end if
!
 exit(1:2) = 0
 73 continue
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
    call xdivte(elp, zi(jcnset), nit, nnose, exit)
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
    do ifiss = 1, nfiss
!
        do i = 1, ifiss
            fisc(i)=0
        end do
        ifisc = ifiss
        nfisc = 0
 80     continue
        if (fisco(2*ifisc-1) .gt. 0) then
            nfisc = nfisc+1
            fisc(2*(nfisc-1)+2) = fisco(2*ifisc)
            ifisc = fisco(2*ifisc-1)
            fisc(2*(nfisc-1)+1) = ifisc
            goto 80
        endif
!
!       BOUCLE SUR LES NIT TETRAS
        do it = 1, nit
!
!         DECOUPAGE EN NSE SOUS-ELEMENTS
            call vecini(18, 0.d0, pinter)
            nmilie = 0
            ninter = 0
            npts = 0
            do i = 1, nsemax*nfimax
                heav(i)=0.d0
            end do
!
            if (.not.iselli(elp)) then
                call xdecqu(nnose, it, ndim, zi(jcnset), jlsn,&
                            igeom, pinter, ninter, npts, ainter,&
                            pmilie, nmilie, nmfis, nmil, txlsn,&
                            zr(jpintt), zr(jpmilt), ifiss, nfiss,&
                            fisc, nfisc, cut, coupe, exit, joncno)
                if (exit(1).eq.1) goto 73
!
                call xdecqv(nnose, it, zi(jcnset), zi(jheavt), zr(jlsn), igeom,&
                            ninter, npts, ndim, ainter, nse, cnse,&
                            heav, nsemax, pinter, pmilie, zr(jpintt), zr(jpmilt), cut,&
                            ncomph, nfisc, nfiss, ifiss, elp, fisc, lonref, txlsn, nmil)
            else
                call xdecou(ndim, elp, nno, nnose, it,&
                            zr(jpintt), zi(jcnset), zr(jlsn), fisc, igeom,&
                            nfiss, ifiss, pinter, ninter, npts,&
                            ainter, lonref, nfisc)
                call xdecov(ndim, elp, nno, nnose, it,&
                            zr(jpintt), zi(jcnset), zi(jheavt), ncomph, zr(jlsn),&
                            fisc, igeom, nfiss, ifiss, pinter,&
                            ninter, npts, ainter, nse, cnse,&
                            heav, nfisc, nsemax)
            endif
!
! ----- - BOUCLE SUR LES NINTER POINTS D'INTER : ARCHIVAGE DE PINTTO
            do 200 ipt = 1, ninter
                do 210 j = 1, ndim
                    newpt(j)=pinter(ndim*(ipt-1)+j) 
210              continue
                do i = 1, 4
                    rainter(i) = ainter(zxain*(ipt-1)+i)
                end do
!
!           VERIF SI EXISTE DEJA DANS PINTTO
                deja=.false.
                do 220 i = 1, npi
                    do 221 j = 1, ndim
                        p(j) = zr(jpintt-1+ndim*(i-1)+j)
221                  continue
                    if (padist(ndim,p,newpt) .lt. (lonref*cridist)) then
                        deja = .true.
                        ni=i
                    endif
220             continue
                ai=nint(ainter(zxain*(ipt-1)+1))
!             SI LE POINT D'INTER COINCIDE AVEC UN NOEUD
                if (ai .eq. 0) go to 200
                if (.not.deja) then
                    npi=npi+1
                    ni =npi
!             NOMBRE TOTAL DE PT D'INTER LIMITE A LA TAILLE DE LA CARTE
                    if (npi .gt. ncompp) then
                       call utmess('F', 'XFEM_55')
                    endif
!             ARCHIVAGE DE PINTTO
                    do 230 j = 1, ndim
                        zr(jpintt-1+ndim*(npi-1)+j)=newpt(j)
230                 continue
!
!             ARCHIVAGE DE PAINTTO POUR LES ELEMENTS PRINCIPAUX
                    if (ndim.eq.ndime) then
                       do k = 1, zxain
                          zr(jout6-1+zxain*(npi-1)+k)=0.d0
                       end do
                       if (ifiss.gt.1) then
!             MARQUAGE POINT DE JONCTION DE FISSURES
                          if (rainter(4).eq.-1.d0) zr(jout6-1+zxain*(npi-1)+4)=-1.d0
                          call conare(typma, ar, nbar)
                          call xelrex(elp, nno, xref)
                          call reeref(elp, nno, zr(igeom), newpt, ndim,&
                                      ptref, ff)
                          call vecini(3, 0.d0, u)
                          call vecini(3, 0.d0, v)
                          call vecini(3, 0.d0, normal)
                          do j = 1, nbar
                             do k = 1, ndim
                                u(k)=xref((ar(j,1)-1)*ndim+k)-xref((ar(j,2)-1)*ndim+k)
                                v(k)=ptref(k)-xref((ar(j,2)-1)*ndim+k)
                             end do
                             call provec(u, v, normal)
                             call xnormv(3, normal, norme)
                             if (norme.lt.cridist) then
                                zr(jout6-1+zxain*(npi-1)+1)=j
                                zr(jout6-1+zxain*(npi-1)+3)=rainter(3)
                                zr(jout6-1+zxain*(npi-1)+4)=rainter(4)
                             endif
                          end do
                       else
                          call conare(typma, ar, nbar)
                          typsma = elrese(ndim)
                          call conare(typsma, ars, nbars)
                          b1=zi(jcnset+nnose*(it-1)+ars(nint(rainter(1)),1)-1)
                          b2=zi(jcnset+nnose*(it-1)+ars(nint(rainter(1)),2)-1)
!
                          do j = 1, nbar
                             a1 = ar(j,1)
                             a2 = ar(j,2)
                             if (a1.eq.b1.and.a2.eq.b2) then
                                zr(jout6-1+zxain*(npi-1)+1)=j
                                zr(jout6-1+zxain*(npi-1)+3)=rainter(3)
                                zr(jout6-1+zxain*(npi-1)+4)=rainter(4)
                             else if (a1.eq.b2.and.a2.eq.b1) then
                                zr(jout6-1+zxain*(npi-1)+1)=j
                                zr(jout6-1+zxain*(npi-1)+3)=rainter(3)
                                zr(jout6-1+zxain*(npi-1)+4)=1.d0-rainter(4)
                             endif
                          end do
                       endif
                    endif
!
!             MISE A JOUR DU CNSE (TRANSFORMATION DES 100 EN 1000...)
                    do 240 ise = 1, nse
                        do 241 in = 1, ndime+1
                            if (cnse(ise,in) .eq. 100+ipt) cnse(ise,in)= 1000+ni
241                      continue
240                 continue
                else
                    do 114 ise = 1, nse
                        do 115 in = 1, ndime+1
                            if (cnse(ise,in) .eq. 100+ipt) cnse(ise,in)= 1000+ni
115                      continue
114                  continue
                endif
200         continue

!
! ------- BOUCLE SUR LES NMILIE POINTS MILIEUX : ARCHIVAGE DE PMILTO
            if (.not.iselli(elp)) then
                do 300 ipt = 1, nmilie
                    do 310 j = 1, ndim
                        newpt(j)=pmilie(ndim*(ipt-1)+j)
310                 continue
!             VERIF SI EXISTE DEJA DANS PMILTO
                    deja=.false.
                    do 320 i = 1, npm
                        do 321 j = 1, ndim
                            p(j) = zr(jpmilt-1+ndim*(i-1)+j)
321                     continue
                        if (padist(ndim,p,newpt) .lt. (lonref*cridist)) then
                            deja = .true.
                            ni=i
                        endif
320                 continue
                    if (.not.deja) then
                        npm=npm+1
!               NOMBRE TOTAL DE POINTS MILIEUX LIMITE A PMMAX
                        if (npm .gt. ncompm) then
                           call utmess('F', 'XFEM_55')
                        endif
!               ARCHIVAGE DE PMILTO
                        do 330 j = 1, ndim
                            zr(jpmilt-1+ndim*(npm-1)+j)=pmilie(ndim*(ipt-1)+j)
330                     continue
!
!               MISE A JOUR DU CNSE (TRANSFORMATION DES 200 EN 2000...)
                        do 340 ise = 1, nse
                            do 341 in = 1, nnose
                                if (cnse(ise,in) .eq. 200+ipt) cnse(ise, in)=2000+npm
341                         continue
340                     continue
                    else
                        do 350 ise = 1, nse
                            do 351 in = 1, nnose
                                if (cnse(ise,in) .eq. 200+ipt) cnse(ise, in)=2000+ni
351                         continue
350                     continue
                    endif
!
300             continue
            endif
! ------- BOUCLE SUR LES NSE SOUS-ELE : ARCHIVAGE DE PCNSETO, PHEAVTO
            do ise = 1, nse
                if (ise .eq. 1) then
                    ise2=it
                else
                    cpt=cpt+1
                    ise2=cpt
                endif
!         NOMBRE TOTAL DE SOUS-ELEMENTS LIMITE A LA TAILLE DE LA CARTE
!           ARCHIVAGE DE PHEAVTO
                do i = 1, ifiss
                    zi(jheavt-1+ncomph*(i-1)+ise2)= nint(heav(ifiss*(&
                ise-1)+i))
                end do
!           ARCHIVAGE DE PCNSETO
                lconnec_ok=.true.   
                do in = 1, nnose
                    lconnec_ok=lconnec_ok.and.&
                                ((cnse(ise,in).gt.0.and.cnse(ise,in).le.nno+nnc).or.&
                                (cnse(ise,in).gt.1000.and.cnse(ise,in).le.1000+ncompp).or.&
                                (cnse(ise,in).gt.2000.and.cnse(ise,in).le.2000+ncompm))
                    zi(jcnset-1+nnose*(ise2-1)+in)=cnse(ise,in)
                end do
                if (.not. lconnec_ok) then
                  call utmess('F', 'XFEMPRECOND_8')
                endif 
            end do
!
        end do
        nit = cpt
!
    end do
!
!     ARCHIVAGE DE LONCHAM SOUS ELEMENTS
    if (nit*nnose .gt. ncompc) then
       call utmess('F', 'XFEM_55')
    endif
    zi(jlonch-1+1)=nit
!
!     REMPLISSAGE DE PJONCNO POUR LES ELEMENTS QUI CONTIENNENT LA JONCTION
    jonc = .false.
    do i = 1, 8
       jonact(i)=0
    end do
    if (pre1 .and. (enr(1:4).eq.'XH2C'.or.enr(1:4).eq.'XH3C')) then
       do i = 1, nnos
          if (zi(joncno-1+i).eq.1) jonc=.true.
       end do
       if (jonc) then
          call conare(typma, ar, nbar)
          do i = 1, nbar
             lsn(1)=zr(jlsn-1+(ar(i,1)-1)*nfiss+1)
             lsn(2)=zr(jlsn-1+(ar(i,2)-1)*nfiss+1)
             lsn(3)=zr(jlsn-1+(ar(i,3)-1)*nfiss+1)
             if (lsn(1).eq.0.d0) then
                jonact(ar(i,1))=1
             elseif (lsn(2).eq.0.d0) then
                jonact(ar(i,2))=1
             elseif (lsn(3).eq.0.d0) then
                jonact(ar(i,1))=1
                jonact(ar(i,2))=1
             elseif (lsn(1)*lsn(2).lt.0.d0) then
                jonact(ar(i,1))=1
                jonact(ar(i,2))=1
             endif
          end do
          do i = 1, nnos
             if (jonact(i).eq.0) zi(joncno-1+i) = 0
          end do
          do i = 1, nnos
             if (zi(joncno-1+i).ge.1) zi(joncno-1+i) = jonact(i)
          end do
       endif
    endif
! 
!     ARCHIVAGE DE LONCHAM POINTS D'INTERSECTION
    zi(jlonch-1+2)=npi
!
!     SERT UNIQUEMENT POUR LA VISU
!
!     POUR COMPTER LES NOEUDS EN DOUBLE (EN POST-TRAITEMENT)
!     NCOMB COMBINAISONS POSSIBLES
    ncomb = 2**nfiss
!     ON VA JUSQU'A NCOMPP, NOMBRE MAX DE POINTS D'INTERSECTION
    ASSERT(ncompp.le.ninmax)
    do i = 1, ninmax*(2**nfimax)
        ndoub2(i)=0
    end do
!     POUR COMPTER LES PT D'INTER EN DOUBLE (EN POST-TRAITEMENT)
!     ON VA JUSQU'A NNO+1 POUR PRENDRE EN COMPTE LE 9EME NOEUD DU QUAD8
    ASSERT((nno+nnc).le.ninmax)
    do i = 1, ninmax*(2**nfimax)
        ndoubl(i)=0
    end do
!     POUR COMPTER LES PT MILIEU EN DOUBLE (EN POST-TRAITEMENT)
!     ON VA JUSQU'A NNO+NNC POUR PRENDRE EN COMPTE LES NOEUDS CENTRAUX
    do i = 1, nmmax*(2**nfimax)
        ndoub3(i)=0
    end do
!
    do 130 ise = 1, nit
        do 127 in = 1, nnose
            i = zi(jcnset-1+nnose*(ise-1)+in)
            if (i .lt. 1000) then
! ----- ON SE PLACE EN BASE 2, LA DIMENSSION DU PB EST NFISS
! ----- POUR CHAQUE FISSURE H=-1 OU 0=>0
!                           H=+1     =>1
                iad = ncomb*(i-1)
                do 128 ifiss = 1, nfiss
                    if (zi(jheavt-1+ncomph*(ifiss-1)+ise) .eq. 1) then
                        iad = iad + 2**(nfiss-ifiss)
                    endif
128             continue
                ndoubl(iad+1) = 1
            else if (i.gt.1000.and.i.lt.2000) then
                iad = ncomb*(i-1001)
                do 129 ifiss = 1, nfiss
                    if (zi(jheavt-1+ncomph*(ifiss-1)+ise) .eq. 1) then
                        iad = iad + 2**(nfiss-ifiss)
                    endif
129             continue
                ndoub2(iad+1) = 1
            else if (i.gt.2000.and.i.lt.3000) then
                iad = ncomb*(i-2001)
                do 779 ifiss = 1, nfiss
                    if (zi(jheavt-1+ncomph*(ifiss-1)+ise) .eq. 1) then
                        iad = iad + 2**(nfiss-ifiss)
                    endif
779             continue
                ndoub3(iad+1) = 1
            endif
127     continue
130 continue
!
!     NOMBRE DE NOUVEAUX POINTS : NNN
    nnn = 0
!  -  ON AJOUTE LES PT D'INTER QUI NE SONT PAS DES NOEUDS
!  -  AUTANT DE FOIS QU'IL Y A DE COMBINAISON D'HEAVISIDE DIFFERENTES
    do 600 i = 1, ncomb*ncompp
        nnn = nnn + ndoub2(i)
600 continue
!
!  -  ON AJOUTE LES NOEUDS, AUTANT DE FOIS QU'IL Y A DE COMBINAISON
!  -  D'HEAVISIDE DIFFERENTES.
!     ON VA JUSQU'A NNO+NNC POUR PRENDRE EN COMPTE LES NOEUDS CENTRAUX
    do 500 i = 1, ncomb*(nno+nnc)
        nnn = nnn + ndoubl(i)
500 continue
!
    if (.not.iselli(elp)) then
!  - CAS QUADRATIQUE, ON AJOUTE LES NOUVEAUX NOEUDS MILIEUX DES SE
!  -  AUTANT DE FOIS QU'IL Y A DE COMBINAISON D'HEAVISIDE DIFFERENTES
!        nnn=nnn+nmfis+npm
        do 700 i = 1, ncomb*nmmax
            nnn = nnn + ndoub3(i)
700     continue
    endif
!
    zi(jlonch-1+3)=nnn
    zi(jlonch-1+4)=0
!   stockage des noeuds centraux servant a definir la connectivite sur sous-tetra courant
        call vecini(3, 0.d0, xg)
!   pour chaque noeud central
        do 401 j = 1, nnc
            deja=.false.
!     recuperation des coordonnees de refrence du noeud central courant
            do 23 k = 1, ndim
                xg(k)=nmil(k,j)
 23         continue
!     pour chaque noeud de chaque sous-tetra
            do 400 i = 1, nit*nnose
!       si le noeud courant du sous-tetra courant est le noeud central courant
            if (zi(jcnset-1+i) .eq. (nno+j)) then
!         s'il n'a pas deja ete ajoute, on ajoute le noeud central courant
                if (.not. deja) then
                    deja=.true.
                    npm=npm+1
                    do 402 k = 1, ndim
                        zr(jpmilt+(npm-1)*ndim+k-1)=xg(k)
402                 continue
                end if
!         NOMBRE TOTAL DE POINTS MILIEUX LIMITE A PMMAX
                if (npm .gt. ncompm) then
                   call utmess('F', 'XFEM_55')
                endif
!         stockage du noeud central courant
                zi(jcnset-1+i)=3000+npm
            endif
400     continue
401 continue
!
!     ARCHIVAGE DE LONCHAM POINTS MILIEUX
!
    if (.not.iselli(elp)) then
       zi(jlonch-1+4)=npm
    endif
!
end subroutine
