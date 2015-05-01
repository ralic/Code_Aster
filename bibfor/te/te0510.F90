subroutine te0510(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/getvtx.h"
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
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/xcface.h"
#include "asterfort/xcfaq2.h"
#include "asterfort/xfacxh.h"
#include "asterfort/xfacxt.h"
#include "asterfort/xxmmvd.h"
#include "blas/ddot.h"
!
    character(len=16) :: option, nomte
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       CALCUL DES DONNÃES TOPOLOGIQUES CONCERNANT LES INTERSECTIONS
!              DES ÃLÃMENTS ENRICHIS ET DU PLAN DE LA FISSURE
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
    character(len=8) :: elp, noma
    character(len=16) :: typdis, face
    integer :: igeom, jlsn, jlst, jgrlsn, jgrlst
    integer :: jcnset, jpint, jmilt, jnit, jaint
    integer :: jout1, jout2, jout3, jout4, jout5, jout6, jout7, jphe
    integer :: iadzi, iazk24
    integer :: ninter, nface, cface(18, 6), nmaabs
    integer :: i, j, k, jj, nnop
    real(kind=8) :: nd(3), grlt(3), tau1(3), tau2(3), norme, ps
    real(kind=8) :: norm2, ptree(3), ptref(3)
    real(kind=8) :: lsn, minlst
    integer :: ndim, ibid, nptf, nbtot, nfiss, jtab(7), iret
    aster_logical :: elim, elim2
    integer :: zxain, ifiss, ncompp, ncompa, ncompb, ncompc
    integer :: ncompe
    integer :: jfisco, jfiss, kfiss, kcoef, ncomph, he, hescl, hmait
    integer :: nfisc, ifisc, nfisc2, nn, vali(2), jtyp
    character(len=16) :: enr, motfac
!
!     ALLOCATION DES OBJETS TEMPORAIRES A UNE TAILLE SUFFISANTE
!     (N'EST PAS EXACTEMENT LA TAILLE DES OBJETS EN SORTIE)
    integer :: ptmaxi
    parameter    (ptmaxi=34)
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
    parameter    (nbmax=28)
    integer :: pthea(nfimax*nbmax)
!
    integer :: nnopma
    parameter    (nnopma=20)
    real(kind=8) :: ff(nnopma)
    data motfac /' '/
!......................................................................
!     LES TABLEAUX FISC, FISCO, PTHEA, PINTER, AINTER ONT ETE ALLOUE DE
!     FACON STATIQUE POUR OPTIMISER LE CPU (CAR LES APPELS A WKVECT
!     DANS LES TE SONT COUTEUX).
!
    ASSERT(option.eq.'TOPOFA')
!
    call jemarq()
!
    zxain = xxmmvd('ZXAIN')
    ASSERT(zxain.eq.zxainx)
!     RECUPERATION DU TYPE DE FACETTES A GENERER
    call getvtx(motfac, 'DECOUPE_FACETTE', iocc=1, scal=face, nbret=ibid)
!
    call elref1(elp)
    call elrefe_info(elrefe=elp, fami='RIGI', ndim=ndim, nno=nnop)
    ASSERT(nnop .le. nnopma)
!
!     RECUPERATION DES ENTRÃES / SORTIE
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
    call jevech('PGRADLN', 'L', jgrlsn)
    call jevech('PGRADLT', 'L', jgrlst)
    call jevech('PPINTTO', 'L', jpint)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PLONCHA', 'L', jnit)
    call jevech('PHEAVTO', 'L', jphe)
    call jevech('PAINTTO', 'L', jaint)
!
!   PREMIERE DETECTION TYPE DISCONTINUITE
!   SUR LE TYPE D ELEMENT
    call teattr('S', 'XFEM', enr, ibid)
    if(enr(2:2).eq.'T'.or.enr(3:3).eq.'T') then
        typdis = 'FISSURE'
    else if(enr(1:3).eq.'XH1'.or.enr(1:3).eq.'XH2'.or.&
            enr(1:3).eq.'XH3'.or.enr(1:3).eq.'XH4') then
!
!   MULTI-HEAVISIDE : ON GERE LA DECOUPE COMME UNE FISSURE
        typdis = 'FISSURE'
    else
        call jevech('PTYPDIS', 'L', jtyp)
        if(zi(jtyp).eq.1) typdis='FISSURE'
        if(zi(jtyp).eq.2) typdis='INTERFACE'
        if(zi(jtyp).eq.3) typdis='COHESIF'
    endif
!
    call jevech('PPINTER', 'E', jout1)
    call jevech('PAINTER', 'E', jout2)
    call jevech('PCFACE', 'E', jout3)
    call jevech('PLONGCO', 'E', jout4)
    call jevech('PBASECO', 'E', jout5)
    call jevech('PGESCLA', 'E', jout6)
!
    call tecael(iadzi, iazk24, noms=0)
    noma=zk24(iazk24)
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
    nmaabs=zi(iadzi)
!
    if (enr .eq. 'XH1' .or. enr .eq. 'XH2' .or. enr .eq. 'XH3' .or. enr .eq. 'XH4') then
! --- PAS D'ELEMENTS COUPÃES PLUSIEURS FOIS SANS CONTACT POUR L'INSTANT
        goto 999
    endif
!
    if ((ibid.eq.0) .and. (enr.eq.'XH' .or.enr.eq.'XHT'.or.enr.eq.'XT'&
        .or.enr.eq.'XHC') .and. .not.iselli(elp)) then
         call jevech('PPMILTO', 'L', jmilt)
    endif
!
    call tecach('NOO', 'PLST', 'L', iret, nval=7,&
                itab=jtab)
!     NOMBRE DE FISSURES
    nfiss = jtab(7)
    vali(1)=nfimax
    vali(2)=nfiss
    if (nfiss .gt. nfimax) then
        call utmess('F', 'XFEM2_6', ni=2, vali=vali)
    endif
    do i = 1, 2*nfimax
        fisco(i)=0
        fisc(i)=0
    end do
    nn=nfimax*nbmax
    do i = 1, nn
        pthea(i)=0
    end do
    if (nfiss .gt. 1) then
        call jevech('PFISCO', 'L', jfisco)
        do i = 1, 2*nfiss
            fisco(i)=zi(jfisco-1+i)
        end do
        call jevech('PHEAVFA', 'E', jout7)
        call tecach('OOO', 'PHEAVFA', 'E', iret, nval=2,&
                    itab=jtab)
        ncomph = jtab(2)
    endif
!
!     DIMENSIONS DES GRANDEURS DANS LA CARTE
    call tecach('OOO', 'PPINTER', 'E', iret, nval=2,&
                itab=jtab)
    ncompp = jtab(2)
    call tecach('OOO', 'PGESCLA', 'E', iret, nval=2,&
                itab=jtab)
    ASSERT(jtab(2).eq.ncompp)
    call tecach('OOO', 'PAINTER', 'E', iret, nval=2,&
                itab=jtab)
    ncompa = jtab(2)
    call tecach('OOO', 'PBASECO', 'E', iret, nval=2,&
                itab=jtab)
    ncompb = jtab(2)
    call tecach('OOO', 'PCFACE', 'E', iret, nval=2,&
                itab=jtab)
    ncompc = jtab(2)
!
! --- BOUCLE SUR LES FISSURES
!
    do ifiss = 1, nfiss
        nface = 0
        nptf = 0
! ----------------------------------------------------------------------
!       RECHERCHE DES INTERSECTIONS ARETES-FISSURE
!       ET DÃCOUPAGE EN FACETTES
        do i = 1, 2*nfiss
            fisc(i)=0
        end do
        ifisc = ifiss
        nfisc = 0
 80     continue
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
        do jfiss = ifiss+1, nfiss
!       STOCKAGE DES FISSURES QUI SE BRANCHENT SUR IFISS
            kfiss = fisco(2*jfiss-1)
            do i = nfisc+1, nfisc+nfisc2
                if (fisc(2*(i-1)+1) .eq. kfiss) then
                    nfisc2 = nfisc2 + 1
                    fisc(2*(nfisc+nfisc2-1)+1) = jfiss
                endif
            end do
            if (kfiss .eq. ifiss) then
                nfisc2 = nfisc2 + 1
                fisc(2*(nfisc+nfisc2-1)+1) = jfiss
            endif
        end do
!
        if (enr(2:2) .eq. 'H' .and. face(1:15).eq.'SOUS_ELEMENTS') then
            call xfacxh(elp, jpint, jmilt, jnit, jcnset, pinter, ninter,&
                        jphe, ndim, ainter, nface, nptf, cface, &
                        igeom ,jlsn, jaint, jgrlsn, nfiss, ifiss,&
                        fisc, nfisc, ncompe, nnop)
                        nbtot = ninter
        elseif (enr(2:2) .eq. 'T' .and. face(1:15).eq.'SOUS_ELEMENTS') then
            call xfacxt(elp, jpint, jmilt, jnit, jcnset, pinter, ninter,&
                        jphe, ndim, ainter, nface, nptf, cface, &
                        igeom, jlsn, jlst, jaint, jgrlsn)
                        nbtot = ninter
        elseif (iselli(elp) .or. ndim .eq. 3) then
            call xcface(zr(jlsn), zr(jlst), jgrlsn, igeom,&
                        enr, nfiss, ifiss, fisc, nfisc,&
                        noma, nmaabs, typdis, pinter, ninter, ainter,&
                        nface, nptf, cface, minlst)
            nbtot = ninter
            if(typdis.eq.'COHESIF'.and.minlst.ge.0.d0) then
                nptf = 0
                ninter = 0
                nface = 0
                goto 97
            endif
        else
            call xcfaq2(jlsn, jlst, jgrlsn, igeom, noma,&
                        nmaabs, pinter, ainter, nface,&
                        nptf, cface, ninter, nfiss, ifiss)
            nbtot = ndim
        endif
!
        if (nfiss .gt. 1 .and. ninter .gt. 0) then
            do i = 1, ninter*nfiss
                pthea(i)=0
            end do
        endif
!       ARCHIVAGE DE PINTER, AINTER, GESCLA, GMAITR ET BASECO
!
        do i = 1, ninter
            do j = 1, ndim
                ptree(j)=pinter(ndim*(i-1)+j)
                zr(jout6-1+ncompp*(ifiss-1)+ndim*(i-1)+j) = pinter(ndim*(i-1)+j)
            end do
!    ON TRANFORME LES COORDONNÃES RÃELES EN COORD. DANS L'ÃLÃMENT DE REF
            call reeref(elp, nnop, zr(igeom), ptree, ndim,&
                        ptref, ff)
!
            do jj = 1, ndim
                zr(jout1-1+ncompp*(ifiss-1)+ndim*(i-1)+jj) = ptref(jj)
            end do
            do j = 1, zxain-1
                zr(jout2-1+ncompa*(ifiss-1)+zxain*(i-1)+j)= ainter(&
                zxain*(i-1)+j)
            end do
!
!     CALCUL DE LA BASE COVARIANTE AUX POINTS D'INTERSECTION
!     ND EST LA NORMALE Ã LA SURFACE : GRAD(LSN)
!     TAU1 EST LE PROJETÃ DE GRAD(LST) SUR LA SURFACE
!     TAU2 EST LE PRODUIT VECTORIEL : ND ^ TAU1
!
!       INITIALISATION TAU1 POUR CAS 2D
            tau1(3)=0.d0
            call vecini(3, 0.d0, nd)
            call vecini(3, 0.d0, grlt)
!
            do j = 1, ndim
                do k = 1, nnop
                    nd(j) = nd(j) + ff(k)*zr(jgrlsn-1+ndim*(nfiss*(k- 1)+ifiss-1)+j)
                    grlt(j) = grlt(j) + ff(k)*zr(jgrlst-1+ndim*(nfiss* (k-1)+ifiss-1)+j)
                end do
            end do
!
            call normev(nd, norme)
            ps=ddot(ndim,grlt,1,nd,1)
            do j = 1, ndim
                tau1(j)=grlt(j)-ps*nd(j)
            end do
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
                ASSERT(norm2.gt.1.d-12)
            endif
            if (ndim .eq. 3) then
                call provec(nd, tau1, tau2)
            endif
!
            do j = 1, ndim
                zr(jout5-1+ncompb*(ifiss-1)+ndim*ndim*(i-1)+j)&
                =nd(j)
                zr(jout5-1+ncompb*(ifiss-1)+ndim*ndim*(i-1)+j+ndim)=&
                tau1(j)
                if (ndim .eq. 3) zr( jout5-1+ncompb*(ifiss-1)+ndim* ndim*(i-1)+j+2*ndim)=tau2(j )
            end do
!
            if (nfiss .gt. 1) then
!    CALCUL DES FONCTIONS HEAVISIDE AUX POINTS D'INTER
                do jfiss = 1, nfiss
                    lsn = 0
                    do k = 1, nnop
                        lsn = lsn + ff(k) * zr(jlsn-1+nfiss*(k-1)+ jfiss)
                    end do
                    if (abs(lsn) .gt. 1.d-10) then
                        pthea(nfiss*(i-1)+jfiss) = nint(sign(1.d0,lsn) )
                    endif
                end do
            endif
!
        end do
!
!     ARCHIVAGE DE CFACE ET DE HEAVFA
        do i = 1, nface
            do j = 1, nptf
                zi(jout3-1+ncompc*(ifiss-1)+nptf*(i-1)+j)=cface(i,j)
            end do
            if (nfiss .gt. 1) then
                elim = .false.
                elim2= .false.
                do jfiss = 1, nfiss
                    if (jfiss .eq. ifiss) then
!    ESCLAVE = -1, MAITRE = +1
                        hescl = -1
                        hmait = +1
                    else
                        he = 0
                        do j = 1, nptf
                            if (pthea(nfiss*(cface(i,j)-1)+jfiss) .ne. 0 .and.&
                                pthea(nfiss*(cface(i,j)-1)+ jfiss) .ne. he .and. he .ne. 0) then
                                elim = .true.
                            endif
                            if (he .eq. 0) he=pthea(nfiss*(cface(i,j)-1)+ jfiss)
!
                        end do
!
!    ESCLAVE = HE, MAITRE = HE
                        hescl = he
                        hmait = he
!    ON MODIFIE LA VALEUR DANS LE CAS DE FONCTION JONCTION
                        kfiss = jfiss
124                     continue
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
                                do j = 1, nptf
                                    if (he .eq. 0) he=pthea( nfiss*(cface(i,j)-1)+kfiss)
                                end do
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
                    zi(jout7-1+ncomph*(nfiss*(ifiss-1)+jfiss-1)+2*i-1) = hescl
                    zi(jout7-1+ncomph*(nfiss*(ifiss-1)+jfiss-1)+2*i) = hmait
                end do
                if (elim2) then
                    call utmess('A', 'XFEM_45', sk=nomte)
                    goto 998
                endif
            endif
        end do
!
!     ARCHIVAGE DE LONGCO
!
97      continue
        if (nface.gt.0) then
           ASSERT(nface.le.(ncompc*nfiss/nptf))
        endif
        zi(jout4+3*(ifiss-1)-1+2)=nface
998     continue
!
        ASSERT(ninter.le.(ncompp*nfiss/ndim))
        ASSERT(ninter.le.(ncompa*nfiss/zxain))
        ASSERT(ninter.le.(ncompb*nfiss/(ndim**2)))
        zi(jout4+3*(ifiss-1)-1+1)=nbtot
!
        zi(jout4+3*(ifiss-1)-1+3)=nptf
!
        if (nfiss .eq. 1) then
            do i = 1, 2*nfimax
                fisco(i)=0
            end do
        endif
!
!
end do
!
999 continue
!
    call jedema()
end subroutine
