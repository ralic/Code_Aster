subroutine xtyele(model, trav, nfiss, fiss, contac,&
                  ndim, linter)
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
! person_in_charge: jacques.pellet at edf.fr
! aslint: disable=W1306
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/cncinv.h"
#include "asterfort/cnocns.h"
#include "asterfort/codent.h"
#include "asterfort/conare.h"
#include "asterfort/confac.h"
#include "asterfort/dismoi.h"
#include "asterfort/intfac.h"
#include "asterfort/ismali.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/padist.h"
#include "asterfort/panbno.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xelfis_lists.h"
#include "asterfort/xtyhea.h"
#include "blas/ddot.h"
!
    character(len=24) :: trav
    integer :: nfiss
    character(len=8) :: fiss(nfiss), model
    integer :: contac, ndim, iret
    aster_logical :: linter
!
! ----------------------------------------------------------------------
!
! --- ROUTINE XFEM
!
! --- REMPLISSAGE DE TAB, QUI DEFINIE LE TYPE D'ELEMENT XFEM POUR
! --- LA CREATION DU MODELE
!
! ----------------------------------------------------------------------
!
!
    real(kind=8) :: minlsn, minlst, maxlsn, lsn
    real(kind=8) :: lsna, lsta, lsnb, lstb, lstc
    real(kind=8) :: a(ndim), b(ndim), ab(ndim), c(ndim), ac(ndim)
    real(kind=8) :: cmin(ndim), longar, m(ndim), rbid(ndim)
    integer :: nmaenr, kk, jgrp(4*nfiss), nbma, indptf(3)
    integer :: jmasup, jconx1, jconx2
    integer :: nbcoup, nbcou2, ibid, ifiss, itypma, jtab, jnbpt, jnbpt2
    integer :: nmasup, ndime, nbar, nbheav, jstnl(nfiss), jstnv(nfiss)
    integer :: ino, ino2, nngl, nnot(3), nno, nno2, ima, ima2, ifis
    integer :: i, j, k, l, icont(nfiss), jco2, jcont(nfiss), ncont
    integer :: ar(12, 3), ia, nunoa, nunob, stna, stnb, nma, imae
    integer :: fa(6, 8), ibid3(12, 3), nbf, ifq, codret, ilsn, ilst, igeom
    character(len=2) :: ch2
    character(len=8) :: typma, k8bid, noma, nomail
    character(len=16) :: exithm
    character(len=19) :: clsn, clst, cnxinv, cstn(nfiss), maicon(nfiss)
    character(len=24) :: grp(4*nfiss)
    character(len=24) :: elfis_heav(nfiss), elfis_ctip(nfiss), elfis_hect(nfiss)
    aster_logical :: lcont
    integer, pointer :: typmail(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    real(kind=8), pointer :: vlsn(:) => null()
    real(kind=8), pointer :: lst(:) => null()
    integer, pointer :: tmdim(:) => null()
!
! ----------------------------------------------------------------------
!



    call jemarq()
!
! --- INITIALISATION
!
    linter = .false.
    do i = 1, 3
        indptf(i)=0
    end do
    clsn = '&&XTYELE.LSN'
    clst = '&&XTYELE.LST'
    do ifiss = 1, nfiss
        call codent(ifiss, 'G', ch2)
        cstn(ifiss)='&&XTYELE.STN'//ch2
        maicon(ifiss)='&&XTYELE.CONT'//ch2
        elfis_heav(ifiss)='&&XTYELE.ELEMFISS.HEAV'//ch2
        elfis_ctip(ifiss)='&&XTYELE.ELEMFISS.CTIP'//ch2
        elfis_hect(ifiss)='&&XTYELE.ELEMFISS.HECT'//ch2
    end do
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=noma)
    call jeveuo(noma(1:8)//'.COORDO    .VALE', 'L', vr=vale)
    call jeveuo('&CATA.TM.TMDIM', 'L', vi=tmdim)
    call jeveuo(noma(1:8)//'.TYPMAIL', 'L', vi=typmail)
!
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call jeveuo(noma(1:8)//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma(1:8)//'.CONNEX', 'LONCUM'), 'L', jconx2)
    cnxinv = '&&XTYELE.CNCINV'
    call cncinv(noma, [ibid], 0, 'V', cnxinv)
!
!     RECUPERATION DE L'ADRESSE DU TABLEAU DE TRAVAIL
    call jeveuo(trav, 'E', jtab)
! --- CREATION DE L'OBJET CONTENANT LE NOMBRE DE FISSURE VUE PAR MAILLE,
! --- CORRESPOND AU NOMBRE DE SOUS POINTS POUR LA CREATION DES SD XCONNO
    call wkvect('&&XTYELE.NBSP', 'V V I', nbma, jnbpt)
! --- CREATION DE L'OBJET CONTENANT LE NOMBRE DE FONCTIONS HEAVISIDES
! --- PAR MAILLES POUR LES MAILLES QUI VOIENT PLUS DE 2 FISSURES
! --- CORRESPOND AU NOMBRE DE SOUS POINTS POUR LA SD FISSNO
    call wkvect('&&XTYELE.NBSP2', 'V V I', nbma, jnbpt2)
!
! --- BOUCLE SUR NOMBRE OCCURRENCES FISSURES
!
    ncont = 0
    nmaenr = 0
    do ifiss = 1, nfiss
        call cnocns(fiss(ifiss)//'.STNO', 'V', cstn(ifiss))
        call jeveuo(cstn(ifiss)//'.CNSL', 'L', jstnl(ifiss))
        call jeveuo(cstn(ifiss)//'.CNSV', 'L', jstnv(ifiss))
        call xelfis_lists(fiss(ifiss), model, elfis_heav(ifiss),&
                              elfis_ctip(ifiss), elfis_hect(ifiss))
        grp(4*(ifiss-1)+1) = elfis_heav(ifiss)
        grp(4*(ifiss-1)+2) = elfis_ctip(ifiss)
        grp(4*(ifiss-1)+3) = elfis_hect(ifiss)
        grp(4*(ifiss-1)+4) = fiss(ifiss)//'.MAILFISS.CONT'
        do l = 1, 3
            call jeexin(grp(4*(ifiss-1)+l), iret)
            if (iret .ne. 0) then
                call jelira(grp(4*(ifiss-1)+l), 'LONMAX', nmaenr, k8bid)
                ncont = ncont + nmaenr
                call jeveuo(grp(4*(ifiss-1)+l), 'L', jgrp(4*(ifiss-1)+l))
            endif
        end do
        icont(ifiss)=0
    end do
!
    if(ncont.gt.0) then
        do ifiss = 1, nfiss
            call wkvect(maicon(ifiss), 'V V I', ncont, jcont(ifiss))
        end do
    endif
!
    do ifiss = 1, nfiss
        call cnocns(fiss(ifiss)//'.LNNO', 'V', clsn)
        call cnocns(fiss(ifiss)//'.LTNO', 'V', clst)
        call jeveuo(clsn//'.CNSV', 'L', vr=vlsn)
        call jeveuo(clst//'.CNSV', 'L', vr=lst)
!
! --- BOUCLE SUR LES GRP
!
        do kk = 1, 3
            call jeexin(grp(4*(ifiss-1)+kk), iret)
            if (iret .ne. 0) then
                call jelira(grp(4*(ifiss-1)+kk), 'LONMAX', nmaenr, k8bid)
!
! --- BOUCLE SUR LES MAILLES DU GROUPE
!
                do i = 1, nmaenr
                    ima = zi(jgrp(4*(ifiss-1)+kk)-1+i)
!
                    zi(jnbpt-1+ima) = zi(jnbpt-1+ima)+1
                    itypma=typmail(ima)
!
                    if (zi(jtab-1+5*(ima-1)+4) .eq. 0) then
! --- BLINDAGE DANS LE CAS DU MULTI-HEAVISIDE
                        call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
                        call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
                        call dismoi('EXI_THM', model, 'MODELE', repk=exithm)
                        if (.not.ismali(typma) .and. exithm .eq. 'NON') then
                            call utmess('F', 'XFEM_41', sk=nomail)
                        endif
                    endif
!
! --- ON RECUPERE LE NB DE NOEUDS SOMMETS DE LA MAILLE
                    call panbno(itypma, nnot)
                    nno = nnot(1)
!
! --- ON DETERMINE S'IL S'AGIT D'UNE MAILLE DE CONTACT OU PAS
                    lcont = .false.
!
! --- SI LE CONTACT EST DECLARÉ DANS LE MODELE
!
                    if (contac .ge. 1) then
! --- PAS DE CONTACT POUR LES MAILLE DE BORD
                        ndime= tmdim(itypma)
                        if (ndime .ne. ndim) goto 110
!
                        maxlsn=-1*r8maem()
                        minlsn=r8maem()
! --- BOUCLE SUR LES ARETES DE LA MAILLE
                        call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
                        call conare(typma, ar, nbar)
                        do ia = 1, nbar
                            nunoa=zi(jconx1-1+zi(jconx2+ima-1)+ar(ia,&
                            1)-1)
                            nunob=zi(jconx1-1+zi(jconx2+ima-1)+ar(ia,&
                            2)-1)
                            lsna=vlsn(nunoa)
                            lsnb=vlsn(nunob)
                            stna=zi(jstnv(ifiss)-1+nunoa)
                            stnb=zi(jstnv(ifiss)-1+nunob)
                            if (lsna .lt. minlsn) minlsn=lsna
                            if (lsnb .lt. minlsn) minlsn=lsnb
                            if (lsna .gt. maxlsn) maxlsn=lsna
                            if (lsnb .gt. maxlsn) maxlsn=lsnb
! --- ARETE OU NOEUD COUPÉ AVEC STATUT NUL -> MAILLE MULTI-H NON COUPÉE
                            if (lsna*lsnb .le. 0.and.nfiss.gt.1) then
                                if (lsna*lsnb .lt. 0 .and. ( stna.eq.0.or.stnb.eq.0) .or. lsna&
                                    .eq. 0 .and. stna .eq. 0 .or. lsnb .eq. 0 .and. stnb .eq. 0) &
                                goto 110
                            endif
                        end do
! --- BOUCLE SUR LES NOEUDS DE LA MAILLE
!                DO 100 INO=1,NNO
!                  NNGL=ZI(JCONX1-1+ZI(JCONX2+IMA-1)+INO-1)
!                  LSN = ZR(JLSN-1+NNGL)
!                  IF (LSN.LT.MINLSN) MINLSN=LSN
!                  IF (LSN.GT.MAXLSN) MAXLSN=LSN
! 100            CONTINUE
! --- TRAITEMENT DES DIFFERENTS CAS
                        if (minlsn*maxlsn .lt. 0) then
!--- LA MAILLE EST COUPÉE, ON ACTIVE LE CONTACT
                            lcont=.true.
                        else if (maxlsn.eq.0) then
! --- SI LA MAILLE EST ENTIEREMENT DU COTÉ ESCLAVE, MAIS TOUCHE LA LSN
! --- LE CONTACT EST ACTIVÉ SI TOUT LES NOEUDS D'UNE FACE SONT COUPÉS
                            nbcoup = 0
                            do ino = 1, nno
                                nngl=zi(jconx1-1+zi(jconx2+ima-1)+ino-&
                                1)
                                lsn = vlsn(nngl)
                                if (lsn .eq. 0) then
! --- LE NOEUD EST COUPÉ SI LE MAX DE LSN DE SA CONNECTIVITÉ
! --- EST STRICTEMENT POSITIF
                                    maxlsn=-1*r8maem()
                                    call jelira(jexnum(cnxinv, nngl), 'LONMAX', nmasup)
                                    call jeveuo(jexnum(cnxinv, nngl), 'L', jmasup)
                                    do j = 1, nmasup
                                        ima2 = zi(jmasup-1+j)
                                        call jelira(jexnum( noma// '.CONNEX', ima2), 'LONMAX',&
                                                    nno2, k8bid)
                                        do ino2 = 1, nno2
                                            nngl=zi(jconx1-1+zi(jconx2+&
                                        ima2-1)+ino2-1)
                                            lsn = vlsn(nngl)
                                            if (lsn .gt. maxlsn) maxlsn=lsn
                                        end do
                                    end do
!
                                    if (maxlsn .gt. 0) nbcoup=nbcoup+1
                                endif
                            end do
! --- ON REGARDE SI LE NOMBRE DE NOEUDS COUPÉES NBCOUP DEFINIT UNE FACE
                            if (ndim .eq. 2) then
                                if (nbcoup .eq. 2) lcont=.true.
                                if (nbcoup .eq. 3 .and. .not.ismali(typma)) lcont=.true.
                            else
                                call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
                                if (typma(1:5) .eq. 'TETRA') then
                                    if (nbcoup .eq. 3) lcont=.true.
                                else if (typma(1:4).eq.'PYRA') then
                                    nngl=zi(jconx1-1+zi(jconx2+ima-1)+&
                                    5-1)
                                    lsn = vlsn(nngl)
                                    if (lsn .eq. 0 .and. nbcoup .eq. 3 .or. nbcoup .eq. 4) &
                                    lcont=.true.
                                else if (typma(1:5).eq.'PENTA') then
                                    nbcou2=0
                                    do ino = 1, 3
                                        nngl=zi(jconx1-1+zi(jconx2+&
                                        ima-1)+ino-1)
                                        lsn = vlsn(nngl)
                                        if (lsn .eq. 0) then
                                            nbcou2 = nbcou2+1
                                        endif
                                    end do
                                    if ((nbcou2.eq.3.or.nbcou2.eq.0) .and. nbcoup .eq. 3 .or.&
                                        nbcoup .eq. 4) lcont=.true.
                                else if (typma(1:4).eq.'HEXA') then
                                    if (nbcoup .eq. 4) lcont=.true.
                                endif
                            endif
                        endif
                    endif
!
! --- CRITERE SUPLEMENTAIRE POUR LE GRP CTIP, ON DESACTIVE LE CONTACT
! --- SI LE MIN DE LST AUX PTS D'INTERSECTIONS DE L'ÉLÉMENT EST POSITIF
!
                    if (kk .eq. 2 .and. lcont) then
                        minlst=r8maem()
                        call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
                        call conare(typma, ar, nbar)
                        do ia = 1, nbar
                            nunoa=zi(jconx1-1+zi(jconx2+ima-1)+ar(ia,&
                            1)-1)
                            nunob=zi(jconx1-1+zi(jconx2+ima-1)+ar(ia,&
                            2)-1)
                            lsna=vlsn(nunoa)
                            lsnb=vlsn(nunob)
                            lsta=lst(nunoa)
                            lstb=lst(nunob)
                            if (lsna .eq. 0.d0 .or. lsnb .eq. 0.d0) then
                                if (lsna .eq. 0.d0 .and. lsta .lt. minlst) minlst=lsta
                                if (lsnb .eq. 0.d0 .and. lstb .lt. minlst) minlst=lstb
                            else if ((lsna*lsnb).lt.0.d0) then
                                do k = 1, ndim
                                    a(k)=vale(3*(nunoa-1)+k)
                                    b(k)=vale(3*(nunob-1)+k)
                                    ab(k)=b(k)-a(k)
                                    c(k)=a(k)-lsna/(lsnb-lsna)*ab(k)
                                    ac(k)=c(k)-a(k)
                                end do
                                ASSERT(ddot(ndim, ab, 1, ab, 1) .gt. r8prem())
                                lstc = lsta + (lstb-lsta) * ddot(ndim, ab,1,ac,1) / ddot(ndim,ab,&
                                       &1,ab,1)
                                if (lstc .lt. minlst) then
                                    minlst=lstc
                                    do k = 1, ndim
                                        cmin(k)=c(k)
                                    end do
                                endif
                            endif
                        end do
                        if (minlst .ge. 0) lcont =.false.
                        if (lcont) then
! --- ON VERIFIE LA TOLERANCE AVEC LES PT DE FOND DE FISSURE
                            call confac(typma, ibid3, ibid, fa, nbf)
!     ON SE RECREE UN ENVIRONNEMENT COMME DANS UN TE
!                 POUR LSN, LST ET IGEOM
!                 AFIN DE POUVOIR APPELER INTFAC
                            call wkvect('&&XTYELE.LSN', 'V V R', nno, ilsn)
                            call wkvect('&&XTYELE.LST', 'V V R', nno, ilst)
                            call wkvect('&&XTYELE.IGEOM', 'V V R', nno* ndim, igeom)
                            do ino = 1, nno
                                nngl=zi(jconx1-1+zi(jconx2+ima-1)+ino-&
                                1)
                                zr(ilsn-1+ino) = vlsn(nngl)
                                zr(ilst-1+ino) = lst(nngl)
                                do j = 1, ndim
                                    zr(igeom-1+ndim*(ino-1)+j) =&
                                    vale(3*(nngl-1)+j)
                                end do
                            end do
! --- BOUCLE SUR LES FACES
                            do ifq = 1, nbf
                                call intfac(noma, ima, ifq, fa, nno,&
                                            zr( ilst), zr(ilsn), ndim, 'NON', ibid,&
                                            ibid, igeom, m, indptf, rbid,&
                                            rbid, codret)
!
                                if (codret .eq. 1) then
!     LONGUEUR CARACTERISTIQUE
                                    do j = 1, ndim
                                        a(j) = zr(igeom-1+ndim*(fa( ifq,1)-1)+j )
                                        b(j) = zr(igeom-1+ndim*(fa( ifq,2)-1)+j )
                                        c(j) = zr(igeom-1+ndim*(fa( ifq,3)-1)+j )
                                    end do
                                    longar=(padist(ndim,a,b)+padist(&
                                    ndim,a,c))/2.d0
                                    if (padist(ndim,m,cmin) .lt. ( longar*1.d-6)) lcont =.false.
                                endif
                            end do
                            call jedetr('&&XTYELE.LSN')
                            call jedetr('&&XTYELE.LST')
                            call jedetr('&&XTYELE.IGEOM')
                        endif
                    endif
110                 continue
!
! --- POUR CHAQUE MAILLE DE CE GRP, REMPLIT LA COLONNE KK
! --- -1 -> X-FEM SANS CONTACT
! ---  1 -> X-FEM AVEC CONTACT
! ---  0 -> FEM SI LA COLONE 4 EST À 1,
!           NON AFFECTÉ SI LA COLONE 4 EST À 0
!
! SI MAILLE PAS ENCORE VUE
!
                    if (lcont) then
                        icont(ifiss) = icont(ifiss)+1
                        zi(jcont(ifiss)-1+icont(ifiss)) = ima
                    endif
!
! SI MAILLE POUR LA PREMIERE FOIS EN CONTACT
! MAIS DEJA VUE AILLEURS
! ON ENRICHIT LES GROUPES DES FISSURES POUR LESQUELS
! C EST UNE HEAVISIDE
!
                    if (lcont .and. zi(jtab-1+5*(ima-1)+kk) .le. 0 .and.&
                        zi(jtab-1+5*(ima-1)+4) .eq. 0) then
                        if (kk .ne. 1) then
                            call utmess('F', 'XFEM_44', sk=nomail)
                        endif
                        do ifis = 1, ifiss-1
                            call jeexin(grp(4*(ifis-1)+1), iret)
                            if (iret .ne. 0) then
                                call jelira(grp(4*(ifis-1)+1), 'LONMAX', nma, k8bid)
                                do j = 1, nma
                                    imae = zi(jgrp(4*(ifis-1)+1)-1+j)
                                    if (imae .eq. ima) then
                                        icont(ifis) = icont(ifis)+1
                                        zi(jcont(ifis)-1+icont(ifis))&
                                        = ima
                                        goto 188
                                    endif
                                end do
                            endif
188                         continue
                        end do
                    endif
!
! SI MAILLE DEJA EN CONTACT POUR UNE AUTRE FISS
                    if (.not.lcont .and. zi(jtab-1+5*(ima-1)+kk) .gt. 0) then
                        if (kk .ne. 1) then
                            call utmess('F', 'XFEM_44', sk=nomail)
                        endif
                        ASSERT(zi(jtab-1+5*(ima-1)+4).eq.0)
                        icont(ifiss) = icont(ifiss)+1
                        zi(jcont(ifiss)-1+icont(ifiss)) = ima
                    endif
                    if (zi(jtab-1+5*(ima-1)+4) .eq. 1) then
                        if (lcont) then
                            if(contac.eq.2) then
                                zi(jtab-1+5*(ima-1)+kk) = 2
                            else
                                zi(jtab-1+5*(ima-1)+kk) = 1
                            endif
                        else
                            zi(jtab-1+5*(ima-1)+kk) = -1
                        endif
                        zi(jtab-1+5*(ima-1)+4) = 0
                    else if (zi(jtab-1+5*(ima-1)+4).eq.0) then
! --- SI LA MAILLE EST VUE UNE DEUXIEME FOIS (MULTIFISSURATION)
!
                        call dismoi('EXI_THM', model, 'MODELE', repk=exithm)
                        if (contac .gt. 1 .and. exithm.eq.'NON') then
! --- SI CONTACT AUTRE QUE P1P1 POUR UN MODELE NON HM-XFEM
                            call utmess('F', 'XFEM_43', sk=nomail)
                        endif
                        if (kk .gt. 1 .or. abs(zi(jtab-1+5*(ima-1)+2)) .eq. 1 .or.&
                            abs(zi(jtab-1+5*(ima-1)+3)) .eq. 1) then
! --- SI UNE DES MAILLES CONTIENT DU CRACK-TIP
                            call utmess('F', 'XFEM_44', sk=nomail)
                        endif
!
! --- CALCUL DU NOMBRE DE FONCTIONS HEAVISIDE
                        call xtyhea(nfiss, ifiss, ima, nno, jconx1,&
                                    jconx2, jstnl, jstnv, nbheav)
                        if (nbheav .gt. 4) then
                            call utmess('F', 'XFEM_40', sk=nomail)
                        endif
                        zi(jnbpt2-1+ima) = nbheav
                        if (zi(jtab-1+5*(ima-1)+1) .gt. 0 .or. lcont) then
! --- SI AU MOINS UNE DES 2 FISSURES A DU CONTACT
! --- ALORS CONTACT POUR TOUTES LES FISSURES VUES PAR L ELEMENT
                            if (contac.eq.2) then
                               zi(jtab-1+5*(ima-1)+kk) = nbheav+4
                            else
                               zi(jtab-1+5*(ima-1)+kk) = nbheav
                            endif
                        else
                            zi(jtab-1+5*(ima-1)+kk) = -1*nbheav
                        endif
                        linter = .true.
                    else
                        ASSERT(.false.)
                    endif
                end do
            endif
        end do
        call jedetr(clsn)
        call jedetr(clst)
!
    end do
!
    do ifiss = 1, nfiss
        if (icont(ifiss) .gt. 0) then
            call wkvect(grp(4*(ifiss-1)+4), 'G V I', icont(ifiss), jco2)
            do l = 1, icont(ifiss)
                zi(jco2-1+l)=zi(jcont(ifiss)-1+l)
            end do
        endif
        if(ncont.gt.0) call jedetr(maicon(ifiss))
        call jedetr(cstn(ifiss))
        do kk = 1, 3
            call jeexin(grp(4*(ifiss-1)+kk), iret)
            if (iret .ne. 0) call jedetr(grp(4*(ifiss-1)+kk)) 
        enddo
    end do
!
    call jedetr(cnxinv)
!
    call jedema()
end subroutine
