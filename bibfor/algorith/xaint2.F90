subroutine xaint2(noma, modele)
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cesexi.h"
#include "asterfort/cncinv.h"
#include "asterfort/codent.h"
#include "asterfort/conare.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/xxmmvd.h"
!
    character(len=8) :: modele, noma
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (PREPARATION)
!
! ON MODIFIE DE TOPOAC.AI
!
! LORS DU TRAITEMENT DU CONTACT SUR LES BRANCHEMENTS DE FISSURES,
! SI LES FACETTES DU SUPPORT D'UN NOEUD D'UNE ARETE COUPÉE SONT
! ASSOCIÉES À DES FONCTIONS HEAVISIDE DIFFÉRENTES: IL Y A CONFLIT :
! ON DÉSACTIVE CE NOEUD :
!       +-----/
!       |    /|
!       |   / |
!       |  /  |
! +-----*-/---+           *NOEUD DÉSACTIVÉ
! |     |/    |
! |     / \   |
! |    /|  \  |
! +---/-+---\-+
!    /       \
!   FISS1     FISS2
!
! ON FAIT CETTE OPÉRATION EN DEHORS DU TE0510 CAR IL FAUT COMPARER LES
! FACETTES DU SUPPORT DU NOEUD.
!
! ----------------------------------------------------------------------
!
!
!  IN  NOMA   : NOM DE L'OBJET MAILLAGE
!  I/O MODELE   : NOM DE LA SD MODELE_XFEM
!
!
!
!
    character(len=24) :: grp(3)
    character(len=19) :: ces(5), cel(5), cnxinv, ligrel
    character(len=8) :: typma, nomfis
    character(len=2) :: ch2
    integer ::  jcesd(5), jcesl(5), jcesv(5), iad, iret
    integer ::  itypma, nncp, ibid, ier
    integer ::  jnbsp2
    integer ::  jconx2,  ndime, ndim
    integer :: nuno(2), nuno2(2), ino(2), ino2, ima, ima2
    integer :: nfis, ifis
    integer :: i, j, k, nfiss, ifiss, nfis2, ifis2, ifis3
    logical(kind=1) :: elim(2), verif
    logical :: lcond
    integer :: igrp, nbma,   nmaenr, jg
    integer :: nface, ninter, inter, zxain, ia, ifh, nfh, nmasup, jmasup
    integer :: heav, he, ar(12, 3), nbar, nno2, nngl, inte2, ninte2
    integer, pointer :: nbsp(:) => null()
    integer, pointer :: xfem_cont(:) => null()
    integer, pointer :: typmail(:) => null()
    character(len=8), pointer :: fiss(:) => null()
    integer, pointer :: tmdim(:) => null()
    integer, pointer :: vnfis(:) => null()
    integer, pointer :: connex(:) => null()
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ON VERIFIE QUE L'ON A CONTACT P1P1
    call jeveuo(modele//'.XFEM_CONT', 'L', vi=xfem_cont)
    if (xfem_cont(1) .ne. 1) goto 999
!
    zxain  =xxmmvd('ZXAIN')
!
! --- RECUPERATION DES DONNEES SUR LE MAILLAGE
!      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8BID,IBID)
!      CALL JEVEUO(MODELE//'.MAILLE','L',JMAIL)
    call jeveuo('&CATA.TM.TMDIM', 'L', vi=tmdim)
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call jeveuo(noma//'.TYPMAIL', 'L', vi=typmail)
    call jeveuo(noma//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
! --- CONNECTIVITE INVERSEE
    cnxinv = '&&XAINT2.CNCINV'
    call cncinv(noma, [ibid], 0, 'V', cnxinv)
    ligrel = modele//'.MODELE'
!
! --- RECUPERATION DES DONNEES ELEMENTAIRES XFEM
!
    call jeveuo('&&XTYELE.NBSP', 'L', vi=nbsp)
    cel(1) = modele//'.TOPOFAC.LO'
    cel(2) = modele//'.TOPOFAC.AI'
    cel(3) = modele//'.STNO'
    cel(4) = modele//'.FISSNO'
    cel(5) = modele//'.TOPOFAC.HE'
    do i = 1, 5
        call codent(i, 'G', ch2)
        ces(i) = '&&XAINT2.CES'//ch2
        call jeexin(cel(i)//'.CELD', ier)
        if (ier .eq. 0) goto 10
        call celces(cel(i), 'V', ces(i))
        call jeveuo(ces(i)//'.CESD', 'L', jcesd(i))
        call jeveuo(ces(i)//'.CESL', 'L', jcesl(i))
        call jeveuo(ces(i)//'.CESV', 'E', jcesv(i))
 10     continue
    end do
!
! --- COMPTEUR LOCAL DES FISSURES DANS LES ÉLÉMENTS
!
    call wkvect('&&XAIN2.NBSP', 'V V I', nbma, jnbsp2)
!
!     BOUCLE SUR LES FISSURES
!
    call jeveuo(modele//'.NFIS', 'L', vi=vnfis)
    nfis = vnfis(1)
    call jeveuo(modele//'.FISS', 'L', vk8=fiss)
    do ifis = 1, nfis
        nomfis = fiss(ifis)
        grp(1)=nomfis//'.MAILFISS.HEAV'
        grp(2)=nomfis//'.MAILFISS.CTIP'
        grp(3)=nomfis//'.MAILFISS.HECT'
!       BOUCLE SUR LES GROUPES
        do igrp = 1, 3
            call jeexin(grp(igrp), iret)
            if (iret .ne. 0) then
                call jeveuo(grp(igrp), 'L', jg)
                call jelira(grp(igrp), 'LONMAX', nmaenr)
!           BOUCLE SUR LES MAILLES DU GROUPE
                do i = 1, nmaenr
                    ima = zi(jg-1+i)
!             ON INCRÉMENTE LE COMPTEUR LOCAL
                    zi(jnbsp2-1+ima) = zi(jnbsp2-1+ima)+1
                end do
            endif
        end do
!       BOUCLE SUR LES GROUPES
        do igrp = 1, 3
            call jeexin(grp(igrp), iret)
            if (iret .ne. 0) then
                call jeveuo(grp(igrp), 'L', jg)
                call jelira(grp(igrp), 'LONMAX', nmaenr)
!           BOUCLE SUR LES MAILLES DU GROUPE
                do i = 1, nmaenr
                    ima = zi(jg-1+i)
                    itypma=typmail(ima)
                    ndime= tmdim(itypma)
                    if (ndime .lt. ndim) goto 220
                    ifiss = zi(jnbsp2-1+ima)
                    call cesexi('S', jcesd(1), jcesl(1), ima, 1,&
                                ifiss, 2, iad)
                    nface = zi(jcesv(1)-1+iad)
                    call cesexi('S', jcesd(1), jcesl(1), ima, 1,&
                                ifiss, 1, iad)
                    ninter = zi(jcesv(1)-1+iad)
                    nfiss = nbsp(ima)
!             ON NE TRAITE QUE LES ELEMENTS MULTI-HEAVISIDE COUPÉS
                    if (nfiss .le. 1 .or. ninter .eq. 0) goto 220
                    if (nface .eq. 0) then
!               SI PAS DE FACETTES, ON VERIFIE TOUTES LES ARETES
                        verif = .false.
                    else
!               SINON ON VERIFIE UNIQUEMENT LES ARETES NÉGATIVE
                        verif = .true.
                    endif
!
                    call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
                    call conare(typma, ar, nbar)
                    do inter = 1, ninter
!               BOUCLE SUR LES ARETES DE L'ÉLÉMENT CONTENANT LA JONCTION
                        call cesexi('S', jcesd(2), jcesl(2), ima, 1,&
                                    ifiss, (inter-1)*zxain+1, iad)
                        ia = nint(zr(jcesv(2)-1+iad))
                        if (ia .eq. 0 .or. ia .gt. 0 .and. verif) goto 230
                        ia = abs(ia)
!               RÉCUP DES NOEUDS J DE L'ARETE
                        do j = 1, 2
                            ino(j) = ar(ia,j)
                            nuno(j) = connex(zi(jconx2+ima-1)+ ino(j)-1)
                            elim(j) = .false.
                        end do
!
                        do j = 1, 2
!       RECUPERATION DU NOMBRE DE DDL HEAVISIDES ACTIFS SUR LE NOEUD J
                            nfh = 0
                            do ifis2 = 1, nfiss
                                call cesexi('S', jcesd(3), jcesl(3), ima, ino(j),&
                                            ifis2, 1, iad)
                                if (zi(jcesv(3)-1+iad) .eq. 1) nfh = nfh + 1
                            end do
                            ASSERT(nfh.gt.1)
!
!     RECUPÉRATION DE LA CONNECTIVITÉ DU NOEUD J
!
                            call jelira(jexnum(cnxinv, nuno(j)), 'LONMAX', nmasup)
                            call jeveuo(jexnum(cnxinv, nuno(j)), 'L', jmasup)
                            heav = 0
                            do k = 1, nmasup
                                ima2 = zi(jmasup-1+k)
                                itypma=typmail(ima2)
                                ndime= tmdim(itypma)
                                if (ndime .lt. ndim) goto 250
                                ifis2 = zi(jnbsp2-1+ima2)
                                nfis2 = nbsp(ima2)
                                call cesexi('S', jcesd(1), jcesl(1), ima2, 1,&
                                            ifis2, 2, iad)
!      SI PAS DE FACETTE DANS CETTE MAILLE POUR CETTE FISSURE, ON SORT
                                if (zi(jcesv(1)-1+iad) .eq. 0) goto 250
!      RECUPÉRATION DU NUMÉRO DE NOEUD INO2 CORRESPONDANT À J DANS IMA2
                                call jelira(jexnum(noma//'.CONNEX', ima2), 'LONMAX', nno2)
                                do ino2 = 1, nno2
                                    nngl=connex(zi(jconx2+ima2-1)&
                                    +ino2-1)
                                    if (nngl .eq. nuno(j)) goto 270
                                end do
270                             continue
!
!     CALCUL DE LA VALEUR DE LA FONCTION HEAVISIDE EN TRINAIRE :
!     HE =(-1,0 OU 1) EN IFH DONNE (0,1 OU 2) POUR LA IFH-1 TRICIMALE
!
                                he = 0
                                do ifh = 1, nfh
!     NUMÉRO DE FISSURE ASSOCIÉ À NUNO(J) ET IFH DANS IMA2
                                    call cesexi('S', jcesd(4), jcesl(4), ima2, ino2,&
                                                ifh, 1, iad)
                                    ifis3 = zi(jcesv(4)-1+iad)
!     RECUPÉRATION DE LA FONCTION HEAVISIDE (ESCLAVE ET MAITRE)
                                    call cesexi('S', jcesd(5), jcesl(5), ima2, 1,&
                                                nfis2*(ifis2-1)+ifis3, 1, iad)
                                    he = he + (1+zi(jcesv(5)-1+iad))* 3**(ifh-1 )
!
                                    call cesexi('S', jcesd(5), jcesl(5), ima2, 1,&
                                                nfis2*(ifis2-1)+ifis3, 2, iad)
                                    he = he + ( 1+zi(jcesv(5)-1+iad))* 3**(nfh+ifh-1)
                                end do
                                if (heav .eq. 0) heav = he
!     SI LA FONC HEAV EST DIFF D'UN ELEM À L'AUTRE, ON DÉSATCTIVE LE NO
                                if (heav .ne. he) elim(j) = .true.
250                             continue
                            end do
!
                        end do
!     ELIM = TT <=> L'ARETE DOIT ÊTRE ENTIÈREMENT ÉLIMINÉE
!     SINON IL FAUDRA RECONSIDÉRER PROPREMENT L'ELIM COMPLÈTE DE L'ARETE
                        lcond=verif.eqv.(elim(1).and.elim(2))
                        ASSERT(lcond)
                        do j = 1, 2
                            if (.not.elim(j)) goto 300
!
!     RECUPÉRATION DE LA CONNECTIVITÉ DU NOEUD J
!
                            call jelira(jexnum(cnxinv, nuno(j)), 'LONMAX', nmasup)
                            call jeveuo(jexnum(cnxinv, nuno(j)), 'L', jmasup)
!
!                 BOUCLE SUR LES ELEM CONEXES
                            do k = 1, nmasup
                                ima2 = zi(jmasup-1+k)
                                itypma=typmail(ima2)
                                ndime= tmdim(itypma)
                                if (ndime .lt. ndim) goto 350
                                call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
                                call conare(typma, ar, nbar)
                                ifis2 = zi(jnbsp2-1+ima2)
                                call cesexi('S', jcesd(1), jcesl(1), ima2, 1,&
                                            ifis2, 1, iad)
                                ninte2 = zi(jcesv(1)-1+iad)
!
!                   BOUCLE SUR LES ARETES AINTER
                                do inte2 = 1, ninte2
                                    call cesexi('S', jcesd(2), jcesl(2), ima2, 1,&
                                                ifis2, (inte2-1)*zxain+1, iad)
                                    ia = abs(nint(zr(jcesv(2)-1+iad)))
!
                                    if (ia .eq. 0) goto 360
                                    nuno2(1)=connex(zi(jconx2+&
                                    ima2-1)+ar(ia,1)-1)
                                    nuno2(2)=connex(zi(jconx2+&
                                    ima2-1)+ar(ia,2)-1)
!         SI LE NOEUD J N'APPARTIENT PAS À L'ARETE, ON SORT
                                    if (nuno(j) .ne. nuno2(1) .and. nuno( j) .ne. nuno2(2)) &
                                    goto 360
!         MISE À ZÉRO DE L'ARETE
                                    zr(jcesv(2)-1+iad) = 0
                                    if (nuno(3-j) .ne. nuno2(1) .and. nuno( 3-j) .ne.&
                                        nuno2(2) .or. (.not.verif)) then
!         SI L'ARETE N'EST PAS TT, ON LA REPORTE SUR SON NOEUD ACTIF
                                        call cesexi('S', jcesd(2), jcesl(2), ima2, 1,&
                                                    ifis2, (inte2-1)*zxain+2, iad)
                                        if (nuno2(1) .eq. nuno(j)) then
!                         ON REPORTE L'ARETE SUR LE NOEUD 2
                                            zr(jcesv(2)-1+iad) = ar(ia,2)
                                        else
!                         ON REPORTE L'ARETE SUR LE NOEUD 1
                                            zr(jcesv(2)-1+iad) = ar(ia,1)
                                        endif
                                    endif
360                                 continue
                                end do
350                             continue
                            end do
300                         continue
                        end do
230                     continue
                    end do
!
220                 continue
                end do
            endif
        end do
    end do
!
! --- CONVERSION CHAM_ELEM_S -> CHAM_ELEM POUR MODELE.TOPOFAC.AI
!
    call cescel(ces(2), ligrel, 'TOPOFA', 'PAINTER', 'OUI',&
                nncp, 'G', cel(2), 'F', ibid)
!
! --- MENAGE
!
    call jedetr(cnxinv)
    call jedetr('&&XAIN2.NBSP')
    do i = 1, 5
        call jeexin(ces(i)//'.CESD', ier)
        if (ier .eq. 0) goto 130
        call detrsd('CHAM_ELEM_S', ces(i))
130     continue
    end do
!
999 continue
!
    call jedema()
end subroutine
