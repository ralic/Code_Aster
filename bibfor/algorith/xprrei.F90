subroutine xprrei(noma, fiss, fispre, noesom, noresi,&
                  cnsln, cnslt, cnsgls, deltat, lcmin,&
                  levset, isozro, cnxinv, nodtor, eletor,&
                  liggrd)
! aslint: disable=W1501
    implicit none
#include "jeveux.h"
!
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescns.h"
#include "asterfort/cesexi.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnscre.h"
#include "asterfort/codent.h"
#include "asterfort/codree.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/xprls0.h"
    real(kind=8) :: deltat, lcmin
    character(len=2) :: levset
    character(len=8) :: noma, fiss, fispre
    character(len=19) :: cnsln, cnslt, cnsgls, noesom, noresi, isozro, cnxinv
    character(len=19) :: nodtor, eletor, liggrd
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
! person_in_charge: patrick.massin at edf.fr
!     ------------------------------------------------------------------
!
!       XPRREI   : X-FEM PROPAGATION : REINITIALISATION DE LEVEL SET
!       ------     -     --            ---
!    DANS LE CADRE DE LA PROPAGATION DE FISSURE XFEM,
!     REINITIALISATION D'UNE LEVEL SET APRES PROPAGATION
!
!    ENTREE
!        NOMA    : NOM DU CONCEPT MAILLAGE
!        FISS    : NOM DU CONCEPT FISSURE XFEM DE LA FISSURE PROPAGEE
!        FISPRE  : NOM DU CONCEPT FISSURE XFEM DE LA FISSURE ACTUELLE
!        NOESOM  : INDICATEUR LOGIQUE DE NOEUD SOMMET
!        NORESI  : VECTEUR LOGIQUE INDIQUANT SI LE RESIDU EST A ESTIMER
!                   SUR LE NOEUD
!        CNSLN   : CHAM_NO_S LEVEL SET NORMALE
!        CNSLT   : CHAM_NO_S LEVEL SET TANGENTE
!        CNSGLS  : CHAM_NO_S GRADIENT DE LEVEL SET A REINITIALISER
!        DELTAT  : PAS DE TEMPS
!        LCMIN   : LONGUEUR CARACTERISTIQUE MINIMALE DES ELEMENTS
!        LEVSET  :   ='LN' SI ON REINITIALISE LN
!                    ='LT' SI ON REINITIALISE LT
!        CNXINV  : MATRICE DE CONNECTIVITE INVERSEE
!        NODTOR  : LISTE DES NOEUDS DEFINISSANT LE DOMAINE DE CALCUL
!        ELETOR  : LISTE DES ELEMENTS DEFINISSANT LE DOMAINE DE CALCUL
!        LIGGRD  : LIGREL DU DOMAINE DE CALCUL (VOIR XPRTOR.F)
!
!    SORTIE
!        CNSLS   : CHAM_NO_S LEVEL SET REINITIALISEE
!        CNSGLS  : CHAM_NO_S GRADIENT DE LEVEL SET REINITIALISEE
!        ISOZRO  :   VECTEUR LOGIQUE IDIQUANT SI LA "VRAIE" LEVEL SET
!                    (DISTANCE SIGNEE) A ETE CALCULEE
!
!     ------------------------------------------------------------------
!
!
    integer :: ifm, niv, nbnom, iret, jconx1, jconx2, jmai, ibid, ndim, jlsno
    integer :: jvi, jvil, jwi, jwil, jptf, jptfl, jgdf, jgdfl, jfel, jfell
    integer :: jfeld, jmeast, jmestl, jmestd, itemp, nbnoma, itypma, iadf
    integer :: iadmet, iaddfi, ino, ima, iadalp, nuno, nbma, jdelfi, jdefil
    integer :: jdefid, jalpha, jalphl, j, i, jalphd, jglsno, jzero, jnosom
    integer :: jresdu, nmanoi, jmanoi, imai, numai, jcoor, jtmdim, ndime, numin
    integer :: ier
    real(kind=8) :: signls, sigmls, sdiff, lsprec, siglst, sdifft, lsnouv, ji(3)
    real(kind=8) :: dist, dismin
    character(len=3) :: iterk3
    character(len=8) :: k8b, lpain(4), lpaout(2), typma
    character(len=10) :: resk10, retk10
    character(len=19) :: mai, cnols, cnogls, celgls, chams, celdfi, cesdfi
    character(len=19) :: celalf, cesalf, cnsvi, cnswi, cnsptf, cnsgdf, cesptf
    character(len=19) :: celgdf, celptf, cnoptf, cnogdf, k19bid
    character(len=24) :: lchin(4), lchout(2)
!
    real(kind=8) :: resiln, resilt
    parameter      (resiln = 5.d-3)
    parameter      (resilt = 5.d-3)
    integer :: itermx
    parameter      (itermx=500)
!      PARAMETER      (ITERMX=1)
    real(kind=8) :: residu(itermx), resit(itermx)
!
!     DOMAIN LOCALIZATION
    integer :: jnodto, nbno, jelcal, node
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
    resk10=' '
    retk10=' '
!
    write(ifm,*) '   UTILISATION DE LA METHODE SIMPLEXE'
!
!  RECUPERATION DES CARACTERISTIQUES DU MAILLAGE
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnom,&
                k8b, iret)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    mai = noma//'.TYPMAIL'
    call jeveuo(mai, 'L', jmai)
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                k8b, iret)
    call jeveuo('&CATA.TM.TMDIM', 'L', jtmdim)
!
!     RETRIEVE THE NUMBER OF THE NODES THAT MUST TO BE USED IN THE
!     CALCULUS (SAME ORDER THAN THE ONE USED IN THE CONNECTION TABLE)
    call jeveuo(nodtor, 'L', jnodto)
!
!     RETRIEVE THE TOTAL NUMBER OF THE NODES THAT MUST BE ELABORATED
    call jelira(nodtor, 'LONMAX', nbno, k8b)
!
!     RETRIEVE THE LIST OF THE ELEMENTS DEFINING THE TORE
    call jeveuo(eletor, 'L', jelcal)
!
!     RETRIEVE THE NUMBER OF ELEMENTS DEFINING THE TORE
    call jelira(eletor, 'LONMAX', nbma, k8b)
!
!   RECUPERATION DE L'ADRESSE DES VALEURS DE LS ET DU GRADIENT DE LS
    if (levset .eq. 'LN') call jeveuo(cnsln//'.CNSV', 'E', jlsno)
    if (levset .eq. 'LT') call jeveuo(cnslt//'.CNSV', 'E', jlsno)
    call jeveuo(cnsgls//'.CNSV', 'E', jglsno)
!
!  RECUPERATION DE L'ADRESSE DE L'INFORMATION 'NOEUD SOMMET'
    call jeveuo(noesom, 'L', jnosom)
!
!  RECUPERATION DE L'ADRESSE DE L'INFORMATION 'RESIDU A CALCULER'
    call jeveuo(noresi, 'L', jresdu)
!
!---------------------------------
!   CREATION DES OBJETS VOLATILES
!---------------------------------
!   LEVEL SET
    cnols = '&&XPRREI.CNOLS'
!   GRADIENT DE LA LEVEL SET
    cnogls = '&&XPRREI.CNOGLS'
    celgls = '&&XPRREI.CELGLS'
    chams = '&&XPRREI.CHAMS'
!   V AU NOEUDS
    cnsvi = '&&XPRREI.CNSVI'
    call cnscre(noma, 'NEUT_R', 1, 'X1', 'V',&
                cnsvi)
    call jeveuo(cnsvi//'.CNSV', 'E', jvi)
    call jeveuo(cnsvi//'.CNSL', 'E', jvil)
!   W AU NOEUDS
    cnswi = '&&XPRREI.CNSWI'
    call cnscre(noma, 'NEUT_R', 1, 'X1', 'V',&
                cnswi)
    call jeveuo(cnswi//'.CNSV', 'E', jwi)
    call jeveuo(cnswi//'.CNSL', 'E', jwil)
!
!   DELTA_PHI
    celdfi = '&&XPRREI.CELDFI'
    cesdfi = '&&XPRREI.CESDFI'
!   ALPHA
    celalf = '&&XPRREI.CELALF'
    cesalf = '&&XPRREI.CESALF'
!
!----------------------------------------------------------------------
!   CALCUL DES VRAIES DISTANCES SIGNEES SUR LES NOEUDS PROCHES DE LS=0
!----------------------------------------------------------------------
!  VECTEUR IDIQUANT SI LS AU NOEUD EST CALCULEE
    call wkvect(isozro, 'V V L', nbnom, jzero)
!
    k19bid=' '
    call xprls0(fispre, noma, noesom, lcmin, cnsln,&
                cnslt, isozro, levset, nodtor, eletor,&
                k19bid, k19bid)
!
!--------------------------------------
!   CALCUL DE PETIT F SUR LES ELEMENTS
!--------------------------------------
    cnsptf = '&&XPRREI.CNSPTF'
    call cnscre(noma, 'NEUT_R', 1, 'X1', 'V',&
                cnsptf)
    call jeveuo(cnsptf//'.CNSV', 'E', jptf)
    call jeveuo(cnsptf//'.CNSL', 'E', jptfl)
    do 10 i = 1, nbno
!        RETREIVE THE NODE NUMBER
        node = zi(jnodto-1+i)
        signls=0.d0
        if (abs(zr(jlsno-1+node)) .gt. r8prem()) signls = zr(jlsno-1+ node) / abs(zr(jlsno-1+node&
                                                          &))
        zl(jptfl-1+node) = .true.
        zr(jptf-1+node) = signls
10  end do
    cnoptf = '&&XPRREI.CNOPTF'
    call cnscno(cnsptf, ' ', 'NON', 'V', cnoptf,&
                'F', ibid)
    celptf = '&&XPRREI.CELPTF'
    lpain(1)='PNEUTR'
    lchin(1)=cnoptf
    lpaout(1)='PMOYEL'
    lchout(1)=celptf
!
    call calcul('S', 'MOY_NOEU_S', liggrd, 1, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
    call jedetr(cnoptf)
    cesptf = '&&XPRREI.CESPTF'
    call celces(celptf, 'V', cesptf)
    call jedetr(celptf)
    call jeveuo(cesptf//'.CESV', 'L', jfel)
    call jeveuo(cesptf//'.CESL', 'L', jfell)
    call jeveuo(cesptf//'.CESD', 'L', jfeld)
!
!-----------------------------------------------------------------------
!
!----------------------------------------
!   RECUPERATION DE |T| SUR LES ELEMENTS
!----------------------------------------
    call jeveuo(fiss//'.PRO.MES_EL'//'.CESV', 'L', jmeast)
    call jeveuo(fiss//'.PRO.MES_EL'//'.CESL', 'L', jmestl)
    call jeveuo(fiss//'.PRO.MES_EL'//'.CESD', 'L', jmestd)
!
!-----------------------------------------------------------------------
!
    cnsgdf = '&&XPRREI.CNSGDF'
    call cnscre(noma, 'NEUT_R', 1, 'X1', 'V',&
                cnsgdf)
    call jeveuo(cnsgdf//'.CNSV', 'E', jgdf)
!
    call jeveuo(cnsgdf//'.CNSL', 'E', jgdfl)
!
    cnogdf = '&&XPRREI.CNOGDF'
!
    celgdf = '&&XPRREI.CELGDF'
!
    if (levset .eq. 'LN') call cnscno(cnsln, ' ', 'NON', 'V', cnols,&
                                      'F', ibid)
    if (levset .eq. 'LT') call cnscno(cnslt, ' ', 'NON', 'V', cnols,&
                                      'F', ibid)
!
!-----BOUCLE PRINCIPALE-------------------------------------------------
!
    do 995 itemp = 1, itermx
        do 110 i = 1, nbno
!           RETREIVE THE NODE NUMBER
            node = zi(jnodto-1+i)
            zl(jvil-1+node) = .true.
            zl(jwil-1+node) = .true.
            zr(jvi-1+node) = 0.d0
            zr(jwi-1+node) = 0.d0
110      continue
!
!--------------------------------------
!   CALCUL DE GRAND F SUR LES ELEMENTS
!--------------------------------------
        do 90 i = 1, nbno
!           RETREIVE THE NODE NUMBER
            node = zi(jnodto-1+i)
            signls=0.d0
            if (abs(zr(jlsno-1+node)) .gt. r8prem()) signls = zr(jlsno- 1+node) / abs(zr(jlsno-1+&
                                                              &node))
            zl(jgdfl-1+node) = .true.
            zr(jgdf-1+node) = signls
!
90      continue
        call cnscno(cnsgdf, ' ', 'NON', 'V', cnogdf,&
                    'F', ibid)
!
!
!
        lpain(1)='PNEUTR'
        lchin(1)=cnogdf
        lpaout(1)='PMOYEL'
        lchout(1)=celgdf
!
        call calcul('S', 'MOY_NOEU_S', liggrd, 1, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
!
!
!-----------------------------------------------------------------------
!
!---------------------------------------------------------
!     CALCUL DU CHAM_ELEM DELTA_PHI ET DU CHAM_ELNO ALPHA
!---------------------------------------------------------
        call cnscno(cnsgls, ' ', 'NON', 'V', cnogls,&
                    'F', ibid)
        lpain(1)='PLSNO'
        lchin(1)=cnols
        lpain(2)='PGRLS'
        lchin(2)=cnogls
        lpain(3)='PGRANDF'
        lchin(3)=celgdf
        lpain(4)='PNIELNO'
        lchin(4)=fiss//'.PRO.NORMAL'
        lpaout(1)='PDPHI'
        lchout(1)=celdfi
        lpaout(2)='PALPHA'
        lchout(2)=celalf
!
!
        call calcul('S', 'XFEM_SMPLX_CALC', liggrd, 4, lchin,&
                    lpain, 2, lchout, lpaout, 'V',&
                    'OUI')
!
!
        call celces(celdfi, 'V', cesdfi)
        call jeveuo(cesdfi//'.CESV', 'L', jdelfi)
        call jeveuo(cesdfi//'.CESL', 'L', jdefil)
        call jeveuo(cesdfi//'.CESD', 'L', jdefid)
        call celces(celalf, 'V', cesalf)
        call jeveuo(cesalf//'.CESV', 'L', jalpha)
        call jeveuo(cesalf//'.CESL', 'L', jalphl)
        call jeveuo(cesalf//'.CESD', 'L', jalphd)
!
!---------------------------------------
!     CALCUL DES CHAMPS NODAUX VI ET WI
!---------------------------------------
!   BOUCLE SUR LES MAILLES DU MAILLAGE
        do 120 i = 1, nbma
!           RETREIVE THE ELEMENT NUMBER
            ima = zi(jelcal-1+i)
            nbnoma = zi(jconx2+ima) - zi(jconx2+ima-1)
!   VERIFICATION DU TYPE DE MAILLE
!           NDIME : DIMENSION TOPOLOGIQUE DE LA MAILLE
            ndime = zi(jtmdim-1+zi(jmai-1+ima))
            if (ndime .ne. ndim) goto 120
!
!   BOUCLE SUR LES NOEUDS DE LA MAILLE
            call cesexi('S', jfeld, jfell, ima, 1,&
                        1, 1, iadf)
            call cesexi('S', jmestd, jmestl, ima, 1,&
                        1, 1, iadmet)
            call cesexi('S', jdefid, jdefil, ima, 1,&
                        1, 1, iaddfi)
            do 130 ino = 1, nbnoma
                call cesexi('S', jalphd, jalphl, ima, ino,&
                            1, 1, iadalp)
                nuno = zi(jconx1-1+zi(jconx2+ima-1)+ino-1)
!
                zr(jvi-1+nuno) = zr(jvi-1+nuno) + zr(jalpha-1+iadalp) * (zr(jdelfi-1+iaddfi)-zr(j&
                                 &fel-1+iadf) * zr(jmeast-1+ iadmet))
!
                zr(jwi-1+nuno) = zr(jwi-1+nuno) + zr(jalpha-1+iadalp) * zr(jmeast-1+iadmet)
!
130          continue
120      continue
!
!-----------------------------------------------------------------------
!
!---------------------------------------
!     CALCUL DE LA LEVEL SET RESULTANTE
!---------------------------------------
        sigmls = 0.d0
        sdiff = 0.d0
        sdifft = 0.d0
        siglst = 0.d0
        do 200 i = 1, nbno
!           RETREIVE THE NODE NUMBER
            node = zi(jnodto-1+i)
!  ON ECARTE LES NOEUDS MILIEUX
            if (.not.zl(jnosom-1+node)) goto 200
!  ON ECARTE LES NOEUDS CALCULES PLUS HAUT
            if (zl(jzero-1+node)) goto 200
            lsprec = zr(jlsno-1+node)
            if (abs(zr(jwi-1+node)) .gt. r8prem()) then
!           WRITE(*,*)'La reinit du noeud d"indice',I,' se fait a WI>0'
                lsnouv = zr(jlsno-1+node)-deltat*(zr(jvi-1+node)/ zr(jwi-1+node))
                zr(jlsno-1+node) = lsnouv
                if (zl(jresdu-1+node)) then
                    sdiff = sdiff + (lsnouv-lsprec)**2.0d0
                    sigmls = sigmls + lsprec**2.0d0
                endif
                sdifft = sdifft + (lsnouv-lsprec)**2.0d0
                siglst = siglst + lsprec**2.0d0
            endif
200      continue
!  CAS OU TOUS LES RESIDUS A ESTIMER SONT CALCULES
        if (sdiff .eq. 0.d0 .and. sigmls .eq. 0.d0) then
            residu(itemp) = 0.d0
        else
            residu(itemp) = (sdiff/sigmls)**0.5d0
        endif
        if (sdifft .eq. 0.d0 .and. siglst .eq. 0.d0) then
            resit(itemp) = 0.d0
        else
            resit(itemp) = (sdifft/siglst)**0.5d0
        endif
!
!---------------------------------
!     CALCUL DES NOEUDS DONT WI=0
!---------------------------------
!
        do 800 i = 1, nbno
!           RETREIVE THE NODE NUMBER
            node = zi(jnodto-1+i)
!  ON ECARTE LES NOEUDS MILIEUX
            if (.not.zl(jnosom-1+node)) goto 800
!  ON ECARTE LES NOEUDS CALCULES PLUS HAUT
            if (zl(jzero-1+node)) goto 800
!
            if (abs(zr(jwi-1+node)) .lt. r8prem()) then
!           WRITE(*,*)'La reinit du noeud d"indice',I,' se fait a WI=0'
!
!    RECUPERATION DES MAILLES CONTENANT LE NOEUD I
                call jelira(jexnum(cnxinv, node), 'LONMAX', nmanoi, k8b)
                call jeveuo(jexnum(cnxinv, node), 'L', jmanoi)
!     BOUCLE SUR LES MAILLES CONTENANT LE NOEUD I
                dismin = r8maem()
!    ON ECARTE LES NOEUDS APPARTENANT A LA STRUCTURE MASSIVE
                if (((nmanoi.gt.2).and.(ndim.eq.2)) .or. ((nmanoi.gt.4) .and.(ndim.eq.3))) &
                goto 800
                numin = 0
                do 160 imai = 1, nmanoi
                    numai = zi(jmanoi-1+imai)
                    itypma = zi(jmai-1+numai)
                    call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
!     SI MAILLE NON VOLUMIQUE (en 3D) OU SURFACIQUE (en 2D) ON LA SAUTE
                    if ((&
                        ((typma(1:5).ne.'TETRA').and. (typma(1:4) .ne.'HEXA') ) .and.&
                        (ndim.eq.3)&
                        )&
                        .or.&
                        (&
                        ((typma(1:4) .ne.'QUAD').and.(typma(1:4).ne.'TRIA')) .and. ( ndim.eq.2)&
                        )) goto 160
!     BOUCLE SUR LES NOEUDS DE LA MAILLE
                    nbnoma = zi(jconx2+numai) - zi(jconx2+numai-1)
!
!    Algo modifie par Julien
!   (On cherche a appliquer une reinitialisation aux mailles de bord
!   et uniquement a elles!)
!
                    do 170 ino = 1, nbnoma
                        nuno=zi(jconx1-1+zi(jconx2+numai-1)+ino-1)
                        if (.not.zl(jnosom-1+nuno)) goto 170
!
                        if (abs(zr(jwi-1+nuno)) .gt. r8prem()) then
!
                            dist=0.d0
                            do 175 j = 1, ndim
                                ji(j) = zr( jcoor-1+3*(node-1)+j) - zr(jcoor-1+3*(nuno-1)+j )
                                dist=dist+ji(j)**2
175                          continue
                            dist=dist**0.5d0
!     On repere le noeud le plus proche
                            if (dist .lt. dismin) then
                                dismin = dist
                                numin = nuno
                            endif
                        endif
!
170                  continue
160              continue
! On affecte au noeud I (WI=0), la reactualisation du noeud NUMIN (WI>0)
                lsprec = zr(jlsno-1+node)
                lsnouv = zr(jlsno-1+node) -deltat*(zr(jvi-1+numin)/zr( jwi-1+numin))
                zr(jlsno-1+node) = lsnouv
            endif
!
800      continue
!
!
!---------------------------------------------------
!     CALCUL DU GRADIENT DE LA LEVEL SET RESULTANTE
!---------------------------------------------------
        if (levset .eq. 'LN') call cnscno(cnsln, ' ', 'NON', 'V', cnols,&
                                          'F', ibid)
        if (levset .eq. 'LT') call cnscno(cnslt, ' ', 'NON', 'V', cnols,&
                                          'F', ibid)
        lpain(1)='PGEOMER'
        lchin(1)=noma//'.COORDO'
        lpain(2)='PNEUTER'
        lchin(2)=cnols
        lpaout(1)='PGNEUTR'
        lchout(1)=celgls
!
        call calcul('S', 'GRAD_NEUT_R', liggrd, 2, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
!
!  PASSAGE D'UN CHAM_ELNO EN UN CHAM_NO
        call celces(celgls, 'V', chams)
        call cescns(chams, ' ', 'V', cnsgls, ' ',&
                    ier)
        call jeveuo(cnsgls//'.CNSV', 'E', jglsno)
!
!---------------------------------------
!     CONDITIONS DE SORTIE DE LA BOUCLE
!---------------------------------------
!  CONVERGENCE ATTEINTE
        if (levset .eq. 'LN' .and. residu(itemp) .lt. resiln) goto 999
        if (levset .eq. 'LT' .and. residu(itemp) .lt. resilt) goto 999
!  NOMBRE D'ITERATION MAXI
        if (itemp .eq. itermx) goto 999
!  MINIMUM LOCAL DU RESIDU GLOBAL ATTEINT
!  ON VEUT SORTIR SUR UN MINIMUM LOCAL SI LE RESIDU N'EST PAS TROP GROS
        if ((itemp.gt.5) .and. (residu(itemp).lt.(1000*resiln))) then
            if ((resit(itemp)-resit(itemp-1)) .ge. 0.d0) goto 999
        endif
!         WRITE(*,*)'Fin de l"iteration N.',ITEMP
!         WRITE(*,*)'Valeur du residu:', RESIDU(ITEMP)
!
995  end do
!-----FIN DE LA BOUCLE PRINCIPALE---------------------------------------
999  continue
!
!-------------------------------------
!     AFFICHAGE DES INFOS UTILISATEUR
!-------------------------------------
!      IF(NIV.GT.1) THEN
    write(ifm,*) '   REINITIALISATION DE '//levset//' :'
    write(ifm,900)
    write(ifm,901)
    write(ifm,902)
    write(ifm,903)
    do 300 i = 1, itemp
        write(ifm,904)i,residu(i),resit(i)
300  continue
    write(ifm,903)
!      ENDIF
    call codent(itemp, 'D', iterk3)
    call codree(residu(itemp), 'E', resk10)
    call codree(resit(itemp), 'E', retk10)
!  CONVERGENCE ATTEINTE
    if ((levset.eq.'LN'.and.residu(itemp).lt.resiln) .or.&
        (levset.eq.'LT'.and.residu(itemp).lt.resilt)) write(ifm, *&
                                                      )'   CONVERGENCE ATTEINTE A L''ITERATION '/&
                                                      &/iterk3
!
!  MINIMUM LOCAL ATTEINT
    if ((itemp.gt.5) .and. (residu(itemp).lt.(100*resiln))) then
        if (itemp .gt. 5 .and. (resit(itemp)-resit(itemp-1)) .ge. 0.d0) then
            write(ifm,*)'   MINIMUM LOCAL DU RESIDU GLOBAL ATTEINT.'
            write(ifm,*)'   ARRET A L''ITERATION '//iterk3
        endif
    endif
!
!  NOMBRE MAXI D'ITERATIONS ATTEINT
    if (itemp .eq. itermx) write(ifm,*)'   NOMBRE MAX D''ITERATION ('//iterk3//') ATTEINT'
!
    write(ifm,*)'   RESIDU LOCAL  = '//resk10
    write(ifm,*)'   RESIDU GLOBAL = '//retk10
!
    call assert(itemp.le.itermx)
!
!   DESTRUCTION DES OBJETS VOLATILES
    call jedetr(cnols)
    call jedetr(cnogls)
    call jedetr(celgls)
    call jedetr(chams)
    call jedetr(cnsvi)
    call jedetr(cnswi)
    call jedetr(celdfi)
    call jedetr(cesdfi)
    call jedetr(celalf)
    call jedetr(cesalf)
    call jedetr(cesptf)
    call jedetr(celgdf)
    call jedetr(cnsptf)
    call jedetr(cnsgdf)
    call jedetr(cnogdf)
!
    900 format(3x,'+',11('-'),'+',12('-'),'+',12('-'),'+')
    901 format('   | ITERATION |   RESIDU   |   RESIDU   |')
    902 format('   |           |   LOCAL    |   GLOBAL   |')
    903 format(3x,'+',11('-'),'+',12('-'),'+',12('-'),'+')
    904 format(3x,'|',5x,i3,2x,2(' |',e11.4),' | ')
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
