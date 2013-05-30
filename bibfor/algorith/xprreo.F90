subroutine xprreo(noma, fiss, noesom, noresi, cnsln,&
                  cnslt, cnsgln, cnsglt, deltat, isozro,&
                  cnxinv, nodtor, eletor, liggrd)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/r8maem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/calcul.h'
    include 'asterfort/celces.h'
    include 'asterfort/cescns.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/cnscno.h'
    include 'asterfort/cnscre.h'
    include 'asterfort/codent.h'
    include 'asterfort/codree.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    real(kind=8) :: deltat
    character(len=8) :: noma, fiss
    character(len=19) :: cnsln, cnslt, cnsgln, cnsglt, noresi, noesom, isozro
    character(len=19) :: cnxinv, nodtor, eletor, liggrd
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
! TOLE CRP_20
!     ------------------------------------------------------------------
!
!       XPRREO   : X-FEM PROPAGATION : REORTHOGONALISATION DE LEVEL SET
!       ------     -     --            ---
!    REORTHOGONALISATION D'UNE LEVEL SET APRES PROPAGATION
!
!    ENTREE
!        NOMA    : NOM DU CONCEPT MAILLAGE
!        FISS    : NOM DU CONCEPT FISSURE XFEM
!        NOESOM  : INDICATEUR LOGIQUE DE NOEUD SOMMET
!        NORESI  : VECTEUR LOGIQUE INDIQUANT SI LE RESIDU EST A ESTIMER
!                   SUR LE NOEUD
!        CNSLN   : CHAM_NO_S  LEVEL SET NORMALE
!        CNSLT   : CHAM_NO_S  LEVEL SET TANGENTE
!        CNSGLN  : CHAM_NO_S  GRADIENT DE LEVEL SET NORMALE
!        CNSGLT  : CHAM_NO_S  GRADIENT DE LEVEL SET TANGENTE
!        DELTAT  : PAS DE TEMPS
!        LCMIN   : LONGUEUR CARACTERISTIQUE MINIMALE DES ELEMENTS
!        ISOZRO  : VECTEUR LOGIQUE INDIQUANT SI LA "VRAIE" LEVEL SET
!                   (DISTANCE SIGNEE) A ETE CALCULEE AU NOEUD
!        CNXINV  : MATRICE DE CONNECTIVITE INVERSEE
!        NODTOR  : LISTE DES NOEUDS DEFINISSANT LE DOMAINE DE CALCUL
!        ELETOR  : LISTE DES ELEMENTS DEFINISSANT LE DOMAINE DE CALCUL
!        LIGGRD  : LIGREL DU DOMAINE DE CALCUL (VOIR XPRTOR.F)
!
!    SORTIE
!        CNSLT   : CHAM_NO_S  LEVEL SET TANGENTE REORTHOGONALISEE
!        CNSGLT  : CHAM_NO_S  GRADIENT DE LEVEL SET TANGENTE
!
!     ------------------------------------------------------------------
!
!
    integer :: i, ifm, niv, nbnom, nbma, iret, jconx1, jconx2, jmai, ndim, jlnno
    integer :: jglnno, jltno, jgltno, jvi, jvil, jwi, jwil, jmeast, jmestl
    integer :: jmestd, jvf, jvfl, jgdf, jgdfl, itemp, ima, jdelfi, jdefil
    integer :: jdefid, jalpha, jalphl, jalphd, nbnoma, itypma, iadmet, iaddfi
    integer :: ino, iadalp, nuno, ibid, j, jzero, jnosom, jresdu, nmanoi, jmanoi
    integer :: imai, numai, jcoor, jtmdim, ndime, numin
    real(kind=8) :: signln, normgn, normgt, sigmlt, sdiff, ltprec, ltnouv
    real(kind=8) :: siglst, sdifft, ji(3), dist, dismin
    character(len=3) :: iterk3
    character(len=8) :: k8b, lpain(4), lpaout(2), typma, licmp(3)
    character(len=10) :: resk10, retk10
    character(len=19) :: cnolt, cnoglt, celglt, chams, celdfi, cesdfi, celalf
    character(len=19) :: cesalf, cnsvi, cnswi, cnsvf, cnsgdf, cnogdf, celgdf
    character(len=19) :: mai
    character(len=24) :: lchin(4), lchout(2)
!
    real(kind=8) :: resimx
    parameter      (resimx=1.d-3)
    integer :: itermx
    parameter      (itermx=500)
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
    call jeveuo(cnsln//'.CNSV', 'L', jlnno)
    call jeveuo(cnsgln//'.CNSV', 'L', jglnno)
    call jeveuo(cnslt//'.CNSV', 'E', jltno)
    call jeveuo(cnsglt//'.CNSV', 'E', jgltno)
!
!
!  RECUPERATION DE L'ADRESSE DE L'INFORMATION 'NOEUD SOMMET'
    call jeveuo(noesom, 'L', jnosom)
!
!  RECUPERATION DE L'ADRESSE DE L'INFORMATION 'RESIDU A CALCULER'
    call jeveuo(noresi, 'L', jresdu)
!
!  RECUPERATION DES NOEUDS DONT LA "VRAIE" LST A ETE CALCULEE
    call jeveuo(isozro, 'L', jzero)
!
!---------------------------------
!   CREATION DES OBJETS VOLATILES
!---------------------------------
!   LEVEL SET TANGENTE
    cnolt = '&&XPRREO.CNOLT'
!   GRADIENT DE LA LEVEL SET
    cnoglt = '&&XPRREO.CNOGLT'
    celglt = '&&XPRREO.CELGLT'
    chams = '&&XPRREO.CHAMS'
!   V AU NOEUDS
    cnsvi = '&&XPRREO.CNSVI'
    call cnscre(noma, 'NEUT_R', 1, 'X1', 'V',&
                cnsvi)
    call jeveuo(cnsvi//'.CNSV', 'E', jvi)
    call jeveuo(cnsvi//'.CNSL', 'E', jvil)
!   W AU NOEUDS
    cnswi = '&&XPRREO.CNSWI'
    call cnscre(noma, 'NEUT_R', 1, 'X1', 'V',&
                cnswi)
    call jeveuo(cnswi//'.CNSV', 'E', jwi)
    call jeveuo(cnswi//'.CNSL', 'E', jwil)
!
!   DELTA_PHI
    celdfi = '&&XPRREO.CELDFI'
    cesdfi = '&&XPRREO.CESDFI'
!   ALPHA
    celalf = '&&XPRREO.CELALF'
    cesalf = '&&XPRREO.CESALF'
!
!-------------------------------------------------------------
!   CALCUL DU CHAM_NO_S VECT_F = SIGN(LN)*GRAD(LN)/|GRAD(LN)|
!-------------------------------------------------------------
    licmp(1)='X1'
    licmp(2)='X2'
    licmp(3)='X3'
    cnsvf = '&&XPRREO.CNSVF'
    call cnscre(noma, 'NEUT_R', 3, licmp, 'V',&
                cnsvf)
    call jeveuo(cnsvf//'.CNSV', 'E', jvf)
    call jeveuo(cnsvf//'.CNSL', 'E', jvfl)
!
!      WRITE(*,*)'Nombre de noeuds du maillage :', NBNO
!
    do 10 i = 1, nbno
!        RETREIVE THE NODE NUMBER
        node = zi(jnodto-1+i)
        zl(jvfl-1+3*(node-1)+1)=.true.
        zl(jvfl-1+3*(node-1)+2)=.true.
        zl(jvfl-1+3*(node-1)+3)=.true.
        if (ndim .eq. 3) then
            normgn = (&
                     zr(&
                     jglnno-1+3*(node-1)+1)**2.d0 + zr(jglnno-1+ 3*(node-1)+2)**2.d0 + zr(jglnno-&
                     &1+3*(node-1)+3&
                     )**2.d0&
                     )** 0.5d0
        else if (ndim.eq.2) then
            normgn = (zr( jglnno-1+2*(node-1)+1)**2.d0 + zr(jglnno-1+ 2*(node-1)+2 )**2.d0&
                     )**0.5d0
        endif
!
        if (normgn .lt. r8prem() .or. abs(zr(jlnno-1+node)) .lt. r8prem()) then
            zr(jvf-1+3*(node-1)+1) = 0.d0
            zr(jvf-1+3*(node-1)+2) = 0.d0
            zr(jvf-1+3*(node-1)+3) = 0.d0
        else
            signln = zr(jlnno-1+node) / abs(zr(jlnno-1+node))
            zr(jvf-1+3*(node-1)+1)=signln*zr(jglnno-1+ndim*(node-1)+1)&
            /normgn
            zr(jvf-1+3*(node-1)+2)=signln*zr(jglnno-1+ndim*(node-1)+2)&
            /normgn
            zr(jvf-1+3*(node-1)+3)= 0.d0
            if (ndim .eq. 3) zr( jvf-1+3*(node-1)+3)=signln*zr(jglnno-1+ 3*(node-1)+3 )/normgn
        endif
!
10  end do
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
    cnsgdf = '&&XPRREO.CNSGDF'
    call cnscre(noma, 'NEUT_R', 1, 'X1', 'V',&
                cnsgdf)
    call jeveuo(cnsgdf//'.CNSV', 'E', jgdf)
    call jeveuo(cnsgdf//'.CNSL', 'E', jgdfl)
!
    cnogdf = '&&XPRREO.CNOGDF'
!
    celgdf = '&&XPRREO.CELGDF'
!
    call cnscno(cnslt, ' ', 'NON', 'V', cnolt,&
                'F', ibid)
!-----BOUCLE PRINCIPALE-------------------------------------------------
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
!         RETREIVE THE NODE NUMBER
            node = zi(jnodto-1+i)
            if (ndim .eq. 3) then
                normgt = (&
                         zr(&
                         jgltno-1+3*(node-1)+1)**2.d0 + zr( jgltno-1+3*(node-1)+2)**2.d0 + zr(jgl&
                         &tno-1+3*(node-1)+ 3&
                         )**2.d0&
                         )**0.5d0
                zl(jgdfl-1+node) = .true.
!            IF (NORMGT.EQ.0.D0) THEN
!  Modif Julien
                if (normgt .lt. r8prem()) then
                    zr(jgdf-1+node)=0.d0
                else
                    zr(jgdf-1+node)= ( zr(jgltno-1+3*(node-1)+1)*zr(&
                    jvf-1+3*(node-1)+1) + zr(jgltno-1+3*(node-1)+2)*&
                    zr(jvf-1+3*(node-1)+2) + zr(jgltno-1+3*(node-1)+3)&
                    *zr(jvf-1+3*(node-1)+3) ) / normgt
                endif
            else if (ndim.eq.2) then
                normgt = (zr( jgltno-1+2*(node-1)+1)**2.d0 + zr( jgltno-1+2*(node-1)+2 )**2.d0&
                         )**0.5d0
                zl(jgdfl-1+node) = .true.
!
                if (normgt .lt. r8prem()) then
                    zr(jgdf-1+node)=0.d0
                else
                    zr(jgdf-1+node)= ( zr(jgltno-1+2*(node-1)+1)*zr(&
                    jvf-1+3*(node-1)+1) + zr(jgltno-1+2*(node-1)+2)*&
                    zr(jvf-1+3*(node-1)+2)) / normgt
                endif
            endif
!
90      continue
        call cnscno(cnsgdf, ' ', 'NON', 'V', cnogdf,&
                    'F', ibid)
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
        call cnscno(cnsglt, ' ', 'NON', 'V', cnoglt,&
                    'F', ibid)
        lpain(1)='PLSNO'
        lchin(1)=cnolt
        lpain(2)='PGRLS'
        lchin(2)=cnoglt
        lpain(3)='PGRANDF'
        lchin(3)=celgdf
        lpain(4)='PNIELNO'
        lchin(4)=fiss//'.PRO.NORMAL'
        lpaout(1)='PDPHI'
        lchout(1)=celdfi
        lpaout(2)='PALPHA'
        lchout(2)=celalf
!
        call calcul('S', 'XFEM_SMPLX_CALC', liggrd, 4, lchin,&
                    lpain, 2, lchout, lpaout, 'V',&
                    'OUI')
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
!
!
!---------------------------------------
!     CALCUL DES CHAMPS NODAUX VI ET WI
!---------------------------------------
!   BOUCLE SUR LES MAILLES DU MAILLAGE
!
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
            call cesexi('S', jmestd, jmestl, ima, 1,&
                        1, 1, iadmet)
            call cesexi('S', jdefid, jdefil, ima, 1,&
                        1, 1, iaddfi)
            do 130 ino = 1, nbnoma
                call cesexi('S', jalphd, jalphl, ima, ino,&
                            1, 1, iadalp)
                nuno = zi(jconx1-1+zi(jconx2+ima-1)+ino-1)
                zr(jvi-1+nuno) = zr(jvi-1+nuno) + zr(jalpha-1+iadalp) * zr(jdelfi-1+iaddfi)
                zr(jwi-1+nuno) = zr(jwi-1+nuno) + zr(jalpha-1+iadalp) * zr(jmeast-1+iadmet)
130          continue
!
120      continue
!
!-----------------------------------------------------------------------
!
!---------------------------------------
!     CALCUL DE LA LEVEL SET RESULTANTE
!---------------------------------------
        sigmlt = 0.d0
        sdiff = 0.d0
        sdifft = 0.d0
        siglst = 0.d0
        do 200 i = 1, nbno
!           RETREIVE THE NODE NUMBER
            node = zi(jnodto-1+i)
!  ON ECARTE LES NOEUDS MILIEUX
            if (.not.zl(jnosom-1+node)) goto 200
!  ON ECARTE LES NOEUDS CALCULES PRECEDEMMENT
            if (zl(jzero-1+node)) goto 200
            ltprec = zr(jltno-1+node)
            if (abs(zr(jwi-1+node)) .gt. r8prem()) then
!               WRITE(*,*)'La reortho du noeud N',I,' se fait a WI>0'
                ltnouv = zr(jltno-1+node) - deltat * ( zr(jvi-1+node) / zr(jwi-1+node) )
                zr(jltno-1+node) = ltnouv
                if (zl(jresdu-1+node)) then
                    sdiff = sdiff + (ltnouv-ltprec)**2.0d0
                    sigmlt = sigmlt + ltprec**2.0d0
                endif
                sdifft = sdifft + (ltnouv-ltprec)**2.0d0
                siglst = siglst + ltprec**2.0d0
            endif
200      continue
!
!  CAS OU TOUS LES RESIDUS A ESTIMER SONT CALCULES
        if (sdiff .eq. 0.d0 .and. sigmlt .eq. 0.d0) then
            residu(itemp) = 0.d0
        else
            residu(itemp) = (sdiff/sigmlt)**0.5d0
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
        do 800 i = 1, nbno
!           RETREIVE THE NODE NUMBER
            node = zi(jnodto-1+i)
!  ON ECARTE LES NOEUDS MILIEUX
            if (.not.zl(jnosom-1+node)) goto 800
!  ON ECARTE LES NOEUDS CALCULES PLUS HAUT
            if (zl(jzero-1+node)) goto 800
!
            if (abs(zr(jwi-1+node)) .lt. r8prem()) then
!           WRITE(*,*)'La reortho du noeud d"indice',I,' se fait a WI=0'
!
!    RECUPERATION DES MAILLES CONTENANT LE NOEUD NODE
                call jelira(jexnum(cnxinv, node), 'LONMAX', nmanoi, k8b)
                call jeveuo(jexnum(cnxinv, node), 'L', jmanoi)
!     BOUCLE SUR LES MAILLES CONTENANT LE NOEUD NODE
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
                ltprec = zr(jltno-1+node)
                ltnouv = zr(jltno-1+node) -deltat*(zr(jvi-1+numin)/zr( jwi-1+numin))
!
                zr(jltno-1+node) = ltnouv
!
!
!
            endif
!
!
!
800      continue
!---------------------------------------------------
!     CALCUL DU GRADIENT DE LA LEVEL SET RESULTANTE
!---------------------------------------------------
        call cnscno(cnslt, ' ', 'NON', 'V', cnolt,&
                    'F', ibid)
        lpain(1)='PGEOMER'
        lchin(1)=noma//'.COORDO'
        lpain(2)='PNEUTER'
        lchin(2)=cnolt
        lpaout(1)='PGNEUTR'
        lchout(1)=celglt
!
        call calcul('S', 'GRAD_NEUT_R', liggrd, 2, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
!
!  PASSAGE D'UN CHAM_ELNO EN UN CHAM_NO
        call celces(celglt, 'V', chams)
        call cescns(chams, ' ', 'V', cnsglt, ' ',&
                    ibid)
        call jeveuo(cnsglt//'.CNSV', 'E', jgltno)
!
!-------------------------------------------------
!     TESTS DES CONDITIONS DE SORTIE DE LA BOUCLE
!-------------------------------------------------
!  CONVERGENCE ATTEINTE
        if (residu(itemp) .lt. resimx) goto 999
!  NOMBRE D'ITERATION MAXI
        if (itemp .eq. itermx) goto 999
!  MINIMUM LOCAL DU RESIDU GLOBAL ATTEINT
        if (itemp .gt. 5) then
            if ((resit(itemp)-resit(itemp-1)) .ge. 0.d0) goto 999
        endif
!
995  end do
!-----FIN DE LA BOUCLE PRINCIPALE---------------------------------------
999  continue
!
!-------------------------------------
!     AFFICHAGE DES INFOS UTILISATEUR
!-------------------------------------
!      IF(NIV.GT.1) THEN
    write(ifm,*) '   REORTHOGONALISATION DE LT :'
    write(ifm,900)
    write(ifm,901)
    write(ifm,902)
    write(ifm,903)
    do 300 i = 1, itemp
        write(ifm,904)i,residu(i),resit(i)
300  continue
    write(ifm,903)
!      ENDIF
!
    call codent(itemp, 'D', iterk3)
    call codree(residu(itemp), 'E', resk10)
    call codree(resit(itemp), 'E', retk10)
!
!  CONVERGENCE ATTEINTE
    if (residu(itemp) .lt. resimx) write(ifm, *)'   CONVERGENCE ATTEINTE A L''ITERATION '//iterk3
!
!  MINIMUM LOCAL ATTEINT
    if (itemp .gt. 5 .and. (resit(itemp)-resit(itemp-1)) .ge. 0.d0) then
        write(ifm,*)'   MINIMUM LOCAL DU RESIDU GLOBAL ATTEINT.'
        write(ifm,*)'   ARRET A L''ITERATION '//iterk3
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
    call jedetr(cnolt)
    call jedetr(cnoglt)
    call jedetr(celglt)
    call jedetr(chams)
    call jedetr(cnsvi)
    call jedetr(cnswi)
    call jedetr(cnsvf)
    call jedetr(celdfi)
    call jedetr(cesdfi)
    call jedetr(celalf)
    call jedetr(cesalf)
    call jedetr(celgdf)
    call jedetr(cnsgdf)
    call jedetr(cnogdf)
!
    900 format(3x,'+',11('-'),'+',12('-'),'+',12('-'),'+')
    901 format('   | ITERATION |   RESIDU   |   RESIDU   |')
    902 format('   |           |   LOCAL    |   GLOBAL   |')
    903 format(3x,'+',11('-'),'+',12('-'),'+',12('-'),'+')
    904 format(3x,'|',5x,i3,2x,2(' |',e11.4),' | ')
!
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
