subroutine xprls0(fispre, noma, noesom, armin, cnsln,&
                  cnslt, isozro, levset, nodtor, eletor,&
                  poifi, trifi)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/conare.h'
    include 'asterfort/confac.h'
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
    include 'asterfort/padist.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'asterfort/xproj.h'
    include 'asterfort/xprpfi.h'
    character(len=2) :: levset
    character(len=8) :: noma, fispre
    character(len=19) :: cnsln, cnslt, isozro, noesom, nodtor, eletor, poifi
    character(len=19) :: trifi
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
!       XPRLS0   : X-FEM PROPAGATION : CALCUL DES LS PROCHE DES ISO-0
!       ------     -     --                       --                -
!    DANS LE CADRE DE LA PROPAGAGTION DE FISSURE XFEM, CALCUL DES VRAIES
!     FONCTIONS DE DISTANCE SIGNEE SUR LES NOEUDS DES MAILLES COUPEES
!     PAR L'ISOZERO D'UNE LEVEL SET.
!    SI LEVSET='LN' : ON CALCULE LN & LT AU VOISINAGE DE LN=0
!    SI LEVSET='LT' : ON CALCULE LT AU VOISINAGE DE LT=0
!
!    ENTREE
!        NOMA    :   NOM DU CONCEPT MAILLAGE
!        NOESOM  :   VECTEUR LOGIQUE CONTENANT L'INFO. 'NOEUD SOMMET'
!        LCMIN   :   LONGEUR DE PLUS PETIT ARETE DU MAILLAGE NOMA
!        CNSLN   :   CHAM_NO_S LEVEL SET NORMALE
!        CNSLT   :   CHAM_NO_S LEVEL SET TANGENTE
!        LEVSET  :   ='LN' SI ON REINITIALISE LN
!                    ='LT' SI ON REINITIALISE LT
!        NODTOR  :   LISTE DES NOEUDS DEFINISSANT LE DOMAINE DE CALCUL
!        ELETOR  :   LISTE DES ELEMENTS DEFINISSANT LE DOMAINE DE CALCUL
!        POIFI   :   POUR LA METHODE UPWIND: NOM DE L'OBJET JEVEUX OU
!                    LES POINTS DEFINISSANTS LA SURFACE LSN=0 DOIVENT
!                    ETRE STOCKES
!                    POUR LES AUTRES METHODES: ' '
!        TRIFI   :   POUR LA METHODE UPWIND: NOM DE L'OBJET JEVEUX OU
!                    LES POINTS D'INTERSECTION ENTRE CHAQUE ELEMENT ET
!                    LSN=0 SONT STOCKES
!                    POUR LES AUTRES METHODES: ' '
!    SORTIE
!        CNSLN   :   CHAM_NO_S LEVEL SET NORMALE
!                    (CALCULEE SEULEMENT SI LEVSET = 'LN')
!        CNSLT   :   CHAM_NO_S LEVEL SET NORMALE CALCULEE
!        ISOZRO  :   VECTEUR LOGIQUE IDIQUANT SI LA "VRAIE" LEVEL SET
!                    (DISTANCE SIGNEE) A ETE CALCULEE
!        POIFI  :   OBJET JEVEUX REMPLI (UPWIND SEULEMENT)
!        TRIFI  :   OBJET JEVEUX REMPLI (UPWIND SEULEMENT)
!
!     ------------------------------------------------------------------
!
!
    integer :: ino, inoa, inob, ima, ifm, niv, nbnog, nbmag, iret, jconx1
    integer :: jconx2, ndim, jzero, jmaco, nbmaco, nbnoma, jmai, nunoa, nunob
    integer :: jnomco, nbnoco, nuno, nmaabs, nptint, ntri, itypma, itri, jcoor
    integer :: jnosom, nbnozo, ia, ib, ic, cptzo, jlsno, jltno, jnouls, jnoult
    integer :: ar(12, 3), nbar, iar, na, nb, ibid, ipt, nblsn0, ibid2(12, 3)
    integer :: ifq, nbf, fa(6, 4), nbsom, jtmdim, ndime, i
    real(kind=8) :: p(3), x(7), y(7), z(7), xa, ya, za, xb, yb, zb, s, a(3)
    real(kind=8) :: b(3), c(3), eps(3), m(3), d, vn(3), lsna, lsnb, lsta, lstb
    real(kind=8) :: lstc, lst(6), bestd, bestdi, lsn, bestlt, bestli, dist
    real(kind=8) :: armin, longar, longmx, mp(3)
    character(len=8) :: k8b, typma
    character(len=19) :: maicou, nomcou, vnouls, vnoult, mai, poifis, trifis
    logical :: dejain, dejadi, noemai, in, deja
    real(kind=8) :: toll
!
!     DOMAIN RESTRICTION
    integer :: jnodto, jeleto, node, elem, nbno, nbma
!
!     UPWIND INTEGRATION
    integer :: jpoi, jtri, nbpfis, pos
    logical :: intabl, upwind
!
!  TRIANGLES ABC QUE L'ON PEUT FORMER A PARTIR DE N POINTS (N=3 A 6)
    integer :: iatri(20), ibtri(20), ictri(20)
!
    real(kind=8) :: lsnp, lsnnew, lstnew
    character(len=19) :: pproj
    integer :: ipproj
!
!        ---------------------
!        |  I | TRIANGLE | N |
!        --------------------
!        |  1 |   1 2 3  | 3 |
!        --------------------
!        |  2 |   1 2 4  |   |
!        |  3 |   1 3 4  | 4 |
!        |  4 |   2 3 4  |   |
!        --------------------
!        |  5 |   1 2 5  |   |
!        |  6 |   1 3 5  |   |
!        |  7 |   1 4 5  | 5 |
!        |  8 |   2 3 5  |   |
!        |  9 |   2 4 5  |   |
!        | 10 |   3 4 5  |   |
!        --------------------
!        | 11 |   1 2 6  |   |
!        | 12 |   1 3 6  |   |
!        | 13 |   1 4 6  |   |
!        | 14 |   1 5 6  |   |
!        | 15 |   2 3 6  | 6 |
!        | 16 |   2 4 6  |   |
!        | 17 |   2 5 6  |   |
!        | 18 |   3 4 6  |   |
!        | 19 |   3 5 6  |   |
!        | 20 |   4 5 6  |   |
!        --------------------
    data   iatri/1,1,1,2,1,1,1,2,2,3,1,1,1,1,2,2,2,3,3,4/
    data   ibtri/2,2,3,3,2,3,4,3,4,4,2,3,4,5,3,4,5,4,5,5/
    data   ictri/3,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6/
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
    poifis = poifi
    trifis = trifi
!
!     CHECK IF THE UPWIND SCHEME WILL BE USED
    if ((poifis(1:1).ne.' ') .and. (trifis(1:1).ne.' ')) then
        upwind = .true.
    else
        upwind = .false.
        poifis = '&&XPRLS0.SPOI'
        trifis = '&&XPRLS0.STRI'
    endif
!
!     EVALUATION OF THE TOLERANCE USED TO ASSESS IF THE VALUE OF THE
!     NORMAL LEVELSET IN ONE NODE IS ZERO OR NOT
!     THIS IS FIXED TO 1% OF THE LENGTH OF THE SMALLEST ELEMENT EDGE
!     IN THE MESH
    toll = 1.0d-2*armin
!
!      IF (NIV.GT.1)
    write(ifm,*)'   CALCUL DES LEVEL SETS A PROXIMITE '&
     &   //'DE L''ISOZERO DE '//levset//'.'
!
!  RECUPERATION DES CARACTERISTIQUES DU MAILLAGE
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnog,&
                k8b, iret)
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbmag,&
                k8b, iret)
!
!     RETRIEVE THE NUMBER OF THE NODES THAT MUST TO BE USED IN THE
!     CALCULUS (SAME ORDER THAN THE ONE USED IN THE CONNECTION TABLE)
    call jeveuo(nodtor, 'L', jnodto)
!
!     RETRIEVE THE TOTAL NUMBER OF THE NODES THAT MUST BE ELABORATED
    call jelira(nodtor, 'LONMAX', nbno, k8b)
!
!     RETRIEVE THE LIST OF THE ELEMENTS SUPPORTING THE NODES IN THE TORE
    call jeveuo(eletor, 'L', jeleto)
!
!     RETRIEVE THE NUMBER OF ELEMENTS DEFINING THE TORE
    call jelira(eletor, 'LONMAX', nbma, k8b)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    mai = noma//'.TYPMAIL'
    call jeveuo(mai, 'L', jmai)
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                k8b, iret)
    call jeveuo('&CATA.TM.TMDIM', 'L', jtmdim)
!
!   RECUPERATION DE L'ADRESSE DES VALEURS DES LS
    if (levset .eq. 'LN') then
        call jeveuo(cnsln//'.CNSV', 'E', jlsno)
        call jeveuo(cnslt//'.CNSV', 'E', jltno)
    else if (levset.eq.'LT') then
        call jeveuo(cnslt//'.CNSV', 'E', jlsno)
        call jeveuo(cnsln//'.CNSV', 'L', jltno)
    endif
!
!  RECUPERATION DE L'ADRESSE DE L'INFORMATION 'NOEUD SOMMET'
    call jeveuo(noesom, 'L', jnosom)
!
!  RECUPERATION DE L'ADRESSE DES VALEURS DE ISOZRO
    call jeveuo(isozro, 'E', jzero)
    do 10 ino = 1, nbnog
        zl(jzero-1+ino)=.false.
10  end do
!
! INITIALISATION DU VECTEUR LST
    do 20 i = 1, 6
        lst(i)=0.d0
20  end do
!
!
!-----------------------------------------------------------------------
!     DANS UN PREMIER TEMPS,ON S'OCCUPE DES NOEUDS SOMMETS SUR L'ISOZERO
!     ( UTILE DANS LE CAS DE MAILLES 1 OU 2 NOEUDS SONT A 0 )
!-----------------------------------------------------------------------
    nbnozo=0
    do 50 ino = 1, nbno
        node = zi(jnodto-1+ino)
        if (abs(zr(jlsno-1+node)) .lt. toll .and. zl(jnosom-1+node)) then
            zr(jlsno-1+node)=0.d0
            zl(jzero-1+node)=.true.
            nbnozo = nbnozo+1
        endif
50  end do
!
!--------------------------------------------------------------------
!     ON REPERE LES MAILLES VOLUMIQUES COUPEES OU TANGENTEES PAR LS=0
!     ( I.E. LES MAILLES OU L'ON PEUT INTERPOLER UN PLAN LS=0 )
!--------------------------------------------------------------------
!  VECTEUR CONTENANT LES NUMEROS DE MAILLES COUPEES
    maicou = '&&XPRLS0.MAICOU'
    call wkvect(maicou, 'V V I', nbmag, jmaco)
!
    nbmaco=0
    do 100 ima = 1, nbma
!
        elem = zi(jeleto-1+ima)
!
!   VERIFICATION DU TYPE DE MAILLE
!         NDIME : DIMENSION TOPOLOGIQUE DE LA MAILLE
        ndime = zi(jtmdim-1+zi(jmai-1+elem))
        if (ndime .ne. ndim) goto 100
        nbnoma = zi(jconx2+elem) - zi(jconx2+elem-1)
!
!  ON COMPTE D'ABORD LE NOMBRE DE NOEUDS DE LA MAILLE QUI S'ANNULENT
        cptzo=0
        do 105 inoa = 1, nbnoma
            nunoa=zi(jconx1-1+zi(jconx2+elem-1)+inoa-1)
            lsna = zr(jlsno-1+nunoa)
            if (abs(lsna) .lt. toll .and. zl(jnosom-1+nunoa)) cptzo = cptzo+1
105      continue
!
!  SI AU - TROIS NOEUDS S'ANNULENT (en 3D),ON A UN PLAN D'INTERSECTION
        if (cptzo .ge. ndim) then
            nbmaco = nbmaco + 1
            zi(jmaco-1+nbmaco) = elem
            goto 100
        endif
!
!  ON PARCOURT LES ARETES DE L'ELEMENT
        itypma=zi(jmai-1+elem)
        call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
        call conare(typma, ar, nbar)
        do 110 iar = 1, nbar
            na=ar(iar,1)
            nb=ar(iar,2)
            nunoa=zi(jconx1-1+zi(jconx2+elem-1)+na-1)
            nunob=zi(jconx1-1+zi(jconx2+elem-1)+nb-1)
            lsna = zr(jlsno-1+nunoa)
            lsnb = zr(jlsno-1+nunob)
!  SI UNE ARETE EST COUPEE,LA MAILLE L'EST FORCEMENT
            if ((lsna*lsnb) .lt. 0.d0 .and. (abs(lsna).gt.r8prem()) .and.&
                (abs(lsnb).gt.r8prem())) then
                nbmaco = nbmaco + 1
                zi(jmaco-1+nbmaco) = elem
                goto 100
            endif
110      continue
!
100  end do
!
!     IF EVERYTHING GOES CORRECTLY, I SHOULD FIND AT LEAST ONE ELEMENT
!     CUT BY THE ISOZERO OF LSN. IT'S BETTER TO CHECK IT BEFORE
!     CONTINUING.
    call assert(nbmaco.gt.0)
!
!----------------------------------------------------------------------
!     CREATE THE STRUCTURE WHERE THE TRIANGLES FORMING THE LSN=0 ARE
!     STORED. THESE INFORMATIONS WILL BE USED BY THE UPWIND SCHEME.
!----------------------------------------------------------------------
!
!     NUMBER OF INTERSECTION POINTS BETWEEN THE LSN=0 AND EACH
!     ELEMENT (MAX=6) AND LIST OF THEIR POSITION IN THE COORDINATE
!     TABLE BELOW (JTRI)
!     EACH ROW:   NUMBER OF POINTS,P1,...,P6
    call wkvect(trifis, 'V V I', nbmaco*7, jtri)
    do 147 ima = 1, nbmaco
        zi(jtri-1+7*(ima-1)+1) = 0
147  end do
!
!     COORDINATES OF THE POINTS OF INTERSECTION BETWEEN EACH ELEMENT
!     AND THE LSN=0. THE THREE COORDINATES AND THE LSN ARE STORED.
!     EACH ROW:   X,Y,Z,LSN
    call wkvect('&&XPRLS0.POIFIS', 'V V R', nbmaco*24, jpoi)
!
!     INITIALISE THE COUNTER FOR JTRI TABLE
    nbpfis = 0
!
!-----------------------------------------------------
!     ON REPERE LES NOEUDS SOMMETS DES MAILLES COUPEES
!-----------------------------------------------------
!  VECTEUR CONTENANT LES NUMEROS DE NOEUD DES MAILLES COUPEES
    nomcou = '&&XPRLS0.NOMCOU'
    call wkvect(nomcou, 'V V I', nbmaco*6, jnomco)
!
    nbnoco=0
!  BOUCLE SUR LES NOEUDS
    do 200 inoa = 1, nbno
        node = zi(jnodto-1+inoa)
!  ON NE CONSIDERE QUE LE NOEUDS SOMMETS
        if (zl(jnosom-1+node)) then
!  BOUCLE SUR LES MAILLES COUPEES
            do 210 ima = 1, nbmaco
                nmaabs = zi(jmaco-1+ima)
                nbnoma = zi(jconx2+nmaabs)-zi(jconx2+nmaabs-1)
!  BOUCLE SUR LES NOEUDS DE LA MAILLE
                do 220 inob = 1, nbnoma
                    nunob = zi(jconx1-1+zi(jconx2+nmaabs-1)+inob-1)
                    if (nunob .eq. node) then
                        nbnoco = nbnoco+1
                        zi(jnomco-1+nbnoco) = node
                        goto 200
                    endif
220              continue
210          continue
        endif
200  end do
!
!----------------------------------------------
!     CALCUL DES LS SUR LES NOEUDS SELECTIONNES
!----------------------------------------------
!  VECTEURS CONTENANT LES NOUVELLES LS POUR LES NOEUDS DE NOMCOU
    vnouls = '&&XPRLS0.VNOULS'
    call wkvect(vnouls, 'V V R', nbnoco, jnouls)
!
    vnoult = '&&XPRLS0.VNOULT'
    call wkvect(vnoult, 'V V R', nbnoco, jnoult)
!
    pproj = '&&XPRLS0.TMP'
    call wkvect(pproj, 'V V L', nbnoco, ipproj)
!
!  BOUCLE SUR LES NOEUDS DES MAILLES COUPEES
!  -----------------------------------------
    do 300 ino = 1, nbnoco
        nuno = zi(jnomco-1+ino)
        lsn = zr(jlsno-1+nuno)
!
!  SI LE NOEUD EST SUR L'ISOZERO, ON L'A DEJA REPERE
        if (zl(jzero-1+nuno)) then
            zr(jnouls-1+ino) = 0.d0
            zr(jnoult-1+ino) = zr(jltno-1+nuno)
            goto 300
        endif
!
        dejain=.false.
        dejadi=.false.
!
!
!  BOUCLE SUR LES MAILLES COUPEES DONT LE NOEUD (INO) EST SOMMET
!  -------------------------------------------------------------
!  BOUCLE SUR LES MAILLES COUPEES
        do 310 ima = 1, nbmaco
            nmaabs = zi(jmaco-1+ima)
            nbnoma = zi(jconx2+nmaabs)-zi(jconx2+nmaabs-1)
!
!  ON CHERCHE SI LE NOEUD(NUNO) APPARTIENT A LA MAILLE(NMAABS)
            noemai=.false.
            do 320 i = 1, nbnoma
                if (zi(jconx1-1+zi(jconx2+nmaabs-1)+i-1) .eq. nuno) noemai=.true.
320          continue
!
!  SI LE NOEUD APPARTIENT A LA MAILLE
            if (noemai) then
!
                p(1)=zr(jcoor-1+3*(nuno-1)+1)
                p(2)=zr(jcoor-1+3*(nuno-1)+2)
                if (ndim .eq. 3) p(3)=zr(jcoor-1+3*(nuno-1)+3)
                if (ndim .eq. 2) p(3)=0.d0
!
!
!  ON RECUPERE LES POINTS D'INTERSECTION ISOZERO-ARETES
                nptint = 0
!
! On initialise les coordonnes des points d'intersection
                do 321 i = 1, 7
                    x(i)=0.d0
                    y(i)=0.d0
                    z(i)=0.d0
321              continue
!
!
!  ON RECHERCHE D'ABORD LES NOEUDS QUI SONT DES POINTS D'INTERSECTIONS
                do 340 inoa = 1, nbnoma
                    nunoa = zi(jconx1-1+zi(jconx2+nmaabs-1)+inoa-1)
                    if (.not.zl(jnosom-1+nunoa)) goto 340
                    lsna = zr(jlsno-1+nunoa)
                    if (abs(lsna) .lt. r8prem()) then
                        nptint = nptint+1
                        x(nptint) = zr(jcoor-1+3*(nunoa-1)+1)
                        y(nptint) = zr(jcoor-1+3*(nunoa-1)+2)
                        if (ndim .eq. 3) then
                            z(nptint) = zr(jcoor-1+3*(nunoa-1)+3)
                        else if (ndim.eq.2) then
                            z(nptint) = 0.d0
                        endif
                        lst(nptint)=zr(jltno-1+nunoa)
!
                    endif
340              continue
!
!
!  ON PARCOURT ENSUITE LES ARETES [AB] DE LA MAILLE
                itypma=zi(jmai-1+nmaabs)
                call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
!  On verifie prealablement qu'une face ne contient pas
! plus de 2 aretes coupees par LSN0
!
                nbsom = 0400
!
                if ((typma(1:4).eq.'HEXA') .or. (typma(1:4).eq.'QUAD')) nbsom = 4
                if ((typma(1:5).eq.'TETRA') .or. (typma(1:4).eq.'TRIA')) nbsom = 3
!
                if (ndim .eq. 3) call confac(typma, ibid2, ibid, fa, nbf)
                if (ndim .eq. 2) then
                    nbf=1
                    do 341 i = 1, nbsom
                        fa(1,i)=i
341                  continue
                endif
!
                do 610 ifq = 1, nbf
                    nblsn0 = 0
                    na=fa(ifq,1)
                    nunoa=zi(jconx1-1+zi(jconx2+nmaabs-1)+na-1)
                    lsna=zr(jlsno-1+(nunoa-1)+1)
                    do 411 i = 2, nbsom
                        nunob=zi(jconx1-1+zi(jconx2+nmaabs-1)+fa(ifq,&
                        i)-1)
                        lsnb=zr(jlsno-1+(nunob-1)+1)
                        if ((lsna*lsnb.lt.0.d0) .and. (abs(lsna) .gt.r8prem()) .and.&
                            (abs(lsnb).gt.r8prem())) then
                            nblsn0 = nblsn0 + 1
                        endif
                        lsna=lsnb
411                  continue
!  On affecte a B le point A initial pour comparer D et A
                    nunob=nunoa
                    lsnb=zr(jlsno-1+(nunob-1)+1)
                    if ((lsna*lsnb.lt.0.d0) .and. (abs(lsna).gt.r8prem( )) .and.&
                        (abs(lsnb).gt.r8prem())) then
                        nblsn0 = nblsn0 + 1
                    endif
!  Arret fatal si on trouve au moins 3 points d'intersection sur
! une meme face
                    if (nblsn0 .ge. 3) call u2mess('F', 'XFEM_61')
610              continue
!
!
                call conare(typma, ar, nbar)
!  On cherche la plus grande ar�te de l'�l�ment
                longmx=0
                do 333 iar = 1, nbar
                    na=ar(iar,1)
                    nb=ar(iar,2)
                    nunoa=zi(jconx1-1+zi(jconx2+nmaabs-1)+na-1)
                    nunob=zi(jconx1-1+zi(jconx2+nmaabs-1)+nb-1)
                    do 444 i = 1, ndim
                        a(i)= zr(jcoor-1+3*(nunoa-1)+i)
                        b(i)= zr(jcoor-1+3*(nunob-1)+i)
444                  continue
                    longar=padist(ndim,a,b)
                    if (longar .gt. longmx) longmx = longar
333              continue
                do 330 iar = 1, nbar
                    na=ar(iar,1)
                    nb=ar(iar,2)
!
                    nunoa=zi(jconx1-1+zi(jconx2+nmaabs-1)+na-1)
                    nunob=zi(jconx1-1+zi(jconx2+nmaabs-1)+nb-1)
                    lsna = zr(jlsno-1+nunoa)
                    lsnb = zr(jlsno-1+nunob)
!
                    if ((lsna*lsnb.lt.0.d0) .and. (abs(lsna).gt.r8prem( )) .and.&
                        (abs(lsnb).gt.r8prem())) then
!  UN POINT D'INTERSECTION SE SITUE ENTRE LES NOEUDS (NUNOA) ET (NUNOB)
!  Incrementation commente par julien pour verifier la validite du point
!                     NPTINT = NPTINT+1
                        xa = zr(jcoor-1+3*(nunoa-1)+1)
                        ya = zr(jcoor-1+3*(nunoa-1)+2)
                        if (ndim .eq. 3) za = zr(jcoor-1+3*(nunoa-1)+3)
                        if (ndim .eq. 2) za = 0.d0
                        xb = zr(jcoor-1+3*(nunob-1)+1)
                        yb = zr(jcoor-1+3*(nunob-1)+2)
                        if (ndim .eq. 3) zb = zr(jcoor-1+3*(nunob-1)+3)
                        if (ndim .eq. 2) zb = 0.d0
                        s = abs(lsna) / ( abs(lsna) + abs(lsnb) )
                        x(nptint+1) = xa + s*(xb-xa)
                        y(nptint+1) = ya + s*(yb-ya)
                        z(nptint+1) = za + s*(zb-za)
!
!  ON VERIFIE LA VALIDITE DU POINT
                        deja=.false.
                        if (ndim .eq. 3) then
                            dist = (&
                                   (&
                                   (x(nptint+1)-xa)**2)+ ((y(nptint+ 1)-ya)**2)+ ((z(nptint+1)-za&
                                   )**2&
                                   )&
                                   )**0.5d0
                        else if (ndim.eq.2) then
                            dist = ( ((x(nptint+1)-xa)**2)+ ((y(nptint+ 1)-ya)**2 ) )**0.5d0
                        endif
!
!
                        if (nptint .gt. 0) then
                            do 380 ipt = 1, nptint
!
                                if (ndim .eq. 3) then
                                    dist = (&
                                           (&
                                           (&
                                           x(nptint+1)-x(nptint+1- ipt))**2)+ ((y(nptint+1)-y(npt&
                                           &int+ 1-ipt))**2)+ ((z(nptint+1)-z( nptint+1-ipt)&
                                           )**2&
                                           )&
                                           )**0.5d0
                                else if (ndim.eq.2) then
                                    dist = (&
                                           (&
                                           (&
                                           x(nptint+1)-x(nptint+1- ipt))**2)+ ((y(nptint+1)-y(npt&
                                           &int+ 1-ipt)&
                                           )**2&
                                           )&
                                           )**0.5d0
                                endif
                                if (dist .gt. (longmx*ndim**0.5d0)) then
                                    deja=.true.
                                    goto 330
                                endif
!
                                if (dist .lt. r8prem()) then
                                    deja=.true.
                                    goto 330
                                endif
!
!
380                          continue
!
                        endif
!
                        if (.not.deja) then
                            nptint = nptint+1
                            lsta = zr(jltno-1+nunoa)
                            lstb = zr(jltno-1+nunob)
                            lst(nptint) = lsta + s*(lstb-lsta)
                        endif
                    endif
!
!
!
330              continue
!
!  VERIFICATION SUR LE NOMBRE DE POINTS D'INTERSECTION TROUVES
!
!              LES ARETES DE LA MAILLE 'NOMMA' DE TYPE 'TYPMA' ONT  N
!              POINTS D'INTERSECTION AVEC L'ISO-ZERO DE 'LEVSET'
!
!
                call assert(.not.(&
                            (typma(1:5) .eq.'TETRA'.and.nptint.gt.4) .or.&
                            (typma(1:5) .eq.'PENTA'.and.nptint.gt.5) .or.&
                            (typma(1:4) .eq.'HEXA'.and.nptint.gt.6) .or.&
                            (typma(1:4) .eq.'QUAD'.and.nptint.gt.2)&
                            ))
!
                if (ndim .eq. 2 .and. nptint .lt. 2) goto 310
                if (ndim .eq. 3 .and. nptint .lt. 3) goto 310
!
                call assert(nptint.le.6)
!
!              CORRECTION FOR THE 2D CASE
!              ONLY TWO POINTS ARE FOUND FOR THE 2D CASE. IN ORDER TO
!              USE THE PROJECTION ON A TRIANGLE, THAT IS THE SAME CODE
!              USED FOR THE 3D CASE, A "VIRTUAL" THIRD POINT IS CREATED.
!              IN ORDER TO AVOID ILL CONDITIONED PROBLEMS, ITS Z-COORD
!              IS EVALUATED AS THE DISTANCE BETWEEN THE OTHER TWO (REAL)
!              POINTS
                if (ndim .eq. 2) then
                    call assert(nptint.eq.2)
                    nptint = nptint+1
                    x(nptint) = x(1)
                    y(nptint) = y(1)
                    z(nptint) = ((x(1)-x(2))**2+(y(1)-y(2))**2)** 0.5d0
                    lst(nptint) = lst(1)
                endif
!
!              STORE THE INTERSECTION POINTS FOR THE UPWIND INTEGRATION.
!              STORE THE NUMBER OF POINTS FOR THE ELEMENT
                zi(jtri-1+7*(ima-1)+1) = nptint
!
!              STORE EACH POINT IN THE COORDINATE TABLE
                do 425 ipt = 1, nptint
!
!                 CHECK IF THE INTERSECTION POINT HAS BEEN ALREADY
!                 INCLUDED IN THE COORDINATE TABLE
                    intabl = .false.
                    do 423 pos = 1, nbpfis
                        dist = sqrt(&
                               (&
                               x(ipt)-zr(jpoi-1+4*(pos-1)+1))** 2+ (y(ipt)-zr(jpoi-1+4*(pos-1)+2)&
                               &)**2+ (z(ipt)-zr(jpoi-1+4*(pos-1)+3)&
                               )**2&
                               )
                        if (dist .lt. r8prem()) then
                            intabl=.true.
                            goto 424
                        endif
423                  continue
!
424                  continue
!
                    if (.not.intabl) then
!                    THE COORDINATES OF THE POINT MUST BE STORED...
                        nbpfis = nbpfis+1
                        zr(jpoi-1+4*(nbpfis-1)+1) = x(ipt)
                        zr(jpoi-1+4*(nbpfis-1)+2) = y(ipt)
                        zr(jpoi-1+4*(nbpfis-1)+3) = z(ipt)
                        zr(jpoi-1+4*(nbpfis-1)+4) = lst(ipt)
!                    ...THE NUMBER OF THE POINT AS WELL
                        zi(jtri-1+7*(ima-1)+ipt+1) = nbpfis
                    else
!                    ONLY THE NUMBER OF THE POINT MUST BE STORED
                        zi(jtri-1+7*(ima-1)+ipt+1) = pos
                    endif
!
425              continue
!
!  CALCUL DE DISTANCE DU NOEUD (INO) A L'ISOZERO SUR LA MAILLE (NMAABS)
!  --------------------------------------------------------------------
!  ON PARCOURT TOUS LES TRIANGLES QUE L'ON PEUT FORMER AVEC LES POINTS
!  D'INTERSECTION ISOZERO-ARETES :
!
                if (nptint .eq. 3) ntri=1
                if (nptint .eq. 4) ntri=4
                if (nptint .eq. 5) ntri=10
                if (nptint .eq. 6) ntri=20
!
                do 350 itri = 1, ntri
                    ia=iatri(itri)
                    ib=ibtri(itri)
                    ic=ictri(itri)
                    a(1)=x(ia)
                    a(2)=y(ia)
                    a(3)=z(ia)
                    b(1)=x(ib)
                    b(2)=y(ib)
                    b(3)=z(ib)
                    c(1)=x(ic)
                    c(2)=y(ic)
                    c(3)=z(ic)
                    if (levset .eq. 'LN') then
                        lsta=lst(ia)
                        lstb=lst(ib)
                        lstc=lst(ic)
                    endif
!
                    call xproj(p, a, b, c, m,&
                               mp, d, vn, eps, in)
!
!  ON RECHERCHE LA DISTANCE MINIMALE TELLE QUE EPS1>0 & EPS2>0 & EPS3>0
!  --------------------------------------------------------------------
!  ON STOCKE LA PREMIERE DISTANCE CALCULEE
                    if (.not.dejadi) then
                        bestd = d
                        if (levset .eq. 'LN') bestlt = eps(1)*lstb + eps(2)*lstc + eps(3)*lsta
                        dejadi=.true.
                    endif
!
!  ON STOCKE LA DISTANCE MINIMALE
                    if (d .lt. bestd) then
                        bestd = d
                        if (levset .eq. 'LN') bestlt = eps(1)*lstb + eps(2)*lstc + eps(3)*lsta
                    endif
!
!  ON STOCKE LA DISTANCE MINIMALE AVEC PROJECTION DANS LE TRIANGLE ABC
                    if (in) then
                        if (.not.dejain) then
                            bestdi = d
                            if (levset .eq. 'LN') bestli = eps(1)*lstb + eps(2)*lstc + eps(3)*lst&
                                                           &a
                            dejain=.true.
                        endif
                        if (d .lt. bestdi) then
                            bestdi = d
                            if (levset .eq. 'LN') bestli = eps(1)*lstb + eps(2)*lstc + eps(3)*lst&
                                                           &a
                        endif
                    endif
350              continue
!
            endif
310      continue
!
!  ON ATTRIBUE LES LS CORRESPONDANT AUX MEILLEURES DISTANCES TROUVEES
!  ------------------------------------------------------------------
!  (INTERIEURES, LE CAS ECHEANT)
        zr(jnouls-1+ino) = bestd * sign(1.d0,lsn)
        if (levset .eq. 'LN') zr(jnoult-1+ino) = bestlt
        if (dejain) then
            zr(jnouls-1+ino) = bestdi*sign(1.d0,lsn)
            if (levset .eq. 'LN') zr(jnoult-1+ino) = bestli
            zl(ipproj-1+ino) = .false.
        else
            zl(ipproj-1+ino) = .true.
        endif
!
        call assert(dejadi)
        zl(jzero-1+nuno)=.true.
!
300  end do
!
!     RESIZE THE TABLE CONTAINING THE INTERSECTION POINTS BETWEEN EACH
!     ELEMENT AND LSN=0 (ONLY FOR THE UPWIND SCHEME)
    call wkvect(poifis, 'V V R', nbpfis*4, pos)
!
    do 500 i = 1, nbpfis*4
        zr(pos-1+i) = zr(jpoi-1+i)
500  end do
!
    call jedetr('&&XPRLS0.POIFIS')
!
!  REMPLACEMENT DES LEVEL SETS PAR CELLES CALCULEES
!  ------------------------------------------------
    do 400 ino = 1, nbnoco
        nuno=zi(jnomco-1+ino)
!
!        CALCULATE THE LEVEL SETS BY PROJECTION AT ALL THE NODES WHICH
!        HAVEN'T A PROJECTION INSIDE A TRIANGLE
        if (zl(jzero-1+nuno) .and. zl(ipproj-1+ino)) then
            p(1)=zr(jcoor-1+3*(nuno-1)+1)
            p(2)=zr(jcoor-1+3*(nuno-1)+2)
            if (ndim .eq. 3) p(3)=zr(jcoor-1+3*(nuno-1)+3)
            if (ndim .eq. 2) p(3)=0.d0
            lsnp = zr(jlsno-1+nuno)
            if (lsnp .ne. 0.d0) then
                call xprpfi(p, lsnp, armin, poifis, trifis,&
                            fispre, ndim, lsnnew, lstnew)
                zr(jnouls-1+ino) = lsnnew
                if (levset .eq. 'LN') zr(jnoult-1+ino) = lstnew
            endif
        endif
!
        zr(jlsno-1+nuno) = zr(jnouls-1+ino)
!
        if (levset .eq. 'LN') then
            zr(jltno-1+nuno) = zr(jnoult-1+ino)
        endif
!
400  end do
!
!      IF (NIV.GT.1)
    write(ifm,*)'   NOMBRE DE LEVEL SETS CALCULEES :',nbnoco+nbnozo
!
!   DESTRUCTION DES OBJETS VOLATILES
    call jedetr(maicou)
    call jedetr(nomcou)
    call jedetr(vnouls)
    call jedetr(vnoult)
    call jedetr(pproj)
!
    if (.not.upwind) then
        call jedetr(poifis)
        call jedetr(trifis)
    endif
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
