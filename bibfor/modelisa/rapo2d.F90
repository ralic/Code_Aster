subroutine rapo2d(numdlz, iocc, fonrez, lisrez, chargz)
    implicit none
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
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/r8prem.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/assvec.h"
#include "asterfort/calcul.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlim1.h"
#include "asterfort/getvem.h"
#include "asterfort/getvtx.h"
#include "asterfort/imprel.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/malin1.h"
#include "asterfort/mecact.h"
#include "asterfort/memare.h"
#include "asterfort/mesomm.h"
#include "asterfort/reajre.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: numdlz, chargz, fonrez, lisrez
! -------------------------------------------------------
!     RACCORD POUTRE-2D PAR DES RELATIONS LINEAIRES
!     ENTRE LE NOEUD DES MAILLES DE BORD DE LA STRUCTURE 2D
!     ET LE NOEUD DE LA POUTRE DONNE PAR L'UTILISATEUR
! -------------------------------------------------------
!  NUMDLZ        - IN    - K14  - : NOM DU NUMEDDL DU LIGREL DU MODELE
!                                     (IL A ETE CREE SUR LA VOLATILE)
!  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
!  FONREZ        - IN    - K4   - : 'REEL'
!  LISREZ        - IN    - K19  - : NOM DE LA SD LISTE_RELA
!  CHARGE        - IN    - K8   - : NOM DE LA SD CHARGE
!                - JXVAR -      -
! -------------------------------------------------------
!
!
! --------- VARIABLES LOCALES ---------------------------
    integer :: nmocl
    parameter (nmocl=300)
    character(len=2) :: typlag
    character(len=4) :: typval, typcoe
    character(len=8) :: betaf, mod, nomg, k8bid, poslag
    character(len=8) :: noma, nomcmp(nmocl)
    character(len=8) :: noepou, nocmp(3), kcmp(3)
    character(len=8) :: lpain(2), lpaout(2)
    character(len=9) :: nomte
    character(len=16) :: motfac, motcle(2), typmcl(2), option
    character(len=19) :: ligrmo, ligrel
    character(len=24) :: lchin(2), lchout(2), nolili, lismai
    character(len=24) :: lisnoe, noeuma, vale1, vale2, grnoma, nogrno
    character(len=8) :: charge
    character(len=14) :: numddl
    character(len=19) :: lisrel
    integer :: ntypel(nmocl), icmp(6), niv, ifm, vali(2)
    integer :: iop, nliai, i, narl, nrl, jnoma, jcoor, inom
    integer :: nbcmp, nddla, nbec, jprnm, nlili, k, iaprno, lonlis, ilisno
    integer :: jlisma, nbma, nbno, nbgno, nno, n1, jgro, in, numnop
    integer :: ino, idiner, idch1, idch2, nbterm
    integer :: jlisno, jlisdl, jliscr, jliscc, jlisdi, jlisdm, ival
    integer :: iocc, iarg
    real(kind=8) :: igzz, coorig(3), beta, eps, un
    real(kind=8) :: xpou, ypou, s, s1, xg, yg, dnorme
    real(kind=8) :: ax, ay, axx, ayy
    complex(kind=8) :: betac, ccmp(3)
! --------- FIN  DECLARATIONS  VARIABLES LOCALES --------
!
    call jemarq()
! --- RECUPERATION DES PARAMETRE D IMPRESSION
    call infniv(ifm, niv)
! -------------------------------------------------------
    numddl = numdlz
    charge = chargz
    lisrel = lisrez
!
    motfac = 'LIAISON_ELEM'
    call getvtx(motfac, 'OPTION', iocc=iocc, scal=option, nbret=iop)
    if (option .ne. '2D_POU') then
        call utmess('F', 'MODELISA6_39', sk=option)
    endif
!
    call getfac(motfac, nliai)
    if (nliai .eq. 0) goto 999
!
! --- INITIALISATIONS
!     ---------------
!
! --- TYPE DES VALEURS AU SECOND MEMBRE DES RELATIONS
    typval = fonrez
! --- TYPE DES VALEURS DES COEFFICIENTS DES RELATIONS
    typcoe = 'REEL'
! --- VALEUR DU SECOND MEMBRE DES RELATIONS QUAND C'EST UNE FONCTION
    betaf = '&FOZERO'
! --- VALEUR DU SECOND MEMBRE DES RELATIONS QUAND C'EST UN REEL
    beta = 0.0d0
! --- VALEUR DU SECOND MEMBRE DES RELATIONS QUAND C'EST UN COMPLEXE
    betac = (0.0d0,0.0d0)
    eps = 1.0d-02
    un = 1.0d0
    kcmp(1) = ' '
    kcmp(2) = ' '
    kcmp(3) = ' '
    ccmp(1) = (0.0d0,0.0d0)
    ccmp(2) = (0.0d0,0.0d0)
    ccmp(3) = (0.0d0,0.0d0)
    do i = 1, 6
        icmp(i) = 0
    end do
!
    ligrel = '&&RAPO2D'
    lisnoe = '&&RAPO2D.LISTE_NOEUDS'
    lismai = '&&RAPO2D.LISTE_MAILLES'
    motcle(1) = 'GROUP_MA_1'
    motcle(2) = 'MAILLE_1'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
! --- ON REGARDE SI LES MULTIPLICATEURS DE LAGRANGE SONT A METTRE
! --- APRES LES NOEUDS PHYSIQUES LIES PAR LA RELATION DANS LA MATRICE
! --- ASSEMBLEE :
! --- SI OUI TYPLAG = '22'
! --- SI NON TYPLAG = '12'
!     -------------------
    call getvtx(motfac, 'NUME_LAGR', iocc=iocc, nbval=0, nbret=narl)
    if (narl .ne. 0) then
        call getvtx(motfac, 'NUME_LAGR', iocc=iocc, scal=poslag, nbret=nrl)
        if (poslag(1:5) .eq. 'APRES') then
            typlag = '22'
        else
            typlag = '12'
        endif
    else
        typlag = '12'
    endif
!
! --- -----------------------------------------------------------------
! --- MODELE ASSOCIE AU LIGREL DE CHARGE
    call dismoi('NOM_MODELE', charge(1:8), 'CHARGE', repk=mod)
!
! --- -----------------------------------------------------------------
!     LIGREL DU MODELE
    ligrmo = mod(1:8)//'.MODELE'
!
! --- -----------------------------------------------------------------
! --- MAILLAGE ASSOCIE AU MODELE
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
    noeuma = noma//'.NOMNOE'
    grnoma = noma//'.GROUPENO'
!
! --- -----------------------------------------------------------------
! --- RECUPERATION DU TABLEAU DES COORDONNEES
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
! --- -----------------------------------------------------------------
! --- RECUPERATION DES NOMS DES DDLS
    nomg = 'DEPL_R'
    nomte = 'D_DEPL_R_'
!
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', inom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp)
    nddla = nbcmp - 1
    if (nddla .gt. nmocl) then
        vali (1) = nmocl
        vali (2) = nddla
        call utmess('F', 'MODELISA8_29', ni=2, vali=vali)
    endif
    do i = 1, nddla
        nomcmp(i) = zk8(inom-1+i)
        call jenonu(jexnom('&CATA.TE.NOMTE', nomte//nomcmp(i) (1:7)), ntypel(i))
    end do
    call dismoi('NB_EC', nomg, 'GRANDEUR', repi=nbec)
!
! --- -----------------------------------------------------------------
! --- ACCES A L'OBJET .PRNM
    if (nbec .gt. 10) then
        call utmess('F', 'MODELISA_94')
    else
        call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
    endif
!
! --- -----------------------------------------------------------------
! --- RECUPERATION DU .PRNO ASSOCIE AU MAILLAGE
    call jelira(numddl//'.NUME.PRNO', 'NMAXOC', nlili)
    k = 0
    do i = 1, nlili
        call jenuno(jexnum(numddl//'.NUME.LILI', i), nolili)
        if (nolili(1:8) .ne. '&MAILLA') goto 30
        k = i
 30     continue
    end do
    ASSERT(k.ne.0)
    call jeveuo(jexnum(numddl//'.NUME.PRNO', k), 'L', iaprno)
!
! --- -----------------------------------------------------------------
! --- ACQUISITION DE LA LISTE DES NOEUDS A LIER
!     (CETTE LISTE EST NON REDONDANTE)
    call malin1(motfac, charge, iocc, 1, lisnoe,&
                lonlis)
    call jeveuo(lisnoe, 'L', ilisno)
!
! --- -----------------------------------------------------------------
! --- CONSTITUTION DU LIGREL FORME DES MAILLES DE BORD DE LA SURFACE 2D
!
! --- CREATION ET AFFECTATION DU VECTEUR DE K8 DE NOM LISMAI
!     CONTENANT LES NOMS DES MAILLES FORMANT LE LIGREL A CREER
    call reliem(' ', noma, 'NU_MAILLE', motfac, iocc,&
                2, motcle(1), typmcl(1), lismai, nbma)
    call jeveuo(lismai, 'L', jlisma)
!
!     CREATION ET AFFECTATION DU LIGREL
    call exlim1(zi(jlisma), nbma, mod, 'V', ligrel)
!
! --- -----------------------------------------------------------------
! --- ACQUISITION DES MOTS-CLES NOEUD_2 OU GROUP_NO_2
    nbno = 0
    nbgno = 0
!
    call getvem(noma, 'NOEUD', motfac, 'NOEUD_2', iocc,&
                iarg, 0, k8bid, nbno)
!
    if (nbno .eq. 0) then
        call getvem(noma, 'GROUP_NO', motfac, 'GROUP_NO_2', iocc,&
                    iarg, 0, k8bid, nbgno)
        if (nbgno .eq. 0) then
            call utmess('F', 'MODELISA6_40', sk=motfac)
        endif
    endif
!
    if (nbno .ne. 0) then
        nbno = -nbno
        if (nbno .ne. 1) then
            call utmess('F', 'MODELISA6_41')
        endif
        call getvem(noma, 'NOEUD', motfac, 'NOEUD_2', iocc,&
                    iarg, nbno, noepou, nno)
    endif
!
    if (nbgno .ne. 0) then
        nbgno = -nbgno
        if (nbgno .ne. 1) then
            call utmess('F', 'MODELISA6_42')
        endif
        call getvem(noma, 'GROUP_NO', motfac, 'GROUP_NO_2', iocc,&
                    iarg, nbgno, nogrno, nno)
        call jelira(jexnom(grnoma, nogrno), 'LONUTI', n1)
        if (n1 .ne. 1) then
            call utmess('F', 'MODELISA6_43', sk=nogrno)
        else
            call jeveuo(jexnom(grnoma, nogrno), 'L', jgro)
            in = zi(jgro+1-1)
            call jenuno(jexnum(noeuma, in), noepou)
        endif
    endif
!
! --- -----------------------------------------------------------------
! --- NUMERO DU NOEUD POUTRE A LIER
    call jenonu(jexnom(noeuma, noepou), numnop)
! --- COORDONNEES DU NOEUD POUTRE
    xpou = zr(jcoor-1+3*(numnop-1)+1)
    ypou = zr(jcoor-1+3*(numnop-1)+2)
!
! --- -----------------------------------------------------------------
! --- CALCUL SUR CHAQUE ELEMENT DE BORD A RELIER A LA POUTRE
!     DES CARACTERISTIQUES GEOMETRIQUES SUIVANTES :
!        SOMME/B_ELEMENT(1,X,Y,X*X,Y*Y,X*Y)DS
    lpain(1) = 'PGEOMER'
    lchin(1) = noma//'.COORDO'
    lpaout(1) = 'PCASECT'
    lchout(1) = '&&RAPO2D.PSECT'
!
    call calcul('S', 'CARA_SECT_POUT3', ligrel, 1, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
! --- -----------------------------------------------------------------
! --- VECTEUR DES QUANTITES GEOMETRIQUES PRECITEES SOMMEES
!     SUR LA SURFACE DE RACCORD, CES QUANTITES SERONT NOTEES :
!        A1 = S,AX,AY,AXX,AYY
    call wkvect('&&RAPO2D.INERTIE_RACCORD', 'V V R', 6, idiner)
! --- -----------------------------------------------------------------
!     SOMMATION DES QUANTITES GEOMETRIQUES ELEMENTAIRES
!     DANS LE VECTEUR &&RAPO2D.INERTIE_RACCORD :
    call mesomm(lchout(1), 6, vr=zr(idiner))
!
    s = zr(idiner+1-1)
    ax = zr(idiner+2-1)
    ay = zr(idiner+3-1)
    axx = zr(idiner+4-1)
    ayy = zr(idiner+5-1)
!
    if (abs(s) .lt. r8prem()) then
        call utmess('F', 'MODELISA6_46')
    endif
    s1 = 1.0d0/s
!
! --- -----------------------------------------------------------------
! --- COORDONNEES DU CENTRE GEOMETRIQUE G DE LA SECTION DE RACCORD
!     XG = AX/S, YG = AY/S
    xg = s1*ax
    yg = s1*ay
!
! --- -----------------------------------------------------------------
!     VERIFICATION DE L'IDENTITE GEOMETRIQUE DE G AVEC LE
!     NOEUD POUTRE A RACCORDER :
    dnorme = sqrt((xpou-xg)*(xpou-xg)+ (ypou-yg)*(ypou-yg))
    if (dnorme .gt. eps) then
        ASSERT(.false.)
    endif
!
!
! --- -----------------------------------------------------------------
!     CALCUL DE LA COMPOSANTE IZZ DU TENSEUR D'INERTIE EN G
    igzz = axx + ayy - s*(xg*xg+yg*yg)
!
! --- -----------------------------------------------------------------
!     NOTATION DANS LA CARTE DE NOM '&&RAPO2D.CAORIGE' DES
!     COORDONNEES DU CENTRE GEOMETRIQUE G DE LA SECTION DE RACCORD
    nocmp(1) = 'X'
    nocmp(2) = 'Y'
!
    coorig(1) = xg
    coorig(2) = yg
!
    call mecact('V', '&&RAPO2D.CAORIGE', 'LIGREL', ligrel, 'GEOM_R',&
                ncmp=2, lnomcmp=nocmp, vr=coorig)
!
! --- DETERMINATION DE 2 LISTES  DE VECTEURS PAR ELEMENT PRENANT
!     LEURS VALEURS AUX NOEUDS DES ELEMENTS.
! --- LA PREMIERE LISTE DE NOM 'VECT_NI' A POUR VALEURS AU NOEUD
!     I D'UN ELEMENT : SOMME/S_ELEMENT(NI,0)DS
! --- LA SECONDE LISTE DE NOM 'VECT_XYNI' A POUR VALEURS AU NOEUD
!     I D'UN ELEMENT : SOMME/S_ELEMENT(X*NI,Y*NI)DS
!        AVEC  X = XM - XG = NJ*XJ - XG
!              Y = YM - YG = NJ*YJ - YG
!
    lpain(1) = 'PGEOMER'
    lpain(2) = 'PORIGIN'
    lchin(1) = noma//'.COORDO'
    lchin(2) = '&&RAPO2D.CAORIGE'
    lpaout(1) = 'PVECTU1'
    lpaout(2) = 'PVECTU2'
    lchout(1) = '&&RAPO2D.VECT_NI'
    lchout(2) = '&&RAPO2D.VECT_XYZNI'
!
    call calcul('S', 'CARA_SECT_POUT4', ligrel, 2, lchin,&
                lpain, 2, lchout, lpaout, 'V',&
                'OUI')
!
! --- -----------------------------------------------------------------
! --- CREATION DES .RERR DES VECTEURS EN SORTIE DE CALCUL
    call memare('V', '&&RAPO2D', mod, ' ', ' ',&
                'CHAR_MECA')
!
! --- -----------------------------------------------------------------
!     ASSEMBLAGE DE LCHOUT(1) DANS LE CHAMNO DE NOM 'CH_DEPL_1'
    call jedetr('&&RAPO2D           .RELR')
    call reajre('&&RAPO2D', lchout(1), 'V')
    call assvec('V', 'CH_DEPL_1', 1, '&&RAPO2D           .RELR', [1.d0],&
                numddl, ' ', 'ZERO', 1)
!
! --- -----------------------------------------------------------------
!     ASSEMBLAGE DE LCHOUT(2) DANS LE CHAMNO DE NOM 'CH_DEPL_2'
    call jedetr('&&RAPO2D           .RELR')
    call reajre('&&RAPO2D', lchout(2), 'V')
    call assvec('V', 'CH_DEPL_2', 1, '&&RAPO2D           .RELR', [1.d0],&
                numddl, ' ', 'ZERO', 1)
!
    vale1 = 'CH_DEPL_1          .VALE'
    vale2 = 'CH_DEPL_2          .VALE'
    call jeveuo(vale1, 'L', idch1)
    call jeveuo(vale2, 'L', idch2)
!
! --- -----------------------------------------------------------------
! --- CREATION DES TABLEAUX NECESSAIRES A L'AFFECTATION DE LISREL
! --- MAJORANT DU NOMBRE DE TERMES DANS UNE RELATION
    nbterm = 2*lonlis + 2
! --- VECTEUR DU NOM DES NOEUDS
    call wkvect('&&RAPO2D.LISNO', 'V V K8', nbterm, jlisno)
! --- VECTEUR DU NOM DES DDLS
    call wkvect('&&RAPO2D.LISDDL', 'V V K8', nbterm, jlisdl)
! --- VECTEUR DES COEFFICIENTS REELS
    call wkvect('&&RAPO2D.COER', 'V V R', nbterm, jliscr)
! --- VECTEUR DES COEFFICIENTS COMPLEXES
    call wkvect('&&RAPO2D.COEC', 'V V C', nbterm, jliscc)
! --- VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
    call wkvect('&&RAPO2D.DIRECT', 'V V R', 2*nbterm, jlisdi)
! --- VECTEUR DES DIMENSIONS DE CES DIRECTIONS
    call wkvect('&&RAPO2D.DIME', 'V V I', nbterm, jlisdm)
!
! --- -----------------------------------------------------------------
! --- RELATIONS ENTRE LES NOEUDS DU BORD ET LE NOEUD POUTRE
!
! --- PREMIER GROUPE DE RELATIONS TRADUISANT :
!        SOMME/S_RACCORD(U_3D) = S_RACCORD*U_NOEUD_POUTRE
!
! --- -----------------------------------------------------------------
! --- PREMIERE RELATION :
!     -S.DX(NOEUD_POUTRE) + (SOMME/S_RACCORD(NI.DS)).DX(NOEUD_I) = 0
    nbterm = lonlis + 1
!     BOUCLE SUR LES NOEUDS DES MAILLES DE LA TRACE DE LA POUTRE
    do i = 1, lonlis
        call jenonu(jexnom(noeuma, zk8(ilisno+i-1)), ino)
!        ADRESSE DE LA PREMIERE COMPOSANTE DU NOEUD INO DANS LES CHAMNO
        ival = zi(iaprno + (ino-1)*(nbec+2))
!
        zk8(jlisno+i-1) = zk8(ilisno+i-1)
        zk8(jlisdl+i-1) = 'DX'
        zr(jliscr+i-1) = zr(idch1+ival-1)
    end do
!
    zk8(jlisno+lonlis+1-1) = noepou
    zk8(jlisdl+lonlis+1-1) = 'DX'
    zr(jliscr+lonlis+1-1) = -s
!
    call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                zr(jlisdi), nbterm, beta, betac, betaf,&
                typcoe, typval, typlag, 0.d0, lisrel)
    call imprel(motfac, nbterm, zr(jliscr), zk8(jlisdl), zk8(jlisno),&
                beta)
!
! --- DEUXIEME RELATION :
!     -S.DY(NOEUD_POUTRE) + (SOMME/S_RACCORD(NI.DS)).DY(NOEUD_I) = 0
    nbterm = lonlis + 1
!     BOUCLE SUR LES NOEUDS DES MAILLES DE LA TRACE DE LA POUTRE
    do i = 1, lonlis
        call jenonu(jexnom(noeuma, zk8(ilisno+i-1)), ino)
!        ADRESSE DE LA PREMIERE COMPOSANTE DU NOEUD INO DANS LES CHAMNO
        ival = zi(iaprno + (ino-1)*(nbec+2))
!
        zk8(jlisno+i-1) = zk8(ilisno+i-1)
        zk8(jlisdl+i-1) = 'DY'
        zr(jliscr+i-1) = zr(idch1+ival-1)
    end do
!
    zk8(jlisno+lonlis+1-1) = noepou
    zk8(jlisdl+lonlis+1-1) = 'DY'
    zr(jliscr+lonlis+1-1) = -s
!
    call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                zr(jlisdi), nbterm, beta, betac, betaf,&
                typcoe, typval, typlag, 0.d0, lisrel)
    call imprel(motfac, nbterm, zr(jliscr), zk8(jlisdl), zk8(jlisno),&
                beta)
!
!
! --- -----------------------------------------------------------------
! --- DEUXIEME GROUPE DE RELATIONS TRADUISANT :
!        SOMME/S_RACCORD(GM X U_3D) = I.OMEGA(NOEUD_POUTRE)
!
!
! --- TOISIEME RELATION :
!        (SOMME/S_RACCORD(X*NI.DS)).DY(NOEUD_I) -
!        (SOMME/S_RACCORD(Y*NI.DS)).DX(NOEUD_I) -
!        IZZ.DRZ(NOEUD_POUTRE)                            = 0
    nbterm = 2*lonlis + 1
!     BOUCLE SUR LES NOEUDS DES MAILLES DE SURFACE DU MASSIF
    do i = 1, lonlis
        call jenonu(jexnom(noeuma, zk8(ilisno+i-1)), ino)
!        ADRESSE DE LA PREMIERE COMPOSANTE DU NOEUD INO DANS LES CHAMNO
        ival = zi(iaprno+ (ino-1)* (nbec+2))
!
        zk8(jlisno+2*(i-1) ) = zk8(ilisno+i-1)
        zk8(jlisno+2*(i-1)+1) = zk8(ilisno+i-1)
        zk8(jlisdl+2*(i-1) ) = 'DY'
        zk8(jlisdl+2*(i-1)+1) = 'DX'
        zr(jliscr+2*(i-1) ) = zr(idch2+ival-1)
        zr(jliscr+2*(i-1)+1) = -zr(idch2+ival-1+1)
    end do
!
    zk8(jlisno+2*lonlis+1-1) = noepou
    zk8(jlisdl+2*lonlis+1-1) = 'DRZ'
    zr(jliscr+2*lonlis+1-1) = -igzz
!
    call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                zr(jlisdi), nbterm, beta, betac, betaf,&
                typcoe, typval, typlag, 0.d0, lisrel)
    call imprel(motfac, nbterm, zr(jliscr), zk8(jlisdl), zk8(jlisno),&
                beta)
!
!
! --- -----------------------------------------------------------------
! --- DESTRUCTION DES OBJETS DE TRAVAIL
!
    call jedetr('&&RAPO2D.LISTE_NOEUDS')
    call jedetr('&&RAPO2D.LISTE_MAILLES')
    call detrsd('CHAMP_GD', '&&RAPO2D.PSECT')
    call jedetr('&&RAPO2D.INERTIE_RACCORD')
    call detrsd('CARTE', '&&RAPO2D.CAORIGE')
    call detrsd('RESUELEM', '&&RAPO2D.VECT_NI')
    call detrsd('RESUELEM', '&&RAPO2D.VECT_XYZNI')
    call jedetr('&&RAPO2D           .RELR')
    call jedetr('&&RAPO2D           .RERR')
    call jedetr('&&RAPO2D.LISNO')
    call jedetr('&&RAPO2D.LISDDL')
    call jedetr('&&RAPO2D.COER')
    call jedetr('&&RAPO2D.COEC')
    call jedetr('&&RAPO2D.DIRECT')
    call jedetr('&&RAPO2D.DIME')
    call detrsd('LIGREL', ligrel)
    call detrsd('CHAMP_GD', 'CH_DEPL_1')
    call detrsd('CHAMP_GD', 'CH_DEPL_2')
!
999 continue
    call jedema()
end subroutine
