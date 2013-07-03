subroutine rapoco(numdlz, iocc, fonrez, lisrez, chargz)
! aslint: disable=W1501
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!    ATTENTION CETTE PROGRAMMATION SUPPOSE QUE L'OBJET NUEQ EST UN
!    VECTEUR IDENTITE. A MODIFIER
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/indik8.h"
#include "asterc/r8prem.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/assvec.h"
#include "asterfort/calcul.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/exlim1.h"
#include "asterfort/getvem.h"
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
#include "asterfort/racotu.h"
#include "asterfort/reajre.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: iocc
    character(len=8) :: charge
    character(len=14) :: numddl
    character(len=19) :: lisrel
    character(len=*) :: numdlz, chargz, fonrez, lisrez
! -------------------------------------------------------
!     RACCORD POUTRE-COQUE PAR DES RELATIONS LINEAIRES
!     ENTRE LES NOEUDS DES MAILLES DE BORD MODELISANT
!     LA TRACE DE LA SECTION DE LA POUTRE SUR LA COQUE
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
    character(len=1) :: k1bid
    character(len=2) :: typlag
    character(len=4) :: typval, typcoe
    character(len=8) :: betaf, mod, nomg, k8bid, poslag, cara
    character(len=8) :: noma, nomcmp(nmocl), option
    character(len=8) :: noepou, nocmp(3), kcmp(3), cmp(6)
    character(len=8) :: lpain(4), lpaout(2)
    character(len=9) :: nomte
    character(len=16) :: motfac, motcle(2), typmcl(2)
    character(len=19) :: ligrmo, ligrel
    character(len=24) :: lchin(4), lchout(2), nolili, lismai, valk(2)
    character(len=24) :: lisnoe, noeuma, vale1, grnoma, vale2, nogrno
    integer :: ntypel(nmocl), dg, icmp(6), niv, ifm, iop, numnop, nliai, nrl
    integer :: vali(2), nlili, nbterm, ncara, nddla, nbma, nbno, nno, nbec
    integer :: nbcmp
    integer :: narl, naxe, lonlis, k, j, in, ino, ier, idiner, ibid, i, ival, n1
    integer :: nbgno
    integer :: jnoma, jprnm, jlisma, jlisdm, jlisno, jliscr, jlisdi, jgro, jcoor
    integer :: jliscc, iaprno, idch2, idch1, ilisno, inom, jlisdl
    real(kind=8) :: ig(6), coorig(3), axepou(3), valr(9)
    real(kind=8) :: ayz, axx, ax, ay, axz, axy, ayy, azz, az, beta, dnorme, eps
    real(kind=8) :: un
    real(kind=8) :: xg, yg, zg, xpou, ypou, zpou, xnorm, s1, s
    complex(kind=8) :: cbid, betac, ccmp(3)
    integer :: iarg
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
    call getvtx(motfac, 'OPTION', iocc, iarg, 1,&
                option, iop)
    if ((option.ne.'COQ_POU') .and. (option.ne.'COQ_TUYA')) then
        call u2mesk('F', 'MODELISA6_39', 1, option)
    endif
!
    call getfac(motfac, nliai)
    if (nliai .eq. 0) goto 130
!
! --- INITIALISATIONS :
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
    eps = 1.d-2
    un = 1.0d0
    kcmp(1) = ' '
    kcmp(2) = ' '
    kcmp(3) = ' '
    cmp(1) = 'DX'
    cmp(2) = 'DY'
    cmp(3) = 'DZ'
    cmp(4) = 'DRX'
    cmp(5) = 'DRY'
    cmp(6) = 'DRZ'
    ccmp(1) = (0.0d0,0.0d0)
    ccmp(2) = (0.0d0,0.0d0)
    ccmp(3) = (0.0d0,0.0d0)
    do 10 i = 1, 6
        icmp(i) = 0
10  end do
!
    ligrel = '&&RAPOCO'
    lisnoe = '&&RAPOCO.LISTE_NOEUDS'
    lismai = '&&RAPOCO.LISTE_MAILLES'
    motcle(1) = 'GROUP_MA_1'
    motcle(2) = 'MAILLE_1'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
! --- ON REGARDE SI LES MULTIPLICATEURS DE LAGRANGE SONT A METTRE
! --- APRES LES NOEUDS PHYSIQUES LIES PAR LA RELATION DANS LA MATRICE
! --- ASSEMBLEE :
! --- SI OUI TYPLAG = '22'
! --- SI NON TYPLAG = '12' :
!     -------------------
!
    call getvtx(motfac, 'NUME_LAGR', iocc, iarg, 0,&
                k8bid, narl)
    if (narl .ne. 0) then
        call getvtx(motfac, 'NUME_LAGR', iocc, iarg, 1,&
                    poslag, nrl)
        if (poslag(1:5) .eq. 'APRES') then
            typlag = '22'
        else
            typlag = '12'
        endif
    else
        typlag = '12'
    endif
!
! --- MODELE ASSOCIE AU LIGREL DE CHARGE :
!     ----------------------------------
    call dismoi('F', 'NOM_MODELE', charge(1:8), 'CHARGE', ibid,&
                mod, ier)
!
! ---  LIGREL DU MODELE :
!      ----------------
    ligrmo = mod(1:8)//'.MODELE'
!
! --- MAILLAGE ASSOCIE AU MODELE :
!     --------------------------
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
!
    noeuma = noma//'.NOMNOE'
    grnoma = noma//'.GROUPENO'
!
! --- RECUPERATION DU TABLEAU DES COORDONNEES :
!     ---------------------------------------
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
! --- RECUPERATION DES NOMS DES DDLS :
!     ------------------------------
    nomg = 'DEPL_R'
    nomte = 'D_DEPL_R_'
!
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', inom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp, k1bid)
    nddla = nbcmp - 1
    if (nddla .gt. nmocl) then
        vali (1) = nmocl
        vali (2) = nddla
        call u2mesg('F', 'MODELISA8_29', 0, ' ', 2,&
                    vali, 0, 0.d0)
    endif
    do 20 i = 1, nddla
        nomcmp(i) = zk8(inom-1+i)
        call jenonu(jexnom('&CATA.TE.NOMTE', nomte//nomcmp(i) (1:7)), ntypel(i))
20  end do
    call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                k8bid, ier)
!
! --- ACCES A L'OBJET .PRNM :
!     ----------------------
    if (nbec .gt. 10) then
        call u2mess('F', 'MODELISA_94')
    else
        call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
    endif
!
! --- RECUPERATION DU .PRNO ASSOCIE AU MAILLAGE :
!     -----------------------------------------
    call jelira(numddl//'.NUME.PRNO', 'NMAXOC', nlili, k1bid)
!
    k = 0
    do 30 i = 1, nlili
        call jenuno(jexnum(numddl//'.NUME.LILI', i), nolili)
        if (nolili(1:8) .ne. '&MAILLA') goto 30
        k = i
30  end do
!
    call assert(k.ne.0)
!
    call jeveuo(jexnum(numddl//'.NUME.PRNO', k), 'L', iaprno)
!
! --- ACQUISITION DE LA LISTE DES NOEUDS A LIER
! --- (CETTE LISTE EST NON REDONDANTE) :
!     -------------------------------
    call malin1(motfac, charge, iocc, 1, lisnoe,&
                lonlis)
    call jeveuo(lisnoe, 'L', ilisno)
!
! --- CONSTITUTION DU LIGREL FORME DES MAILLES DE BORD MODELISANT
! --- LA TRACE DE LA SECTION DE LA POUTRE SUR LA COQUE :
!     ------------------------------------------------
!
! --- CREATION ET AFFECTATION DU VECTEUR DE K8 DE NOM LISMAI
! --- CONTENANT LES NOMS DES MAILLES FORMANT LE LIGREL A CREER :
!     --------------------------------------------------------
    call reliem(' ', noma, 'NU_MAILLE', motfac, iocc,&
                2, motcle(1), typmcl(1), lismai, nbma)
    call jeveuo(lismai, 'L', jlisma)
!
! ---   CREATION ET AFFECTATION DU LIGREL
    call exlim1(zi(jlisma), nbma, mod, 'V', ligrel)
!
! --- ACQUISITION DES MOTS-CLES NOEUD_2 OU GROUP_NO_2 :
!     -----------------------------------------------
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
            valk(1) = motfac
            valk(2) = option
            call u2mesk('F', 'MODELISA6_48', 2, valk)
        endif
    endif
!
    if (nbno .ne. 0) then
        nbno = -nbno
        if (nbno .ne. 1) then
            call u2mess('F', 'MODELISA6_49')
        endif
        call getvem(noma, 'NOEUD', motfac, 'NOEUD_2', iocc,&
                    iarg, nbno, noepou, nno)
    endif
!
    if (nbgno .ne. 0) then
        nbgno = -nbgno
        if (nbgno .ne. 1) then
            call u2mess('F', 'MODELISA6_50')
        endif
        call getvem(noma, 'GROUP_NO', motfac, 'GROUP_NO_2', iocc,&
                    iarg, nbgno, nogrno, nno)
        call jelira(jexnom(grnoma, nogrno), 'LONUTI', n1, k1bid)
        if (n1 .ne. 1) then
            call u2mesk('F', 'MODELISA6_43', 1, nogrno)
        else
            call jeveuo(jexnom(grnoma, nogrno), 'L', jgro)
            in = zi(jgro+1-1)
            call jenuno(jexnum(noeuma, in), noepou)
        endif
    endif
!
! --- RECUPERATION DU VECTEUR ORIENTANT LA POUTRE ET DIRIGE
! --- DE LA PARTIE COQUE VERS LA PARTIE POUTRE :
!     ----------------------------------------
    call getvr8(motfac, 'AXE_POUTRE', iocc, iarg, 3,&
                axepou, naxe)
    if (naxe .eq. 0) then
        call u2mess('F', 'MODELISA6_51')
    endif
!
    xnorm = sqrt(axepou(1)*axepou(1)+axepou(2)*axepou(2)+ axepou(3)*axepou(3))
!
    if (xnorm .le. r8prem()) then
        call u2mess('F', 'MODELISA6_52')
    endif
!
    axepou(1) = axepou(1)/xnorm
    axepou(2) = axepou(2)/xnorm
    axepou(3) = axepou(3)/xnorm
!
! --- NOTATION DANS LA CARTE DE NOM '&&RAPOCO.CAXE_POU' DES
! --- COORDONNEES DU VECTEUR UNITAIRE ORIENTANT LA POUTRE :
!     ---------------------------------------------------
!
    nocmp(1) = 'X'
    nocmp(2) = 'Y'
    nocmp(3) = 'Z'
!
    call mecact('V', '&&RAPOCO.CAXE_POU', 'LIGREL', ligrel, 'GEOM_R',&
                3, nocmp, icmp, axepou, ccmp,&
                kcmp)
!
! --- RECUPERATION DES CARACTERISTIQUES ELEMENTAIRES :
!     ----------------------------------------------
    call getvid(motfac, 'CARA_ELEM', iocc, iarg, 1,&
                cara, ncara)
    if (ncara .eq. 0) then
        call u2mess('F', 'MODELISA6_53')
    endif
!
! ---  NUMERO DU NOEUD POUTRE A LIER :
!      -----------------------------
    call jenonu(jexnom(noeuma, noepou), numnop)
!
! ---  COORDONNEES DU NOEUD POUTRE :
!      ---------------------------
    xpou = zr(jcoor-1+3* (numnop-1)+1)
    ypou = zr(jcoor-1+3* (numnop-1)+2)
    zpou = zr(jcoor-1+3* (numnop-1)+3)
!
! --- VERIFICATION DU FAIT QUE LES NOEUDS DE LISNOE (DONC
! --- APPARTENANT A LA COQUE)  PORTENT LES DDLS DE ROTATION :
!     -----------------------------------------------------
    do 50 i = 1, lonlis
! ---     NUMERO DU NOEUD COURANT DE LA LISTE
        call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+i-1)), ino)
!
        dg = zi(jprnm-1+ (ino-1)*nbec+1)
        do 40 j = 4, 6
            icmp(j) = indik8(nomcmp,cmp(j),1,nddla)
            if (.not.exisdg(dg,icmp(j))) then
                valk(1) = zk8(ilisno+i-1)
                valk(2) = cmp(j)
                call u2mesk('F', 'MODELISA6_54', 2, valk)
            endif
40      continue
50  end do
!
! --- VERIFICATION DU FAIT QUE LE NOEUD POUTRE A RACCORDER PORTE
! --- LES 3 DDLS DE TRANSLATION ET LES 3 DDLS DE ROTATION :
!     ---------------------------------------------------
    dg = zi(jprnm-1+ (numnop-1)*nbec+1)
    do 60 j = 1, 6
        icmp(j) = indik8(nomcmp,cmp(j),1,nddla)
        if (.not.exisdg(dg,icmp(j))) then
            valk(1) = noepou
            valk(2) = cmp(j)
            call u2mesk('F', 'MODELISA6_45', 2, valk)
        endif
60  end do
!
! --- CALCUL SUR CHAQUE ELEMENT DE BORD A RELIER A LA POUTRE
! --- DES CARACTERISTIQUES GEOMETRIQUES SUIVANTES :
! ---  SOMME/S_ELEMENT(1,X,Y,Z,X*X,Y*Y,Z*Z,X*Y,X*Z,Y*Z)DS
!     ---------------------------------------------------
    lpain(1) = 'PGEOMER'
    lchin(1) = noma//'.COORDO'
    lpain(2) = 'PCACOQU'
    lchin(2) = cara//'.CARCOQUE'
    lpain(3) = 'PCAORIE'
    lchin(3) = '&&RAPOCO.CAXE_POU'
    lpaout(1) = 'PCASECT'
    lchout(1) = '&&RAPOCO.PSECT'
!
    call calcul('S', 'CARA_SECT_POUT3', ligrel, 3, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
! --- VECTEUR DES QUANTITES GEOMETRIQUES PRECITEES SOMMEES
! --- SUR LA SURFACE DE RACCORD, CES QUANTITES SERONT NOTEES :
! ---  A1 = S,AX,AY,AZ,AXX,AYY,AZZ,AXY,AXZ,AYZ
!     ----------------------------------------
    call wkvect('&&RAPOCO.INERTIE_RACCORD', 'V V R', 10, idiner)
!
! --- SOMMATION DES QUANTITES GEOMETRIQUES ELEMENTAIRES
! --- DANS LE VECTEUR &&RAPOCO.INERTIE_RACCORD :
!     ----------------------------------------
    call mesomm(lchout(1), 10, ibid, zr(idiner), cbid,&
                0, ibid)
!
    s = zr(idiner+1-1)
    ax = zr(idiner+2-1)
    ay = zr(idiner+3-1)
    az = zr(idiner+4-1)
    axx = zr(idiner+5-1)
    ayy = zr(idiner+6-1)
    azz = zr(idiner+7-1)
    axy = zr(idiner+8-1)
    axz = zr(idiner+9-1)
    ayz = zr(idiner+10-1)
!
    if (abs(s) .lt. r8prem()) then
        call u2mess('F', 'MODELISA6_55')
    endif
    s1 = 1.0d0/s
!
! --- COORDONNEES DU CENTRE GEOMETRIQUE G DE LA SECTION DE RACCORD
! --- XG = AX/S, YG = AY/S, ZG = AZ/S :
!     -------------------------------
    xg = s1*ax
    yg = s1*ay
    zg = s1*az
! --- VERIFICATION DE L'IDENTITE GEOMETRIQUE DE G AVEC LE
! --- NOEUD POUTRE A RACCORDER :
!     ------------------------
    dnorme = sqrt((xpou-xg)* (xpou-xg)+ (ypou-yg)* (ypou-yg)+ (zpou-zg)* (zpou-zg)) &
           / sqrt(s/3.14159265d0)
    if (dnorme .gt. eps) then
        valr(1) = xg
        valr(2) = yg
        valr(3) = zg
        valr(4) = xpou
        valr(5) = ypou
        valr(6) = zpou
        valr(7) = eps*100.0d0
        valr(8) = sqrt(s/3.14159265d0)
        valr(9) = dnorme
        valk(1) = option
        vali(1) = iocc
        call u2mesg('A', 'CALCULEL3_80', 1, valk, 1,&
                    vali, 9, valr)
    endif
!
! --- CALCUL DU TENSEUR D'INERTIE EN G, CE TENSEUR EST SYMETRIQUE :
! --- ON CALCULE LES COMPOSANTES DE LA PARTIE SUPERIEURE PAR LIGNE
!     ------------------------------------------------------------
!
! ---    IGXX = AYY + AZZ -S*(YG*YG+ZG*ZG)
    ig(1) = ayy + azz - s* (yg*yg+zg*zg)
! ---    IGXY = -AXY + S*XG*YG
    ig(2) = -axy + s*xg*yg
! ---    IGXZ = -AXZ + S*XG*ZG
    ig(3) = -axz + s*xg*zg
! ---    IGYY = AZZ + AXX -S*(ZG*ZG+XG*XG)
    ig(4) = azz + axx - s* (zg*zg+xg*xg)
! ---    IGYZ = -AYZ + S*YG*ZG
    ig(5) = -ayz + s*yg*zg
! ---    IGZZ = AXX + AYY -S*(XG*XG+YG*YG)
    ig(6) = axx + ayy - s* (xg*xg+yg*yg)
!
! --- NOTATION DANS LA CARTE DE NOM '&&RAPOCO.CAORIGE' DES
! --- COORDONNEES DU CENTRE GEOMETRIQUE G DE LA SECTION DE RACCORD
!     ------------------------------------------------------------
!
    nocmp(1) = 'X'
    nocmp(2) = 'Y'
    nocmp(3) = 'Z'
!
    coorig(1) = xg
    coorig(2) = yg
    coorig(3) = zg
!
    call mecact('V', '&&RAPOCO.CAORIGE', 'LIGREL', ligrel, 'GEOM_R',&
                3, nocmp, icmp, coorig, ccmp,&
                kcmp)
!
! --- DETERMINATION DE 2 LISTES  DE VECTEURS PAR ELEMENT PRENANT
! --- LEURS VALEURS AUX NOEUDS DES ELEMENTS.
! --- LA PREMIERE LISTE DE NOM 'VECT_EINI' A POUR VALEURS AU NOEUD
! --- I D'UN ELEMENT :
! ---  SOMME/S_ELEMENT(E1(1)*NI,E1(2)*NI,E1(3)*NI,
! ---                  E2(1)*NI,E2(2)*NI,E2(3)*NI)DS
! --- OU E1 EST UN VECTEUR UNITAIRE PERPENDICULAIRE A L'ELEMENT
! --- DE BORD ORIENTE DE LA COQUE VERS LA POUTRE ET
! --- E2 EST LE VECTEUR TANGENT A LA FIBRE MOYENNE DE L'ELEMENT DE BORD
! --- LA SECONDE LISTE DE NOM 'VECT_XYZNI' A POUR VALEURS AU NOEUD
! --- I D'UN ELEMENT :
! ---  SOMME/S_ELEMENT(X*NI,Y*NI,Z*NI,NI,0,0)DS
! ---  AVEC X = XM - XG = NJ*XJ - XG
! ---       Y = YM - YG = NJ*YJ - YG
! ---       Z = ZM - ZG = NJ*ZJ - ZG
!     ------------------------------
    lpain(1) = 'PGEOMER'
    lchin(1) = noma//'.COORDO'
    lpain(2) = 'PORIGIN'
    lchin(2) = '&&RAPOCO.CAORIGE'
    lpain(3) = 'PCACOQU'
    lchin(3) = cara//'.CARCOQUE'
    lpain(4) = 'PCAORIE'
    lchin(4) = '&&RAPOCO.CAXE_POU'
    lpaout(1) = 'PVECTU1'
    lpaout(2) = 'PVECTU2'
    lchout(1) = '&&RAPOCO.VECT_XYZNI'
    lchout(2) = '&&RAPOCO.VECT2'
!
    call calcul('S', 'CARA_SECT_POUT4', ligrel, 4, lchin,&
                lpain, 2, lchout, lpaout, 'V',&
                'OUI')
!
! --- CREATION DES .RERR DES VECTEURS EN SORTIE DE CALCUL
!     --------------------------------------------------------
!
    call memare('V', '&&RAPOCO', mod, ' ', ' ',&
                'CHAR_MECA')
!
! --- ASSEMBLAGE DE LCHOUT(1) DANS LE CHAMNO DE NOM 'CH_DEPL_1'
!     ---------------------------------------------------------
    call jedetr('&&RAPOCO           .RELR')
    call reajre('&&RAPOCO', lchout(1), 'V')
    call assvec('V', '&&RAPOCO.CH_DEPL_01', 1, '&&RAPOCO           .RELR', un,&
                numddl, ' ', 'ZERO', 1)
!
    vale1 = '&&RAPOCO.CH_DEPL_01.VALE'
    call jeveuo(vale1, 'L', idch1)
!
!
! --- ASSEMBLAGE DE LCHOUT(2) DANS LE CHAMNO DE NOM 'CH_DEPL_1'
!     ---------------------------------------------------------
    call jedetr('&&RAPOCO           .RELR')
    call reajre('&&RAPOCO', lchout(2), 'V')
!
    call assvec('V', '&&RAPOCO.CH_DEPL_02', 1, '&&RAPOCO           .RELR', un,&
                numddl, ' ', 'ZERO', 1)
!
    vale2 = '&&RAPOCO.CH_DEPL_02.VALE'
    call jeveuo(vale2, 'L', idch2)
!
! --- CREATION DES TABLEAUX DE TRAVAIL NECESSAIRES A L'AFFECTATION
! --- DE LISREL
!     ------------------------------------------------------------
!
! ---     MAJORANT DU NOMBRE DE TERMES DANS UNE RELATION
    nbterm = 5*lonlis + 3
! ---     VECTEUR DU NOM DES NOEUDS
    call wkvect('&&RAPOCO.LISNO', 'V V K8', nbterm, jlisno)
! ---     VECTEUR DU NOM DES DDLS
    call wkvect('&&RAPOCO.LISDDL', 'V V K8', nbterm, jlisdl)
! ---     VECTEUR DES COEFFICIENTS REELS
    call wkvect('&&RAPOCO.COER', 'V V R', nbterm, jliscr)
! ---     VECTEUR DES COEFFICIENTS COMPLEXES
    call wkvect('&&RAPOCO.COEC', 'V V C', nbterm, jliscc)
! ---     VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
    call wkvect('&&RAPOCO.DIRECT', 'V V R', 6*nbterm, jlisdi)
! ---     VECTEUR DES DIMENSIONS DE CES DIRECTIONS
    call wkvect('&&RAPOCO.DIME', 'V V I', nbterm, jlisdm)
!
! ---    RELATIONS ENTRE LES NOEUDS DU MASSIF ET LE NOEUD POUTRE
!        -------------------------------------------------------
! ---    PREMIER GROUPE DE RELATIONS TRADUISANT :
! ---      SOMME/S_RACCORD(U_COQUE) = S_RACCORD*U_NOEUD_POUTRE
!         ----------------------------------------------------
!
! ---    PREMIERE RELATION :
!     -S.DX(NOEUD_POUTRE) + (SOMME/S_RACCORD(NI.DS)).DX(NOEUD_I) = 0
!     --------------------------------------------------------------
!
    nbterm = lonlis + 1
! ---    BOUCLE SUR LES NOEUDS DES MAILLES DE BORD DE LA PARTIE COQUE
    do 70 i = 1, lonlis
        call jenonu(jexnom(noeuma, zk8(ilisno+i-1)), ino)
! ---    ADRESSE DE LA PREMIERE COMPOSANTE DU NOEUD INO DANS LES
! ---    CHAMNO (SOMME/S_RACCORD(NI.DS))
        ival = zi(iaprno+ (ino-1)* (nbec+2)+1-1) - 1
!
        zk8(jlisno+i-1) = zk8(ilisno+i-1)
        zk8(jlisdl+i-1) = 'DX'
        zr(jliscr+i-1) = zr(idch1-1+ival+4)
70  end do
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
! ---    DEUXIEME RELATION :
!     -S.DY(NOEUD_POUTRE) + (SOMME/S_RACCORD(NI.DS)).DY(NOEUD_I) = 0
!     --------------------------------------------------------------
!
    nbterm = lonlis + 1
! ---   BOUCLE SUR LES NOEUDS DES MAILLES DE BORD DE LA PARTIE COQUE
    do 80 i = 1, lonlis
        call jenonu(jexnom(noeuma, zk8(ilisno+i-1)), ino)
! ---    ADRESSE DE LA PREMIERE COMPOSANTE DU NOEUD INO DANS LES
! ---    CHAMNO (SOMME/S_RACCORD(NI.DS))
        ival = zi(iaprno+ (ino-1)* (nbec+2)+1-1) - 1
!
        zk8(jlisno+i-1) = zk8(ilisno+i-1)
        zk8(jlisdl+i-1) = 'DY'
        zr(jliscr+i-1) = zr(idch1-1+ival+4)
80  end do
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
! ---    TROISIEME RELATION :
!     -S.DZ(NOEUD_POUTRE) + (SOMME/S_RACCORD(NI.DS)).DZ(NOEUD_I) = 0
!     --------------------------------------------------------------
!
    nbterm = lonlis + 1
! --- BOUCLE SUR LES NOEUDS DES MAILLES DE BORD DE LA PARTIE COQUE
    do 90 i = 1, lonlis
        call jenonu(jexnom(noeuma, zk8(ilisno+i-1)), ino)
! ---    ADRESSE DE LA PREMIERE COMPOSANTE DU NOEUD INO DANS LES
! ---    CHAMNO (SOMME/S_RACCORD(NI.DS))
        ival = zi(iaprno+ (ino-1)* (nbec+2)+1-1) - 1
!
        zk8(jlisno+i-1) = zk8(ilisno+i-1)
        zk8(jlisdl+i-1) = 'DZ'
        zr(jliscr+i-1) = zr(idch1-1+ival+4)
90  end do
!
    zk8(jlisno+lonlis+1-1) = noepou
    zk8(jlisdl+lonlis+1-1) = 'DZ'
    zr(jliscr+lonlis+1-1) = -s
!
    call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                zr(jlisdi), nbterm, beta, betac, betaf,&
                typcoe, typval, typlag, 0.d0, lisrel)
    call imprel(motfac, nbterm, zr(jliscr), zk8(jlisdl), zk8(jlisno),&
                beta)
!
! ---    DEUXIEME GROUPE DE RELATIONS TRADUISANT :
! ---      SOMME/S_RACCORD(GM X U_COQUE) = I.OMEGA(NOEUD_POUTRE)
!         ------------------------------------------------------
! ---    QUATRIEME RELATION :
!
    nbterm = 5*lonlis + 3
! ---    BOUCLE SUR LES NOEUDS DES MAILLES DE BORD DE LA PARTIE COQUE
    do 100 i = 1, lonlis
        call jenonu(jexnom(noeuma, zk8(ilisno+i-1)), ino)
! ---    ADRESSE DE LA PREMIERE COMPOSANTE DU NOEUD INO DANS LES CHAMNO
        ival = zi(iaprno+ (ino-1)* (nbec+2)+1-1) - 1
!
        zk8(jlisno+5* (i-1)+1-1) = zk8(ilisno+i-1)
        zk8(jlisno+5* (i-1)+2-1) = zk8(ilisno+i-1)
        zk8(jlisno+5* (i-1)+3-1) = zk8(ilisno+i-1)
        zk8(jlisno+5* (i-1)+4-1) = zk8(ilisno+i-1)
        zk8(jlisno+5* (i-1)+5-1) = zk8(ilisno+i-1)
!
        zk8(jlisdl+5* (i-1)+1-1) = 'DZ'
        zk8(jlisdl+5* (i-1)+2-1) = 'DY'
        zk8(jlisdl+5* (i-1)+3-1) = 'DRX'
        zk8(jlisdl+5* (i-1)+4-1) = 'DRY'
        zk8(jlisdl+5* (i-1)+5-1) = 'DRZ'
!
        zr(jliscr+5* (i-1)+1-1) = zr(idch1-1+ival+2)
        zr(jliscr+5* (i-1)+2-1) = -zr(idch1-1+ival+3)
        zr(jliscr+5* (i-1)+3-1) = zr(idch2-1+ival+1)
        zr(jliscr+5* (i-1)+4-1) = zr(idch2-1+ival+2)
        zr(jliscr+5* (i-1)+5-1) = zr(idch2-1+ival+3)
100  end do
!
    zk8(jlisno+5*lonlis+1-1) = noepou
    zk8(jlisno+5*lonlis+2-1) = noepou
    zk8(jlisno+5*lonlis+3-1) = noepou
!
    zk8(jlisdl+5*lonlis+1-1) = 'DRX'
    zk8(jlisdl+5*lonlis+2-1) = 'DRY'
    zk8(jlisdl+5*lonlis+3-1) = 'DRZ'
!
    zr(jliscr+5*lonlis+1-1) = -ig(1)
    zr(jliscr+5*lonlis+2-1) = -ig(2)
    zr(jliscr+5*lonlis+3-1) = -ig(3)
!
    call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                zr(jlisdi), nbterm, beta, betac, betaf,&
                typcoe, typval, typlag, 0.d0, lisrel)
    call imprel(motfac, nbterm, zr(jliscr), zk8(jlisdl), zk8(jlisno),&
                beta)
!
! ---    CINQUIEME RELATION :
!
    nbterm = 5*lonlis + 3
! ---    BOUCLE SUR LES NOEUDS DES MAILLES DE BORD DE LA PARTIE COQUE
    do 110 i = 1, lonlis
        call jenonu(jexnom(noeuma, zk8(ilisno+i-1)), ino)
! ---    ADRESSE DE LA PREMIERE COMPOSANTE DU NOEUD INO DANS LES CHAMNO
        ival = zi(iaprno+ (ino-1)* (nbec+2)+1-1) - 1
!
        zk8(jlisno+5* (i-1)+1-1) = zk8(ilisno+i-1)
        zk8(jlisno+5* (i-1)+2-1) = zk8(ilisno+i-1)
        zk8(jlisno+5* (i-1)+3-1) = zk8(ilisno+i-1)
        zk8(jlisno+5* (i-1)+4-1) = zk8(ilisno+i-1)
        zk8(jlisno+5* (i-1)+5-1) = zk8(ilisno+i-1)
!
        zk8(jlisdl+5* (i-1)+1-1) = 'DX'
        zk8(jlisdl+5* (i-1)+2-1) = 'DZ'
        zk8(jlisdl+5* (i-1)+3-1) = 'DRX'
        zk8(jlisdl+5* (i-1)+4-1) = 'DRY'
        zk8(jlisdl+5* (i-1)+5-1) = 'DRZ'
!
        zr(jliscr+5* (i-1)+1-1) = zr(idch1-1+ival+3)
        zr(jliscr+5* (i-1)+2-1) = -zr(idch1-1+ival+1)
        zr(jliscr+5* (i-1)+3-1) = zr(idch2-1+ival+2)
        zr(jliscr+5* (i-1)+4-1) = zr(idch2-1+ival+4)
        zr(jliscr+5* (i-1)+5-1) = zr(idch2-1+ival+5)
110  end do
!
    zk8(jlisno+5*lonlis+1-1) = noepou
    zk8(jlisno+5*lonlis+2-1) = noepou
    zk8(jlisno+5*lonlis+3-1) = noepou
!
    zk8(jlisdl+5*lonlis+1-1) = 'DRX'
    zk8(jlisdl+5*lonlis+2-1) = 'DRY'
    zk8(jlisdl+5*lonlis+3-1) = 'DRZ'
!
    zr(jliscr+5*lonlis+1-1) = -ig(2)
    zr(jliscr+5*lonlis+2-1) = -ig(4)
    zr(jliscr+5*lonlis+3-1) = -ig(5)
!
    call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                zr(jlisdi), nbterm, beta, betac, betaf,&
                typcoe, typval, typlag, 0.d0, lisrel)
    call imprel(motfac, nbterm, zr(jliscr), zk8(jlisdl), zk8(jlisno),&
                beta)
!
! ---    SIXIEME RELATION :
!
    nbterm = 5*lonlis + 3
! ---    BOUCLE SUR LES NOEUDS DES MAILLES DE BORD DE LA PARTIE COQUE
    do 120 i = 1, lonlis
        call jenonu(jexnom(noeuma, zk8(ilisno+i-1)), ino)
! ---    ADRESSE DE LA PREMIERE COMPOSANTE DU NOEUD INO DANS LES CHAMNO
        ival = zi(iaprno+ (ino-1)* (nbec+2)+1-1) - 1
!
        zk8(jlisno+5* (i-1)+1-1) = zk8(ilisno+i-1)
        zk8(jlisno+5* (i-1)+2-1) = zk8(ilisno+i-1)
        zk8(jlisno+5* (i-1)+3-1) = zk8(ilisno+i-1)
        zk8(jlisno+5* (i-1)+4-1) = zk8(ilisno+i-1)
        zk8(jlisno+5* (i-1)+5-1) = zk8(ilisno+i-1)
!
        zk8(jlisdl+5* (i-1)+1-1) = 'DY'
        zk8(jlisdl+5* (i-1)+2-1) = 'DX'
        zk8(jlisdl+5* (i-1)+3-1) = 'DRX'
        zk8(jlisdl+5* (i-1)+4-1) = 'DRY'
        zk8(jlisdl+5* (i-1)+5-1) = 'DRZ'
!
        zr(jliscr+5* (i-1)+1-1) = zr(idch1-1+ival+1)
        zr(jliscr+5* (i-1)+2-1) = -zr(idch1-1+ival+2)
        zr(jliscr+5* (i-1)+3-1) = zr(idch2-1+ival+3)
        zr(jliscr+5* (i-1)+4-1) = zr(idch2-1+ival+5)
        zr(jliscr+5* (i-1)+5-1) = zr(idch2-1+ival+6)
120  end do
!
    zk8(jlisno+5*lonlis+1-1) = noepou
    zk8(jlisno+5*lonlis+2-1) = noepou
    zk8(jlisno+5*lonlis+3-1) = noepou
!
    zk8(jlisdl+5*lonlis+1-1) = 'DRX'
    zk8(jlisdl+5*lonlis+2-1) = 'DRY'
    zk8(jlisdl+5*lonlis+3-1) = 'DRZ'
!
    zr(jliscr+5*lonlis+1-1) = -ig(3)
    zr(jliscr+5*lonlis+2-1) = -ig(5)
    zr(jliscr+5*lonlis+3-1) = -ig(6)
!
    call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                zr(jlisdi), nbterm, beta, betac, betaf,&
                typcoe, typval, typlag, 0.d0, lisrel)
    call imprel(motfac, nbterm, zr(jliscr), zk8(jlisdl), zk8(jlisno),&
                beta)
!
    if ((option.eq.'COQ_TUYA')) then
        call racotu(zi(iaprno), lonlis, zk8(ilisno), noepou, noma,&
                    ligrel, mod, cara, numddl, typlag,&
                    lisrel, coorig)
    endif
!
! --- DESTRUCTION DES OBJETS DE TRAVAIL
!     ---------------------------------
    call detrsd('LIGREL', ligrel)
    call jedetr('&&RAPOCO.LISTE_NOEUDS')
    call jedetr('&&RAPOCO.LISTE_MAILLES')
    call detrsd('CARTE', '&&RAPOCO.CAXE_POU')
    call detrsd('CHAMP_GD', '&&RAPOCO.PSECT')
    call jedetr('&&RAPOCO.INERTIE_RACCORD')
    call detrsd('CARTE', '&&RAPOCO.CAORIGE')
    call detrsd('RESUELEM', '&&RAPOCO.VECT2')
    call detrsd('RESUELEM', '&&RAPOCO.VECT_XYZNI')
    call jedetr('&&RAPOCO           .RELR')
    call jedetr('&&RAPOCO           .RERR')
    call jedetr('&&RAPOCO.LISNO')
    call jedetr('&&RAPOCO.LISDDL')
    call jedetr('&&RAPOCO.COER')
    call jedetr('&&RAPOCO.COEC')
    call jedetr('&&RAPOCO.DIRECT')
    call jedetr('&&RAPOCO.DIME')
    call detrsd('CHAMP_GD', '&&RAPOCO.CH_DEPL_01')
    call detrsd('CHAMP_GD', '&&RAPOCO.CH_DEPL_02')
!
130  continue
    call jedema()
end subroutine
