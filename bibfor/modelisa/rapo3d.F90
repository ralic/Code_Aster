subroutine rapo3d(numdlz, iocc, fonrez, lisrez, chargz)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
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
#include "asterfort/reliem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
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
#include "asterfort/ratu3d.h"
#include "asterfort/reajre.h"
#include "asterfort/utmess.h"
#include "asterfort/veripl.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: charge
    character(len=14) :: numddl
    character(len=19) :: lisrel
    character(len=*) :: numdlz, chargz, fonrez, lisrez
! -------------------------------------------------------
!     RACCORD POUTRE-3D PAR DES RELATIONS LINEAIRES
!     ENTRE LES NOEUDS DES MAILLES DE SURFACE MODELISANT
!     LA TRACE DE LA SECTION DE LA POUTRE SUR LE MASSIF 3D
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
    aster_logical :: vexcen
    character(len=2) :: typlag
    character(len=4) :: typval, typcoe
    character(len=8) :: betaf, mod, nomg, k8bid, poslag, cara
    character(len=8) :: noma, nomcmp(nmocl)
    character(len=8) :: noepou, nocmp(3), kcmp(3), cmp(6)
    character(len=8) :: lpain(2), lpaout(2)
    character(len=9) :: nomte
    character(len=16) :: motfac, motcle(4), typmcl(4), option
    character(len=19) :: ligrmo, ligrel
    character(len=24) :: lchin(2), lchout(2), nolili, lismai, valk(2)
    character(len=24) :: lisnoe, noeuma, vale1, vale2, grnoma
    integer :: ntypel(nmocl), dg, icmp(6), niv, ifm, vali(2)
    integer :: iop, nliai, i, narl, nrl, ibid, inom
    integer :: nbcmp, nddla, nbec, nlili, k, iaprno, lonlis, ilisno
    integer :: jlisma, nbma, nbno, numnop
    integer :: ino, j, idch1, idch2, nbterm,jno2
    integer :: ival
    integer :: ncara, iocc
    real(kind=8) :: ig(6), coorig(3), angt, beta, eps, un, vtang(6)
    real(kind=8) :: xpou, ypou, zpou, s, s1, xg, yg, zg, dnorme
    real(kind=8) :: ax, ay, az, axx, ayy, azz, axy, axz, ayz, valr(9)
    complex(kind=8) :: betac, ccmp(3)
    complex(kind=8), pointer :: coec(:) => null()
    real(kind=8), pointer :: coer(:) => null()
    integer, pointer :: dime(:) => null()
    real(kind=8), pointer :: direct(:) => null()
    real(kind=8), pointer :: inertie_raccord(:) => null()
    character(len=8), pointer :: lisddl(:) => null()
    character(len=8), pointer :: lisno(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    character(len=8), pointer :: lgrf(:) => null()
    integer, pointer :: prnm(:) => null()
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
    if ((option.ne.'3D_POU') .and. (option.ne.'3D_TUYAU') .and.&
        (option.ne.'PLAQ_POUT_ORTH')) then
        call utmess('F', 'MODELISA6_39', sk=option)
    endif
!
    call getfac(motfac, nliai)
    if (nliai .eq. 0) goto 999
!
!     VERIFIE-T'ON L'EXCENTREMENT DES POUTRES
    vexcen=.true.
    if (option .eq. 'PLAQ_POUT_ORTH') then
        call getvtx(motfac, 'VERIF_EXCENT', iocc=iocc, nbval=0, nbret=narl)
        if (narl .ne. 0) then
            call getvtx(motfac, 'VERIF_EXCENT', iocc=iocc, scal=k8bid, nbret=narl)
            if (k8bid(1:3) .eq. 'NON') vexcen=.false.
        endif
    endif
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
    cmp(1) = 'DX'
    cmp(2) = 'DY'
    cmp(3) = 'DZ'
    cmp(4) = 'DRX'
    cmp(5) = 'DRY'
    cmp(6) = 'DRZ'
    ccmp(1) = (0.0d0,0.0d0)
    ccmp(2) = (0.0d0,0.0d0)
    ccmp(3) = (0.0d0,0.0d0)
    do i = 1, 6
        icmp(i) = 0
    end do
!
    ligrel = '&&RAPO3D'
    lisnoe = '&&RAPO3D.LISTE_NOEUDS'
    lismai = '&&RAPO3D.LISTE_MAILLES'
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
! --- -----------------------------------------------------------------
!     LIGREL DU MODELE
    ligrmo = mod(1:8)//'.MODELE'
!
! --- -----------------------------------------------------------------
! --- MAILLAGE ASSOCIE AU MODELE
    call jeveuo(ligrmo//'.LGRF', 'L', vk8=lgrf)
    noma = lgrf(1)
    noeuma = noma//'.NOMNOE'
    grnoma = noma//'.GROUPENO'
!
! --- -----------------------------------------------------------------
! --- RECUPERATION DU TABLEAU DES COORDONNEES
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
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
        call jeveuo(ligrmo//'.PRNM', 'L', vi=prnm)
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
! --- CONSTITUTION DU LIGREL FORME DES MAILLES DE SURFACE MODELISANT
!     LA TRACE DE LA SECTION DE LA POUTRE SUR LE MASSIF 3D
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
!     VERIFICATION DE LA PLANEITE DE LA SURFACE :
    call getvr8(motfac, 'ANGL_MAX', iocc=iocc, scal=angt, nbret=ibid)
    if (option .eq. 'PLAQ_POUT_ORTH') then
        call veripl(noma, nbma, zi(jlisma), angt, 'F')
    else
        call veripl(noma, nbma, zi(jlisma), angt, 'A')
    endif
!
! --- -----------------------------------------------------------------
! --- Recuperation du noeud "poutre" (_2) :
    motcle(1)='GROUP_NO_2'
    motcle(2)='NOEUD_2'
    motcle(3)='GROUP_MA_2'
    motcle(4)='MAILLE_2'
    typmcl(1)='GROUP_NO'
    typmcl(2)='NOEUD'
    typmcl(3)='GROUP_MA'
    typmcl(4)='MAILLE'
    call reliem(' ', noma, 'NO_NOEUD', motfac, iocc,&
                  4, motcle, typmcl, '&&RAPO3D.NO2', nbno)

    if (nbno .ne. 1) then
        call utmess('F', 'MODELISA6_40', si=nbno)
    endif
    call jeveuo('&&RAPO3D.NO2','L',jno2)
    noepou=zk8(jno2)
    call jenonu(jexnom(noeuma, noepou), numnop)
    call jedetr('&&RAPO3D.NO2')
!   --- coordonnees du noeud poutre
    xpou = vale(3*(numnop-1)+1)
    ypou = vale(3*(numnop-1)+2)
    zpou = vale(3*(numnop-1)+3)
!
! --- -----------------------------------------------------------------
! --- VERIFICATION DU FAIT QUE LES NOEUDS DE LISNOE :
!        SI MASSIF : NE PORTENT PAS DE COMPOSANTES DE ROTATION.
!        SI COQUE  : PORTENT DES COMPOSANTES DE ROTATION.
    if (option .eq. 'PLAQ_POUT_ORTH') then
        do i = 1, lonlis
!           NUMERO DU NOEUD COURANT DE LA LISTE
            call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+i-1)), ino)
!           IL DOIT ETRE DIFFERENT DU NOEUD DE LA POUTRE
            if (ino .eq. numnop) then
                valk(1) = noepou
                valr(1) = xpou
                valr(2) = ypou
                valr(3) = zpou
                vali(1) = iocc
                call utmess('F', 'MODELISA6_28', sk=valk(1), si=vali(1), nr=3,&
                            valr=valr)
            endif
            dg = prnm((ino-1)*nbec+1)
            do j = 4, 6
                icmp(j) = indik8(nomcmp,cmp(j),1,nddla)
                if (.not. exisdg([dg],icmp(j))) then
                    valk(1) = zk8(ilisno+i-1)
                    valk(2) = cmp(j)
                    call utmess('F', 'MODELISA6_32', nk=2, valk=valk)
                endif
            end do
        end do
    else
        do i = 1, lonlis
!           NUMERO DU NOEUD COURANT DE LA LISTE
            call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+i-1)), ino)
            dg = prnm((ino-1)*nbec+1)
            do j = 4, 6
                icmp(j) = indik8(nomcmp,cmp(j),1,nddla)
                if (exisdg([dg],icmp(j))) then
                    valk(1) = zk8(ilisno+i-1)
                    valk(2) = cmp(j)
                    call utmess('F', 'MODELISA6_44', nk=2, valk=valk)
                endif
            end do
        end do
    endif
!
! --- -----------------------------------------------------------------
! --- VERIFICATION DU FAIT QUE LE NOEUD POUTRE A RACCORDER PORTE
!     LES 3 DDLS DE TRANSLATION ET LES 3 DDLS DE ROTATION.
    dg = prnm((numnop-1)*nbec+1)
    do j = 1, 6
        icmp(j) = indik8(nomcmp,cmp(j),1,nddla)
        if (.not.exisdg([dg],icmp(j))) then
            valk(1) = noepou
            valk(2) = cmp(j)
            call utmess('F', 'MODELISA6_45', nk=2, valk=valk)
        endif
    end do
!
! --- -----------------------------------------------------------------
! --- CALCUL SUR CHAQUE ELEMENT DE SURFACE A RELIER A LA POUTRE
!     DES CARACTERISTIQUES GEOMETRIQUES SUIVANTES :
!        SOMME/S_ELEMENT(1,X,Y,Z,X*X,Y*Y,Z*Z,X*Y,X*Z,Y*Z)DS
    lpain(1) = 'PGEOMER'
    lchin(1) = noma//'.COORDO'
    lpaout(1) = 'PCASECT'
    lchout(1) = '&&RAPO3D.PSECT'
!
    call calcul('S', 'CARA_SECT_POUT3', ligrel, 1, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
! --- -----------------------------------------------------------------
! --- VECTEUR DES QUANTITES GEOMETRIQUES PRECITEES SOMMEES
!     SUR LA SURFACE DE RACCORD, CES QUANTITES SERONT NOTEES :
!        A1 = S,AX,AY,AZ,AXX,AYY,AZZ,AXY,AXZ,AYZ
    AS_ALLOCATE(vr=inertie_raccord, size=16)
! --- -----------------------------------------------------------------
!     SOMMATION DES QUANTITES GEOMETRIQUES ELEMENTAIRES
!     DANS LE VECTEUR &&RAPO3D.INERTIE_RACCORD :
!     SEULES LES 10 PREMIERES VALEURS SERONT UTILISEES
    call mesomm(lchout(1), 16, vr=inertie_raccord)
!
    s = inertie_raccord(1)
    ax = inertie_raccord(2)
    ay = inertie_raccord(3)
    az = inertie_raccord(4)
    axx = inertie_raccord(5)
    ayy = inertie_raccord(6)
    azz = inertie_raccord(7)
    axy = inertie_raccord(8)
    axz = inertie_raccord(9)
    ayz = inertie_raccord(10)
!
    if (abs(s) .lt. r8prem()) then
        call utmess('F', 'MODELISA6_46')
    endif
    s1 = 1.0d0/s
!
! --- -----------------------------------------------------------------
! --- COORDONNEES DU CENTRE GEOMETRIQUE G DE LA SECTION DE RACCORD
!     XG = AX/S, YG = AY/S, ZG = AZ/S
    xg = s1*ax
    yg = s1*ay
    zg = s1*az
! --- -----------------------------------------------------------------
!     VERIFICATION DE L'IDENTITE GEOMETRIQUE DE G AVEC LE
!     NOEUD POUTRE A RACCORDER :
    dnorme = sqrt(&
             (xpou-xg)*(xpou-xg)+ (ypou-yg)*(ypou-yg)+ (zpou-zg)*(zpou-zg))/sqrt(s/3.14159265d0)
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
        if (vexcen) then
            call utmess('A', 'CALCULEL3_80', sk=valk(1), si=vali(1), nr=9,&
                        valr=valr)
        else
            call utmess('I', 'CALCULEL3_78', sk=valk(1), si=vali(1), nr=9,&
                        valr=valr)
        endif
    endif
!
! --- -----------------------------------------------------------------
!     RECUPERATION DES VECTEURS TANGENTS ORTHONORMES DU 1ER ELEMENT
    if (option .eq. 'PLAQ_POUT_ORTH') then
        call mesomm(lchout(1), 16, vr=inertie_raccord, nbma=1, linuma=zi(jlisma))
        vtang(1) = inertie_raccord(11)
        vtang(2) = inertie_raccord(12)
        vtang(3) = inertie_raccord(13)
        vtang(4) = inertie_raccord(14)
        vtang(5) = inertie_raccord(15)
        vtang(6) = inertie_raccord(16)
    endif
!
! --- -----------------------------------------------------------------
!     CALCUL DU TENSEUR D'INERTIE EN G, CE TENSEUR EST SYMETRIQUE :
!     ON CALCULE LES COMPOSANTES DE LA PARTIE SUPERIEURE PAR LIGNE
!
! --- IGXX = AYY + AZZ -S*(YG*YG+ZG*ZG)
    ig(1) = ayy + azz - s*(yg*yg+zg*zg)
! --- IGXY = -AXY + S*XG*YG
    ig(2) = -axy + s*xg*yg
! --- IGXZ = -AXZ + S*XG*ZG
    ig(3) = -axz + s*xg*zg
! --- IGYY = AZZ + AXX -S*(ZG*ZG+XG*XG)
    ig(4) = azz + axx - s*(zg*zg+xg*xg)
! --- IGYZ = -AYZ + S*YG*ZG
    ig(5) = -ayz + s*yg*zg
! --- IGZZ = AXX + AYY -S*(XG*XG+YG*YG)
    ig(6) = axx + ayy - s*(xg*xg+yg*yg)
!
! --- -----------------------------------------------------------------
!     NOTATION DANS LA CARTE DE NOM '&&RAPO3D.CAORIGE' DES
!     COORDONNEES DU CENTRE GEOMETRIQUE G DE LA SECTION DE RACCORD
    nocmp(1) = 'X'
    nocmp(2) = 'Y'
    nocmp(3) = 'Z'
!
    coorig(1) = xg
    coorig(2) = yg
    coorig(3) = zg
!
    call mecact('V', '&&RAPO3D.CAORIGE', 'LIGREL', ligrel, 'GEOM_R',&
                ncmp=3, lnomcmp=nocmp, vr=coorig)
!
! --- DETERMINATION DE 2 LISTES  DE VECTEURS PAR ELEMENT PRENANT
!     LEURS VALEURS AUX NOEUDS DES ELEMENTS.
! --- LA PREMIERE LISTE DE NOM 'VECT_NI' A POUR VALEURS AU NOEUD
!     I D'UN ELEMENT : SOMME/S_ELEMENT(NI,0,0)DS
! --- LA SECONDE LISTE DE NOM 'VECT_XYZNI' A POUR VALEURS AU NOEUD
!     I D'UN ELEMENT : SOMME/S_ELEMENT(X*NI,Y*NI,Z*NI)DS
!        AVEC  X = XM - XG = NJ*XJ - XG
!              Y = YM - YG = NJ*YJ - YG
!              Z = ZM - ZG = NJ*ZJ - ZG
!
    lpain(1) = 'PGEOMER'
    lpain(2) = 'PORIGIN'
    lchin(1) = noma//'.COORDO'
    lchin(2) = '&&RAPO3D.CAORIGE'
    lpaout(1) = 'PVECTU1'
    lpaout(2) = 'PVECTU2'
    lchout(1) = '&&RAPO3D.VECT_NI'
    lchout(2) = '&&RAPO3D.VECT_XYZNI'
!
    call calcul('S', 'CARA_SECT_POUT4', ligrel, 2, lchin,&
                lpain, 2, lchout, lpaout, 'V',&
                'OUI')
!
! --- -----------------------------------------------------------------
! --- CREATION DES .RERR DES VECTEURS EN SORTIE DE CALCUL
    call memare('V', '&&RAPO3D', mod, ' ', ' ',&
                'CHAR_MECA')
!
! --- -----------------------------------------------------------------
!     ASSEMBLAGE DE LCHOUT(1) DANS LE CHAMNO DE NOM 'CH_DEPL_1'
    call jedetr('&&RAPO3D           .RELR')
    call reajre('&&RAPO3D', lchout(1), 'V')
    call assvec('V', 'CH_DEPL_1', 1, '&&RAPO3D           .RELR', [1.d0],&
                numddl, ' ', 'ZERO', 1)
!
! --- -----------------------------------------------------------------
!     ASSEMBLAGE DE LCHOUT(2) DANS LE CHAMNO DE NOM 'CH_DEPL_2'
    call jedetr('&&RAPO3D           .RELR')
    call reajre('&&RAPO3D', lchout(2), 'V')
    call assvec('V', 'CH_DEPL_2', 1, '&&RAPO3D           .RELR', [1.d0],&
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
    nbterm = 3*lonlis + 3
! --- VECTEUR DU NOM DES NOEUDS
    AS_ALLOCATE(vk8=lisno, size=nbterm)
! --- VECTEUR DU NOM DES DDLS
    AS_ALLOCATE(vk8=lisddl, size=nbterm)
! --- VECTEUR DES COEFFICIENTS REELS
    AS_ALLOCATE(vr=coer, size=nbterm)
! --- VECTEUR DES COEFFICIENTS COMPLEXES
    AS_ALLOCATE(vc=coec, size=nbterm)
! --- VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
    AS_ALLOCATE(vr=direct, size=3*nbterm)
! --- VECTEUR DES DIMENSIONS DE CES DIRECTIONS
    AS_ALLOCATE(vi=dime, size=nbterm)
!
! --- -----------------------------------------------------------------
! --- RELATIONS ENTRE LES NOEUDS DE LA COQUE ET LE NOEUD POUTRE
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
        lisno(i) = zk8(ilisno+i-1)
        lisddl(i) = 'DX'
        coer(i) = zr(idch1+ival-1)
    end do
!
    lisno(1+lonlis+1-1) = noepou
    lisddl(1+lonlis+1-1) = 'DX'
    coer(1+lonlis+1-1) = -s
!
    call afrela(coer, coec, lisddl, lisno, dime,&
                direct, nbterm, beta, betac, betaf,&
                typcoe, typval, typlag, 0.d0, lisrel)
    call imprel(motfac, nbterm, coer, lisddl, lisno,&
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
        lisno(i) = zk8(ilisno+i-1)
        lisddl(i) = 'DY'
        coer(i) = zr(idch1+ival-1)
    end do
!
    lisno(1+lonlis+1-1) = noepou
    lisddl(1+lonlis+1-1) = 'DY'
    coer(1+lonlis+1-1) = -s
!
    call afrela(coer, coec, lisddl, lisno, dime,&
                direct, nbterm, beta, betac, betaf,&
                typcoe, typval, typlag, 0.d0, lisrel)
    call imprel(motfac, nbterm, coer, lisddl, lisno,&
                beta)
!
! --- TROISIEME RELATION :
!     -S.DZ(NOEUD_POUTRE) + (SOMME/S_RACCORD(NI.DS)).DZ(NOEUD_I) = 0
    nbterm = lonlis + 1
!     BOUCLE SUR LES NOEUDS DES MAILLES DE LA TRACE DE LA POUTRE
    do i = 1, lonlis
        call jenonu(jexnom(noeuma, zk8(ilisno+i-1)), ino)
!        ADRESSE DE LA PREMIERE COMPOSANTE DU NOEUD INO DANS LES CHAMNO
        ival = zi(iaprno + (ino-1)*(nbec+2))
!
        lisno(i) = zk8(ilisno+i-1)
        lisddl(i) = 'DZ'
        coer(i) = zr(idch1+ival-1)
    end do
!
    lisno(1+lonlis+1-1) = noepou
    lisddl(1+lonlis+1-1) = 'DZ'
    coer(1+lonlis+1-1) = -s
!
    call afrela(coer, coec, lisddl, lisno, dime,&
                direct, nbterm, beta, betac, betaf,&
                typcoe, typval, typlag, 0.d0, lisrel)
    call imprel(motfac, nbterm, coer, lisddl, lisno,&
                beta)
!
! --- -----------------------------------------------------------------
! --- DEUXIEME GROUPE DE RELATIONS TRADUISANT :
!        SOMME/S_RACCORD(GM X U_3D) = I.OMEGA(NOEUD_POUTRE)
!
! --- QUATRIEME RELATION :
!        (SOMME/S_RACCORD(Y*NI.DS)).DZ(NOEUD_I) -
!        (SOMME/S_RACCORD(Z*NI.DS)).DY(NOEUD_I) -
!        IXX.DRX(NOEUD_POUTRE) - IXY.DRY(NOEUD_POUTRE) -
!        IXZ.DRZ(NOEUD_POUTRE)                          = 0
    nbterm = 2*lonlis + 3
!     BOUCLE SUR LES NOEUDS DES MAILLES DE LA COQUE
    do i = 1, lonlis
        call jenonu(jexnom(noeuma, zk8(ilisno+i-1)), ino)
!        ADRESSE DE LA PREMIERE COMPOSANTE DU NOEUD INO DANS LES CHAMNO
        ival = zi(iaprno + (ino-1)*(nbec+2))
!
        lisno(1+2*(i-1)+1-1) = zk8(ilisno+i-1)
        lisno(1+2*(i-1)+2-1) = zk8(ilisno+i-1)
        lisddl(1+2*(i-1)+1-1) = 'DZ'
        lisddl(1+2*(i-1)+2-1) = 'DY'
!        SOMME/S_RACCORD(Y*NI.DS) = ZR(IDCH2+IVAL+1-1)
!        SOMME/S_RACCORD(Z*NI.DS) = ZR(IDCH2+IVAL+2-1)
        coer(1+2*(i-1)+1-1) = zr(idch2+ival-1+1)
        coer(1+2*(i-1)+2-1) = -zr(idch2+ival-1+2)
    end do
!
    lisno(1+2*lonlis+1-1) = noepou
    lisno(1+2*lonlis+2-1) = noepou
    lisno(1+2*lonlis+3-1) = noepou
!
    lisddl(1+2*lonlis+1-1) = 'DRX'
    lisddl(1+2*lonlis+2-1) = 'DRY'
    lisddl(1+2*lonlis+3-1) = 'DRZ'
!
    coer(1+2*lonlis+1-1) = -ig(1)
    coer(1+2*lonlis+2-1) = -ig(2)
    coer(1+2*lonlis+3-1) = -ig(3)
!
    call afrela(coer, coec, lisddl, lisno, dime,&
                direct, nbterm, beta, betac, betaf,&
                typcoe, typval, typlag, 0.d0, lisrel)
    call imprel(motfac, nbterm, coer, lisddl, lisno,&
                beta)
!
! --- CINQUIEME RELATION :
!        (SOMME/S_RACCORD(Z*NI.DS)).DX(NOEUD_I) -
!        (SOMME/S_RACCORD(X*NI.DS)).DZ(NOEUD_I) -
!        IXY.DRX(NOEUD_POUTRE) - IYY.DRY(NOEUD_POUTRE) -
!        IYZ.DRZ(NOEUD_POUTRE)                            = 0
    nbterm = 2*lonlis + 3
!     BOUCLE SUR LES NOEUDS DES MAILLES DE SURFACE DU MASSIF
    do i = 1, lonlis
        call jenonu(jexnom(noeuma, zk8(ilisno+i-1)), ino)
!        ADRESSE DE LA PREMIERE COMPOSANTE DU NOEUD INO DANS LES CHAMNO
        ival = zi(iaprno+ (ino-1)* (nbec+2))
!
        lisno(1+2*(i-1)+1-1) = zk8(ilisno+i-1)
        lisno(1+2*(i-1)+2-1) = zk8(ilisno+i-1)
        lisddl(1+2*(i-1)+1-1) = 'DX'
        lisddl(1+2*(i-1)+2-1) = 'DZ'
!        SOMME/S_RACCORD(Z*NI.DS) = ZR(IDCH2+IVAL+2-1)
!        SOMME/S_RACCORD(X*NI.DS) = ZR(IDCH2+IVAL-1)
        coer(1+2*(i-1)+1-1) = zr(idch2+ival-1+2)
        coer(1+2*(i-1)+2-1) = -zr(idch2+ival-1)
    end do
!
    lisno(1+2*lonlis+1-1) = noepou
    lisno(1+2*lonlis+2-1) = noepou
    lisno(1+2*lonlis+3-1) = noepou
!
    lisddl(1+2*lonlis+1-1) = 'DRX'
    lisddl(1+2*lonlis+2-1) = 'DRY'
    lisddl(1+2*lonlis+3-1) = 'DRZ'
!
    coer(1+2*lonlis+1-1) = -ig(2)
    coer(1+2*lonlis+2-1) = -ig(4)
    coer(1+2*lonlis+3-1) = -ig(5)
!
    call afrela(coer, coec, lisddl, lisno, dime,&
                direct, nbterm, beta, betac, betaf,&
                typcoe, typval, typlag, 0.d0, lisrel)
    call imprel(motfac, nbterm, coer, lisddl, lisno,&
                beta)
!
! --- SIXIEME RELATION :
!        (SOMME/S_RACCORD(X*NI.DS)).DY(NOEUD_I) -
!        (SOMME/S_RACCORD(Y*NI.DS)).DX(NOEUD_I) -
!        IXZ.DRX(NOEUD_POUTRE) - IYZ.DRY(NOEUD_POUTRE) -
!        IZZ.DRZ(NOEUD_POUTRE)                            = 0
    nbterm = 2*lonlis + 3
!     BOUCLE SUR LES NOEUDS DES MAILLES DE SURFACE DU MASSIF
    do i = 1, lonlis
        call jenonu(jexnom(noeuma, zk8(ilisno+i-1)), ino)
!        ADRESSE DE LA PREMIERE COMPOSANTE DU NOEUD INO DANS LES CHAMNO
        ival = zi(iaprno+ (ino-1)* (nbec+2))
!
        lisno(1+2*(i-1)+1-1) = zk8(ilisno+i-1)
        lisno(1+2*(i-1)+2-1) = zk8(ilisno+i-1)
        lisddl(1+2*(i-1)+1-1) = 'DY'
        lisddl(1+2*(i-1)+2-1) = 'DX'
!        SOMME/S_RACCORD(X*NI.DS) = ZR(IDCH2+IVAL-1)
!        SOMME/S_RACCORD(Y*NI.DS) = ZR(IDCH2+IVAL+1-1)
        coer(1+2*(i-1)+1-1) = zr(idch2+ival-1)
        coer(1+2*(i-1)+2-1) = -zr(idch2+ival-1+1)
    end do
!
    lisno(1+2*lonlis+1-1) = noepou
    lisno(1+2*lonlis+2-1) = noepou
    lisno(1+2*lonlis+3-1) = noepou
!
    lisddl(1+2*lonlis+1-1) = 'DRX'
    lisddl(1+2*lonlis+2-1) = 'DRY'
    lisddl(1+2*lonlis+3-1) = 'DRZ'
!
    coer(1+2*lonlis+1-1) = -ig(3)
    coer(1+2*lonlis+2-1) = -ig(5)
    coer(1+2*lonlis+3-1) = -ig(6)
!
    call afrela(coer, coec, lisddl, lisno, dime,&
                direct, nbterm, beta, betac, betaf,&
                typcoe, typval, typlag, 0.d0, lisrel)
    call imprel(motfac, nbterm, coer, lisddl, lisno,&
                beta)
!
! === =================================================================
! === TRAITEMENT PLAQ_POUT_ORTH : DANS LE REPERE LOCAL DE LA COQUE
    if (option .eq. 'PLAQ_POUT_ORTH') then
! --- -----------------------------------------------------------------
! --- TROISIEME GROUPE DE RELATIONS TRADUISANT DANS LE PLAN TANGENT
!        SOMME/S_RACCORD(OMEGA_NOEUD_I) = S_RACCORD*OMEGA(NOEUD_POUTRE)
!
! --- -----------------------------------------------------------------
! --- PREMIERE RELATION :
!     -S.DRX(NOEUD_POUTRE) + (SOMME/S_RACCORD(NI.DS)).DRX(NOEUD_I) = 0
        nbterm = 3*lonlis + 3
!     BOUCLE SUR LES NOEUDS DES MAILLES DE LA TRACE DE LA POUTRE
        do i = 1, lonlis
            call jenonu(jexnom(noeuma, zk8(ilisno+i-1)), ino)
!        ADRESSE DE LA PREMIERE COMPOSANTE DU NOEUD INO DANS LES CHAMNO
            ival = zi(iaprno + (ino-1)*(nbec+2))
!
            lisno(1+3*(i-1)+1-1) = zk8(ilisno+i-1)
            lisno(1+3*(i-1)+2-1) = zk8(ilisno+i-1)
            lisno(1+3*(i-1)+3-1) = zk8(ilisno+i-1)
            lisddl(1+3*(i-1)+1-1) = 'DRX'
            lisddl(1+3*(i-1)+2-1) = 'DRY'
            lisddl(1+3*(i-1)+3-1) = 'DRZ'
            coer(1+3*(i-1)+1-1) = vtang(1)*zr(idch1+ival-1)
            coer(1+3*(i-1)+2-1) = vtang(2)*zr(idch1+ival-1)
            coer(1+3*(i-1)+3-1) = vtang(3)*zr(idch1+ival-1)
        end do
!
        lisno(1+3*lonlis+1-1) = noepou
        lisno(1+3*lonlis+2-1) = noepou
        lisno(1+3*lonlis+3-1) = noepou
        lisddl(1+3*lonlis+1-1) = 'DRX'
        lisddl(1+3*lonlis+2-1) = 'DRY'
        lisddl(1+3*lonlis+3-1) = 'DRZ'
        coer(1+3*lonlis+1-1) = -s*vtang(1)
        coer(1+3*lonlis+2-1) = -s*vtang(2)
        coer(1+3*lonlis+3-1) = -s*vtang(3)
!
        call afrela(coer, coec, lisddl, lisno, dime,&
                    direct, nbterm, beta, betac, betaf,&
                    typcoe, typval, typlag, 0.d0, lisrel)
        call imprel(motfac, nbterm, coer, lisddl, lisno,&
                    beta)
!
! --- PREMIERE RELATION :
!     -S.DRY(NOEUD_POUTRE) + (SOMME/S_RACCORD(NI.DS)).DRY(NOEUD_I) = 0
        nbterm = 3*lonlis + 3
!     BOUCLE SUR LES NOEUDS DES MAILLES DE LA TRACE DE LA POUTRE
        do i = 1, lonlis
            call jenonu(jexnom(noeuma, zk8(ilisno+i-1)), ino)
!        ADRESSE DE LA PREMIERE COMPOSANTE DU NOEUD INO DANS LES CHAMNO
            ival = zi(iaprno + (ino-1)*(nbec+2))
!
            lisno(1+3*(i-1)+1-1) = zk8(ilisno+i-1)
            lisno(1+3*(i-1)+2-1) = zk8(ilisno+i-1)
            lisno(1+3*(i-1)+3-1) = zk8(ilisno+i-1)
            lisddl(1+3*(i-1)+1-1) = 'DRX'
            lisddl(1+3*(i-1)+2-1) = 'DRY'
            lisddl(1+3*(i-1)+3-1) = 'DRZ'
            coer(1+3*(i-1)+1-1) = vtang(4)*zr(idch1+ival-1)
            coer(1+3*(i-1)+2-1) = vtang(5)*zr(idch1+ival-1)
            coer(1+3*(i-1)+3-1) = vtang(6)*zr(idch1+ival-1)
        end do
!
        lisno(1+3*lonlis+1-1) = noepou
        lisno(1+3*lonlis+2-1) = noepou
        lisno(1+3*lonlis+3-1) = noepou
        lisddl(1+3*lonlis+1-1) = 'DRX'
        lisddl(1+3*lonlis+2-1) = 'DRY'
        lisddl(1+3*lonlis+3-1) = 'DRZ'
        coer(1+3*lonlis+1-1) = -s*vtang(4)
        coer(1+3*lonlis+2-1) = -s*vtang(5)
        coer(1+3*lonlis+3-1) = -s*vtang(6)
!
        call afrela(coer, coec, lisddl, lisno, dime,&
                    direct, nbterm, beta, betac, betaf,&
                    typcoe, typval, typlag, 0.d0, lisrel)
        call imprel(motfac, nbterm, coer, lisddl, lisno,&
                    beta)
!
    endif
! === =================================================================
!
! --- -----------------------------------------------------------------
! --- RACCORD 3D - TUYAU : LIAISONS SUR DDLS DE FOURIER
    if (option .eq. '3D_TUYAU') then
        call getvid(motfac, 'CARA_ELEM', iocc=iocc, scal=cara, nbret=ncara)
        if (ncara .eq. 0) then
            call utmess('F', 'MODELISA6_47')
        endif
        call ratu3d(zi(iaprno), lonlis, zk8(ilisno), noepou, noma,&
                    ligrel, mod, cara, numddl, typlag,&
                    lisrel, coorig, s)
    endif
!
! --- -----------------------------------------------------------------
! --- DESTRUCTION DES OBJETS DE TRAVAIL
!
    call jedetr('&&RAPO3D.LISTE_NOEUDS')
    call jedetr('&&RAPO3D.LISTE_MAILLES')
    AS_DEALLOCATE(vr=inertie_raccord)
    call jedetr('&&RAPO3D           .RELR')
    call jedetr('&&RAPO3D           .RERR')
    AS_DEALLOCATE(vk8=lisno)
    AS_DEALLOCATE(vk8=lisddl)
    AS_DEALLOCATE(vr=coer)
    AS_DEALLOCATE(vc=coec)
    AS_DEALLOCATE(vr=direct)
    AS_DEALLOCATE(vi=dime)
    call detrsd('CHAMP_GD', '&&RAPO3D.PSECT')
    call detrsd('RESUELEM', '&&RAPO3D.VECT_NI')
    call detrsd('RESUELEM', '&&RAPO3D.VECT_XYZNI')
    call detrsd('CARTE', '&&RAPO3D.CAORIGE')
    call detrsd('CHAM_NO', 'CH_DEPL_1')
    call detrsd('CHAM_NO', 'CH_DEPL_2')
    call detrsd('LIGREL', ligrel)
!
999 continue
    call jedema()
end subroutine
