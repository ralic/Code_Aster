subroutine resloc(modele, ligrel, yaxfem, yathm, tbgrca,&
                  perman, chtime, mate, sigmam, sigmap,&
                  chsigx, chdepm, chdepp, cherrm, lchar,&
                  nchar, tabido, chvois, cvoisx, chelem)
! ......................................................................
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
!     BUT:
!         CALCUL DE L'ESTIMATEUR D'ERREUR EN RESIDU
!
!         OPTION : 'ERME_ELEM'
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
!     MODELE : NOM DU MODELE
!     LIGREL : NOM DU LIGREL
!     YAXFEM : EST-CE UNE MODELISATION XFEM ?
!     YATHM  : EST-CE UNE MODELISATION THM ?
!     TBGRCA : TABLEAU GRANDEURS CARACTERISTIQUES
!              (UTILISE SI MODELISATION HM)
!     PERMAN : SI OUI, EST-CE UNE MODELISATION PERMANENTE ?
!     CHTIME : CARTE D'INSTANTS POUR L'INSTANT ACTUEL
!     MATE   : NOM DU CONCEPT CHAMP_MATERIAU
!     SIGMAM : NOM DU CHAMP DE CONTRAINTES AUX NOEUDS PAR ELEMENT
!              A L'INSTANT PRECEDENT
!     SIGMAP : NOM DU CHAMP DE CONTRAINTES AUX NOEUDS PAR ELEMENT
!              A L'INSTANT ACTUEL
!     CHDEPM : NOM DU CHAMP DE DEPLACEMENTS A L'INSTANT PRECEDENT
!     CHDEPP : NOM DU CHAMP DE DEPLACEMENTS A L'INSTANT ACTUEL
!     CHERRM : NOM DU CHAMP D'INDICATEURS A L'INSTANT PRECEDENT
!     LCHAR  : LISTE DES CHARGES
!     NCHAR  : NOMBRE DE CHARGES
!     TABIDO : TABLEAU D'ENTIERS CONTENANT DES ADRESSES
!          1 : IATYMA : ADRESSE DU VECTEUR TYPE MAILLE (NUMERO <-> NOM)
!          2 : IAGD   : ADRESSE DU VECTEUR GRANDEUR (NUMERO <-> NOM)
!          3 : IACMP  : ADRESSE DU VECTEUR NOMBRE DE COMPOSANTES
!                 (NUMERO DE GRANDEUR <-> NOMBRE DE COMPOSANTES)
!          4 : ICONX1 : ADRESSE DE LA COLLECTION CONNECTIVITE
!          5 : ICONX2 : ADRESSE DU POINTEUR DE LONGUEUR DE LA
!                       CONNECTIVITE
!     CHVOIS : NOM DU CHAMP DES VOISINS
!     CVOISX : POUR XFEM, NOM DU CHAMP DES VOISINS DES SOUS-ELEMENTS
!
!      SORTIE :
!-------------
!      CHELEM  : NOM DU CHAM_ELEM_ERREUR PRODUIT
!                SI CHELEM EXISTE DEJA, ON LE DETRUIT.
!
! REMARQUE : RESLOC ET QIRES1 DOIVENT RESTER TRES SIMILAIRES
! ......................................................................
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/etenca.h"
#include "asterfort/exisd.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/mecact.h"
#include "asterfort/megeom.h"
#include "asterfort/utmess.h"
!
    integer :: nchar
    integer :: tabido(5)
    character(len=8) :: modele, lchar(1)
    character(len=24) :: sigmam, sigmap, chdepm, chdepp, cherrm
    character(len=24) :: chtime, chvois, chsigx, cvoisx, chelem
    character(len=*) :: ligrel, mate
    real(kind=8) :: tbgrca(3)
    logical(kind=1) :: yaxfem, yathm, perman
!
! DECLARATION VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'RESLOC' )
!
    integer :: nbcmp
    parameter ( nbcmp = 12 )
!
    integer :: nbchix
    parameter ( nbchix = 20 )
!
    integer :: i, j, icmp(nbcmp), iatyma, iagd, iacmp, iconx1, iconx2
    integer :: iret, iret1, iret2, iret4, iret5, iret6, iret7
    integer :: jceldp, jcelvp, jceldm, jcelvm
    integer :: iade1, iaptm1, iava1, numgd1
    integer :: iade2, iaptm2, iava2, numgd2
    integer :: iade3, iaptm3, iava3, numgd3
    integer :: iarepe
    integer :: ibid
    integer :: nbrin
    real(kind=8) :: rcmp(2)
    character(len=1) :: base
    character(len=8) :: licmp(nbcmp), lpain(nbchix), lpaout(1)
    character(len=8) :: typc3
    character(len=16) :: option
    character(len=19) :: carte1, carte2, carte3, nomgd1, nomgd2, nomgd3
    character(len=19) :: pintto, cnseto, loncha, pmilto
    character(len=24) :: chgeom, lchin(nbchix), lchout(1), chfor1, chfor2
    character(len=24) :: chfor3
!
    integer :: ntychx
    parameter ( ntychx = 9 )
    integer :: itycha(ntychx)
    character(len=5) :: ktych(ntychx)
!
! DEB-------------------------------------------------------------------
!====
! 1. PREALABLES
!====
!
    base = 'V'
!
    call megeom(modele, chgeom)
!
!
! ------- TEST SUR LE TYPE DE CHARGEMENT DES BORDS --------------------
!
!   ATTENTION : POUR UN MEME CHARGEMENT (FORCE_FACE OU PRES_REP), SEULE
!   LA DERNIERE CHARGE EST CONSIDEREE (REGLE DE SURCHARGE ACTUELLEMENT)
! --- ON ALARME POUR LES CHARGES NON TRAITEES
!
    ktych(1) = 'CIMPO'
    ktych(2) = 'F1D2D'
    ktych(3) = 'F2D3D'
    ktych(4) = 'PRESS'
    ktych(5) = 'PESAN'
    ktych(6) = 'ROTAT'
    ktych(7) = 'F2D2D'
    ktych(8) = 'F3D3D'
    ktych(9) = 'FLUX'
!
    do 11 , j = 1 , ntychx
    itycha(j) = 0
    11 end do
!
!GN      WRITE(6,*) 'NCHAR =', NCHAR
    iret1 = 0
    do i = 1, nchar
        iret2 = 0
        do 121 , j = 1 , ntychx
        call exisd('CHAMP_GD', lchar(i)//'.CHME.'//ktych(j), iret)
        if (iret .ne. 0) then
            itycha(j) = itycha(j) + 1
            iret2 = iret2 + 1
        endif
121     continue
        if (iret2 .eq. 0) then
            call utmess('A', 'INDICATEUR_6', sk=lchar(i))
            iret1 = iret1 + 1
        endif
    end do
!
!     ON VERIFIE QU'UN TYPE DE CHARGE N'EST PAS PRESENT 2 FOIS
!     REMARQUE : SAUF POUR DU DIRICHLET (CIMPO)
!
!GN        WRITE(6,*) 'ITYCHA(',1,') =', ITYCHA(1)
    do 13 , j = 2 , ntychx
!GN        WRITE(6,*) 'ITYCHA(',J,') =', ITYCHA(J)
    if (itycha(j) .gt. 1) then
        call utmess('A', 'INDICATEUR_7', sk=ktych(j))
        iret1 = iret1 + 1
    endif
    13 end do
!
    if (iret1 .ne. 0) then
        call utmess('F', 'INDICATEUR_8')
    endif
!
!====
! 2. DECODAGE DES CHARGES
!====
!
    carte1 = ' '
    carte2 = ' '
    carte3 = ' '
    nomgd1 = ' '
    nomgd2 = ' '
    nomgd3 = ' '
!
    do i = 1, nchar
!GN        WRITE(6,*) 'CHARGE A ANALYSER : ',LCHAR(I)
!GN      CALL UTIMSD(6,1,.TRUE.,.TRUE.,LCHAR(I)//'.CHME',1,' ')
!
! 2.1. ==> FORCES AU BORD
!
        call exisd('CHAMP_GD', lchar(i)//'.CHME.F1D2D', iret1)
        call exisd('CHAMP_GD', lchar(i)//'.CHME.F2D3D', iret2)
        if (iret1 .ne. 0) then
            carte1 = lchar(i)//'.CHME.F1D2D'
            call dismoi('NOM_GD', carte1, 'CARTE', repk=nomgd1)
            call etenca(carte1, ligrel, iret)
            ASSERT(iret.eq.0)
!GN        WRITE(6,*) 'ON A DU F1D2D'
        else if (iret2.ne.0) then
            carte1 = lchar(i)//'.CHME.F2D3D'
            call dismoi('NOM_GD', carte1, 'CARTE', repk=nomgd1)
            call etenca(carte1, ligrel, iret)
            ASSERT(iret.eq.0)
!GN        WRITE(6,*) 'ON A DU F2D3D'
        endif
!
! 2.2. ==> PRESSIONS MECANIQUES
!
        call exisd('CHAMP_GD', lchar(i)//'.CHME.PRESS', iret1)
        if (iret1 .ne. 0) then
            carte2 = lchar(i)//'.CHME.PRESS'
            call dismoi('NOM_GD', carte2, 'CARTE', repk=nomgd2)
            call etenca(carte2, ligrel, iret)
            ASSERT(iret.eq.0)
!GN        WRITE(6,*) 'ON A DU PRESS AVEC CARTE2 : ',CARTE2
        endif
!
! 2.3. ==> EN THM, CONDITIONS DE NEUMANN HYDRAULIQUES
!
        if (yathm) then
            call exisd('CHAMP_GD', lchar(i)//'.CHME.FLUX', iret1)
            if (iret1 .ne. 0) then
                carte3 = lchar(i)//'.CHME.FLUX'
                call dismoi('NOM_GD', carte3, 'CARTE', repk=nomgd3)
                call etenca(carte3, ligrel, iret)
                ASSERT(iret.eq.0)
            endif
        endif
!
    end do
!
    call jeveuo(ligrel(1:19)//'.REPE', 'L', iarepe)
!
! RECUPERATION DES ADRESSES DU CHAMP DE CONTRAINTES A L'INSTANT ACTUEL
!
    call jeveuo(sigmap(1:19)//'.CELD', 'L', jceldp)
    call jeveuo(sigmap(1:19)//'.CELV', 'L', jcelvp)
!
! RECUPERATION DES ADRESSES DU CHAMP DE CONTRAINTES A L'INSTANT
! PRECEDENT (THM UNIQUEMENT)
!
    if (yathm .and. ( .not. perman )) then
        call jeveuo(sigmam(1:19)//'.CELD', 'L', jceldm)
        call jeveuo(sigmam(1:19)//'.CELV', 'L', jcelvm)
    else
        jceldm = -1
        jcelvm = -1
    endif
!
! RECUPERATION DES ADRESSES DE LA CARTE NUMERO 1
!
    if (carte1 .ne. ' ') then
        call jeveuo(carte1//'.DESC', 'L', iade1)
        call jeveuo(carte1//'.VALE', 'L', iava1)
        call jeexin(carte1//'.PTMA', iret)
        if (iret .eq. 0) then
            iaptm1 = 0
        else
!            LA CARTE A ETE ETENDUE
            call jeveuo(carte1//'.PTMA', 'L', iaptm1)
        endif
        call jenonu(jexnom('&CATA.GD.NOMGD', nomgd1), numgd1)
    else
        iade1 = 0
        iava1 = 0
        iaptm1 = 0
        numgd1 = 0
    endif
!
! RECUPERATION DES ADRESSES DE LA CARTE NUMERO 2
!
    if (carte2 .ne. ' ') then
!GN      WRITE(6,*) 'ADRESSES DE LA CARTE '//CARTE2
        call jeveuo(carte2//'.DESC', 'L', iade2)
        call jeveuo(carte2//'.VALE', 'L', iava2)
        call jeexin(carte2//'.PTMA', iret)
        if (iret .eq. 0) then
            iaptm2 = 0
        else
!            LA CARTE A ETE ETENDUE
            call jeveuo(carte2//'.PTMA', 'L', iaptm2)
        endif
        call jenonu(jexnom('&CATA.GD.NOMGD', nomgd2), numgd2)
!GN      WRITE(6,154) (ZI(IAPTM2+IBID),IBID=0,11)
!GN      WRITE(6,155) (ZR(IAVA2+IBID),IBID=0,35,6)
    else
        iade2 = 0
        iava2 = 0
        iaptm2 = 0
        numgd2 = 0
    endif
!GN  154 FORMAT(6I10)
!GN  155 FORMAT(G12.5)
!
! RECUPERATION DES ADRESSES DE LA CARTE NUMERO 3 (THM UNIQUEMENT)
!
    if (carte3 .ne. ' ') then
!GN       WRITE(6,*) 'ADRESSES DE LA CARTE '//CARTE3
        call jeveuo(carte3//'.DESC', 'L', iade3)
        call jeveuo(carte3//'.VALE', 'L', iava3)
        call jeexin(carte3//'.PTMA', iret)
!
        if (iret .eq. 0) then
            iaptm3 = 0
        else
!          LA CARTE A ETE ETENDUE
            call jeveuo(carte3//'.PTMA', 'L', iaptm3)
        endif
!
        call jenonu(jexnom('&CATA.GD.NOMGD', nomgd3), numgd3)
    else
        iade3 = 0
        iava3 = 0
        iaptm3 = 0
        numgd3 = 0
    endif
!
!====
! 3. CREATION DE 2 CARTES CONTENANT DES ADRESSES D'OBJETS JEVEUX
!====
!
    iatyma = tabido(1)
    iagd = tabido(2)
    iacmp = tabido(3)
    iconx1 = tabido(4)
    iconx2 = tabido(5)
!
    licmp(1) = 'X1'
    licmp(2) = 'X2'
    licmp(3) = 'X3'
    licmp(4) = 'X4'
    licmp(5) = 'X5'
    licmp(6) = 'X6'
    licmp(7) = 'X7'
    licmp(8) = 'X8'
    licmp(9) = 'X9'
    licmp(10) = 'X10'
    licmp(11) = 'X11'
    licmp(12) = 'X12'
!
    icmp(1) = iarepe
    icmp(2) = jceldp
    icmp(3) = jcelvp
    icmp(4) = iatyma
    icmp(5) = iagd
    icmp(6) = iacmp
!
    icmp(7) = iade1
    icmp(8) = iava1
    icmp(9) = iaptm1
    icmp(10) = numgd1
!
    icmp(11) = iconx1
    icmp(12) = iconx2
!
    call mecact(base, '&&'//nompro//'.CH_FORCE', 'MODELE', ligrel, 'NEUT_I',&
                ncmp=nbcmp, lnomcmp=licmp, vi=icmp)
!
    icmp(2) = jceldm
    icmp(3) = jcelvm
!
    icmp(5) = iade2
    icmp(6) = iava2
    icmp(7) = iaptm2
    icmp(8) = numgd2
!GN      WRITE(6,*) '    IADE2,    IAVA2,   IAPTM2,   NUMGD2'
!GN      WRITE(6,154) IADE2,IAVA2,IAPTM2,NUMGD2
!
    icmp(9) = iade3
    icmp(10) = iava3
    icmp(11) = iaptm3
    icmp(12) = numgd3
!
    call mecact(base, '&&'//nompro//'.CH_PRESS', 'MODELE', ligrel, 'NEUT_I',&
                ncmp=nbcmp, lnomcmp=licmp, vi=icmp)
!
!====
! 4. CHARGEMENTS VOLUMIQUES : PESANTEUR, ROTATION OU FORCES DE VOLUME
!     ATTENTION : SI UN TYPE DE CHARGEMENT EST PRESENT DANS PLUSIEURS
!                 CHARGES, SEULE SON OCCURENCE DANS LA DERNIERE CHARGE
!                 EST CONSIDEREE
!====
!
    chfor1 = ' '
    chfor2 = ' '
    chfor3 = ' '
    typc3 = '        '
!
    do i = 1, nchar
!GN        CALL UTIMSD(6,1,.TRUE.,.TRUE.,LCHAR(I),1,' ')
        call exisd('CHAMP_GD', lchar(i)//'.CHME.PESAN', iret4)
        call exisd('CHAMP_GD', lchar(i)//'.CHME.ROTAT', iret5)
        call exisd('CHAMP_GD', lchar(i)//'.CHME.F2D2D', iret6)
        call exisd('CHAMP_GD', lchar(i)//'.CHME.F3D3D', iret7)
        if (iret4 .ne. 0) then
            chfor1 = lchar(i)//'.CHME.PESAN.DESC'
!GN          WRITE(6,*) 'ON A DU CHFOR1'
        endif
        if (iret5 .ne. 0) then
            chfor2 = lchar(i)//'.CHME.ROTAT.DESC'
!GN          WRITE(6,*) 'ON A DU CHFOR2'
        endif
        if (iret6 .ne. 0) then
            chfor3 = lchar(i)//'.CHME.F2D2D.DESC'
            call jeveuo(lchar(i)//'.TYPE', 'L', ibid)
            typc3 = zk8(ibid)
!GN          WRITE(6,*) 'ON A DU F2D2D AVEC '//CHFOR3//' ET '//TYPC3
        endif
        if (iret7 .ne. 0) then
            chfor3 = lchar(i)//'.CHME.F3D3D.DESC'
            call jeveuo(lchar(i)//'.TYPE', 'L', ibid)
            typc3 = zk8(ibid)
!GN          WRITE(6,*) 'ON A DU F3D3D AVEC '//CHFOR3//' ET '//TYPC3
        endif
    end do
!
!====
! 5. PARAMETRES DU CALCUL
!====
!
! ==> SI ON EST SUR UNE MODELISATION XFEM ...
!
    if (yaxfem) then
!
! 5.1. ==> ... ON RECUPERE LES ADRESSES DES CHAMPS PROPRES A X-FEM
!
        pintto = modele//'.TOPOSE.PIN'
        cnseto = modele//'.TOPOSE.CNS'
        loncha = modele//'.TOPOSE.LON'
        pmilto = modele//'.TOPOSE.PMI'
!
    endif
!
! ==> SI ON EST SUR UNE MODELISATION HM ...
!
    if (yathm) then
!
! 5.2. ==> ... ON CREE UNE CARTE CONTENANT
!              LONGUEUR ET PRESSION CARACTERISTIQUES
!
        licmp(1) = 'X1'
        licmp(2) = 'X2'
        rcmp(1) = tbgrca(1)
        rcmp(2) = tbgrca(2)
!
        call mecact(base, '&&'//nompro//'.GRDCA', 'MODELE', ligrel, 'NEUT_R',&
                    ncmp=2, lnomcmp=licmp, vr=rcmp)
    endif
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PMATERC'
    lchin(2) = mate
    lpain(3) = 'PVOISIN'
    lchin(3) = chvois
    lpain(4) = 'PTEMPSR'
    lchin(4) = chtime
    lpain(5) = 'PCONTNO'
    lchin(5) = sigmap
    lpain(6) = 'PPESANR'
    lchin(6) = chfor1
    lpain(7) = 'PROTATR'
    lchin(7) = chfor2
    lpain(8) = 'PFORCE'
    lchin(8) = '&&'//nompro//'.CH_FORCE'
    lpain(9) = 'PPRESS'
    lchin(9) = '&&'//nompro//'.CH_PRESS'
    nbrin = 9
!
    if (yaxfem) then
!
        lpain(10)= 'PPINTTO'
        lchin(10)= pintto
        lpain(11)= 'PCNSETO'
        lchin(11)= cnseto
        lpain(12)= 'PLONCHA'
        lchin(12)= loncha
        lpain(13) = 'PCVOISX'
        lchin(13) = cvoisx
        lpain(14) = 'PCONTSER'
        lchin(14) = chsigx
        lpain(15)= 'PPMILTO'
        lchin(15)= pmilto
!
        nbrin = 15
!
    endif
!
    if (yathm) then
!
        lpain(10) = 'PDEPLAR'
        lchin(10) = chdepp
        lpain(11) = 'PGRDCA'
        lchin(11) = '&&'//nompro//'.GRDCA'
        if (perman) then
            nbrin = 11
        else
            lpain(12) = 'PDEPLMR'
            lchin(12) = chdepm
            lpain(13) = 'PCONTNM'
            lchin(13) = sigmam
            lpain(14) = 'PERREM'
            lchin(14) = cherrm
            nbrin = 14
        endif
!
    endif
!
    if (typc3(1:1) .ne. ' ') then
        nbrin = nbrin + 1
        if (typc3(1:7) .eq. 'MECA_RE') then
            lpain(nbrin) = 'PFRVOLU'
        else if (typc3(1:7).eq.'MECA_FO') then
            lpain(nbrin) = 'PFFVOLU'
        endif
        lchin(nbrin) = chfor3
    endif
!
    lpaout(1) = 'PERREUR'
    lchout(1) = chelem
!
    option = 'ERME_ELEM'
!
!GN      WRITE(6,*) NOMPRO,' APPELLE CALCUL POUR ', OPTION
!GN      WRITE(6,*) '  LPAIN    LCHIN'
!GN      DO 33 , IBID = 1 , NBRIN
!GN        WRITE(6,3000) IBID, LPAIN(IBID), LCHIN(IBID)
!GN   33 CONTINUE
!GN 3000 FORMAT(I2,1X,A8,1X,A24)
!
    call calcul('C', option, ligrel, nbrin, lchin,&
                lpain, 1, lchout, lpaout, 'G',&
                'OUI')
    call exisd('CHAMP_GD', lchout(1), iret)
    if (iret .eq. 0) then
        call utmess('F', 'CALCULEL2_88', sk=option)
    endif
!
!====
! 4. MENAGE FINAL
!====
!
    call jedetr(carte1//'.PTMA')
    call jedetr(carte2//'.PTMA')
    call jedetr(carte3//'.PTMA')
!
    call detrsd('CHAMP_GD', '&&'//nompro//'.CH_FORCE')
    call detrsd('CHAMP_GD', '&&'//nompro//'.CH_PRESS')
!
end subroutine
