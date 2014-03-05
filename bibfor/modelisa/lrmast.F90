subroutine lrmast(nomu, ifm, ifl, nbnoeu, nbmail,&
                  nbcoor)
    implicit none
!     IN
#include "jeveux.h"
#include "asterc/ismaem.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/enlird.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeimpo.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/leccoo.h"
#include "asterfort/lecdbg.h"
#include "asterfort/lecgrp.h"
#include "asterfort/lecmai.h"
#include "asterfort/lectit.h"
#include "asterfort/liritm.h"
#include "asterfort/sdmail.h"
#include "asterfort/stkcoo.h"
#include "asterfort/stkgrp.h"
#include "asterfort/stkmai.h"
#include "asterfort/stktit.h"
#include "asterfort/tesfin.h"
#include "asterfort/ulisop.h"
#include "asterfort/ulopen.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: ifm, ifl
    character(len=24) :: cooval, coodsc, cooref, grpnoe, grpmai, connex
    character(len=24) :: titre, nommai, nomnoe, typmai
    character(len=24) :: adapma
    character(len=8) :: nomu
!     OUT
    integer :: nbnoeu, nbmail, nbcoor
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     LECTURE DU FICHIER MAILLAGE AU FORMAT ASTER
!
!  REMARQUES ET RESTRICTIONS D UTILISATION
!
!               - ON VERIFIE LA PRESENCE D'UN ITEM  LORSQUE CELUI CI
!                 EST ATTENDU ( ORDRE LOGIQUE DE LA SEQUENCE A LIRE )
!
!               - ON TESTE LA PRESENCE D UN ITEM LORSQUE CELUI CI EST
!                 EVENTUEL ( MOT CLE A LIRE )
!
!               - LES MOTS CLES SONT A PRIORI TOUS RESERVES, MAIS
!                 IL N Y A PAS VERIFICATION A TOUT INSTANT DE LA
!                 PRESENCE D'UN MOT CLE SAUF POUR FIN ET FINSF
!
!               - ON IMPOSE QUE LES MOTS CLES SE TROUVENT EN DEBUT DE
!                 LIGNE (PREMIERE POSITION DANS L ENREGISTREMENT)
!
!               - ON  IMPOSE QUE LES DONNEES COMMENCENT  EN DEBUT DE
!                 LIGNE POUR CHAQUE REPETITION DU FORMAT
!
!               - AUCUN MOT NE DEPASSE 8 CARACTERES (SAUF DANS LES
!                 COMMENTAIRES)
!               - LES MINUSCULES SONT RELEVEES EN MAJUSCULES
!
!               - TOUT TEXTE APRES LE CARACTERE RESERVE % EST CONSIDERE
!                 COMME DU COMMENTAIRE ET EST IGNORE JUSQU EN FIN
!                 DE LIGNE
!
!               - UNE LIGNE FAIT 80 CARACTERES > TRONCATURE A 80 SI
!                 LA LONGUEUR D ENREGISTREMENT DEPASSE CETTE VALEUR :
!                 ATTENTION LORS DE L EDITION DE FICHIERS MAILLAGES AVEC
!                 VI OU TEXTEDIT SUR SUN
!
!               - LE BLANC EST SEPARATEUR MAIS NON SIGNIFICATIF
!
!
!
!  NOMENCLATURE
!
!       XXX             TYPE DE MOT CLE (LIE AU MODE DE STOCKAGE JEVEUX)
!                       = TIT > TYPE TITRE
!                       = GRP > TYPE GROUPE
!                       = COO > TYPE COORDONNEES
!                       = MAI > TYPE MAILLE
!                       = DBG > TYPE DEBUG (ASSISTANCE)
!
!  DESCRIPTION DES ROUTINES
!
!       LECXXX          PREMIERE LECTURE DE VERIFICATION DES DONNEES ET
!                       DE DIMENSIONNEMENT DES OBJETS JEVEUX POUR LES
!                       MOTS CLES DU TYPE XXX
!
!       STKXXX          DEUXIEME LECTURE ET STOCKAGE DES DONNEES SUR
!                       LES BASES POUR LES MOTS CLES DU TYPE XXX
!
!       TESFIN          TESTE L OCCURENCE DES MOTS CLES FIN ET FINSF
!
!       TESMCL          TESTE LA PRESENCE D UN MOT CLE
!
!       VERMOT          VERIFIE LA PRESENCE ATTENDUE D UN MOT
!
!       VERNMB          VERIFIE LA PRESENCE ATTENDUE D UN NOMBRE
!
!       VERDBL          VERIFIE QUE L ITEM LU EST EN DEBUT DE LIGNE
!
!       LIRITM          RECHERCHE DE  L ITEM SUIVANT
!
!       LIRLIG          LECTURE DE LA LIGNE SUIVANTE
!
!  DESCRIPTION DES PRINCIPALES VARIABLES
!
!
!       NBMXXX          NOMBRE DE MOTS CLES DU TYPE XXX
!       NBTXXX          NOMBRE TOTAL D ITEMS LUS POUR CHAQUE MOT CLE  DU
!                       TYPE XXX
!       DIMXXX          NOMBRE D ELEMENTS    LUS POUR CHAQUE MOT CLE  DU
!                       TYPE XXX
!
!       FMTXXX          NOMBRE D ITEMS A LIRE POUR CHAQUE ELEMENT DE
!                       CHAQUE MOT CLE DU TYPE XXX
!       MCLXXX          LISTE DES MOT CLES DU TYPE XXX
!
!       NBCOOR          NOMBRE DE COORDONNEES
!       NBMAIL          NOMBRE DE MAILLES
!       NBNOEU          NOMBRE DE NOEUDS
!       NBLTIT          NOMBRE DE LIGNES DE TITRE
!       NBGRNO          NOMBRE DE GROUPES NOEUDS
!       NBGRMA          NOMBRE DE GROUPES MAILLES
!
!       NBNOMA          NOMBRE TOTAL DE NOEUDS LUS DANS MAILLE
!       NBNOGN          NOMBRE TOTAL DE NOEUDS LUS DANS  GROUPE NOEUD
!       NBMAGM          NOMBRE TOTAL DE MAILLES LUES DANS GROUPE MAILLE
!
!-----------------------------------------------------------------------
!
!
! ----- DECLARATIONS
!
!-----------------------------------------------------------------------
    integer :: i, iad, ibid, icl, ier, ii, im1
    integer :: inbn, iret, irtet, iv, j, jgcnx, jgg
    integer :: jmail, jmail2, jnoeu, jnoeu2, jvcnx, jvg, nbg
    integer :: nbgrma, nbgrmp, nbgrno, nbgrnp, nbltit, nbma, nbma1
    integer :: nbmagm, nbmcoo, nbmdbg, nbmgrp, nbmint, nbmmai, nbmmax
    integer :: nbmtit, nbno, nbno1, nbnogn, nbnoma, ntgeo, num
    integer :: numele, numgrm, numgrn, numlti, numneu, numnod
!-----------------------------------------------------------------------
    parameter       (nbmtit  = 1  )
    parameter       (nbmgrp  = 2  )
    parameter       (nbmcoo  = 3  )
    parameter       (nbmmax  = 100)
    parameter       (nbmdbg  = 2  )
    parameter       (nbmint  = 7  )
!
    integer :: dimtit(nbmtit), dimgrp(nbmgrp)
    integer :: dimmai(nbmmax), dimcoo(nbmcoo)
    integer :: dimdbg(nbmdbg)
    integer :: nbttit(nbmtit), nbtgrp(nbmgrp)
    integer :: nbtmai(nbmmax), nbtcoo(nbmcoo)
    integer :: deblig
    integer :: fmtmai(nbmmax)
!
    real(kind=8) :: rv
!
    character(len=4) :: dimesp
    character(len=8) :: mcltit(nbmtit), mclgrp(nbmgrp)
    character(len=8) :: mclmai(nbmmax), mclcoo(nbmcoo)
    character(len=8) :: mcldbg(nbmdbg), mclint(nbmint)
    character(len=8) :: nom, nomn
    character(len=8) :: nom1
    character(len=14) :: cnl
    character(len=16) :: cmd, k16nom
    character(len=24) :: grpnov, grpmav, conxv, gpptnn, gpptnm
    character(len=24) :: valk(2), nomg, gpptnv, gpptmv
    character(len=24) :: nomdbg(50, nbmdbg)
    character(len=80) :: cv, dat
!
    common          /opmail/        cmd
!
    data dat        /' '/
    data mcltit     /'TITRE   '/
    data mclgrp     /'GROUP_NO','GROUP_MA'/
    data mclcoo     /'COOR_1D ','COOR_2D ','COOR_3D '/
    data mcldbg     /'DUMP    ','DEBUG   '/
    data mclint     /'GROUP_FA','SYS_UNIT','SYS_COOR','MACRO_AR',&
     &                 'MACRO_FA','MACRO_EL','MATERIAU'/
!
! --- INITIALISATION DU NB D'ERREUR
!
    ier = 0
    k16nom = ' '
    if (ulisop ( ifl, k16nom ) .eq. 0) then
        call ulopen(ifl, ' ', ' ', 'NEW', 'O')
    endif
!
    call jemarq()
!
    call sdmail(nomu, nommai, nomnoe, cooval, coodsc,&
                cooref, grpnoe, gpptnn, grpmai, gpptnm,&
                connex, titre, typmai, adapma)
!
!  1  CONSTRUCTION DES NOMS JEVEUX POUR L OBJET-MAILLAGE
!     --------------------------------------------------
!
!
    conxv = nomu//'.CONXV'
    grpnov = nomu//'.GROUPNOV'
    grpmav = nomu//'.GROUPMAV'
    gpptnv = nomu//'.PTRNOMNOV'
    gpptmv = nomu//'.PTRNOMMAV'
!
!
!  2  PREMIERE LECTURE DE DIMENSIONNEMENT DES OBJETS
!     ----------------------------------------------
!
    nbcoor = 0
    nbmail = 0
    nbnoma = 0
    nbg = -1
!
    do i = 1, nbmtit
        nbttit(i) = 0
        dimtit(i) = 0
    end do
    do i = 1, nbmgrp
        nbtgrp(i) = 0
        dimgrp(i) = 0
    end do
    do i = 1, nbmcoo
        nbtcoo(i) = 0
        dimcoo(i) = 0
    end do
    do i = 1, nbmmax
        fmtmai(i) = 0
        nbtmai(i) = 0
        dimmai(i) = 0
    end do
    do i = 1, nbmdbg
        dimdbg(i) = 0
    end do
!
! -     LECTURE DES NOMS/NBNO DES TYPES DE MAILLES DANS LE CATALOGUE
!
    call jelira('&CATA.TM.NOMTM', 'NOMMAX', nbmmai)
    if (nbmmai .gt. nbmmax) then
        call utmess('F', 'MODELISA5_1')
    endif
    do i = 1, nbmmai
        call jenuno(jexnum('&CATA.TM.NOMTM', i), mclmai(i))
        call jeveuo(jexnum('&CATA.TM.NBNO' , i), 'L', inbn)
        fmtmai(i) = zi(inbn)
    end do
!
! -   LECTURE PREMIER ITEM  EN DEBUT DE LIGNE SUIVANTE
!
  9 continue
    deblig = -1
    call liritm(ifl, icl, iv, rv, cv,&
                cnl, deblig, 1)
!
! -   ITEM = MOT CLE FIN , FINSF , AUTRE ?
!
    call tesfin(icl, iv, cv, irtet)
    if (irtet .eq. 1) then
        goto 8
    else if (irtet .eq. 2) then
        goto 9
    endif
!
! -   PREMIERE LECTURE DES DONNEES POUR CHAQUE TYPE DE MOT CLE
!
    call lecdbg(ifl, icl, iv, rv, cv,&
                cnl, mcldbg, nbmdbg, nbg, dimdbg,&
                nomdbg, irtet)
    if (irtet .eq. 1) then
        goto 8
    else if (irtet .eq. 2) then
        goto 9
    endif
!
    call lectit(ifl, icl, iv, rv, cv,&
                cnl, mcltit, nbmtit, nbg, dimtit,&
                nbttit, irtet)
    if (irtet .eq. 1) then
        goto 8
    else if (irtet .eq. 2) then
        goto 9
    endif
!
    call lecgrp(ifl, icl, iv, rv, cv,&
                cnl, mclgrp, nbmgrp, nbg, dimgrp,&
                nbtgrp, ier, irtet)
    if (irtet .eq. 1) then
        goto 8
    else if (irtet .eq. 2) then
        goto 9
    endif
!
    call leccoo(ifl, icl, iv, rv, cv,&
                cnl, mclcoo, nbmcoo, nbg, dimcoo,&
                nbtcoo, ier, irtet)
    if (irtet .eq. 1) then
        goto 8
    else if (irtet .eq. 2) then
        goto 9
    endif
!
    call lecmai(ifl, icl, iv, rv, cv,&
                cnl, mclmai, nbmmai, nbg, fmtmai,&
                dimmai, nbtmai, ier, irtet)
    if (irtet .eq. 1) then
        goto 8
    else if (irtet .eq. 2) then
        goto 9
    endif
!
    ASSERT(.false.)


!
! -   DIMENSIONS GLOBALES DES OBJETS
!
! -   DIMENSION ESPACE        (NB COORDONNEES)
!
  8 continue
    do i = 1, nbmcoo
        if(dimcoo(i).ne.0)nbcoor = i
    end do
!
! -   DIMENSION CONNEX        (NB MAILLES)
!
    do i = 1, nbmmai
        nbmail = nbmail + dimmai(i)
    end do
!
! -   DIMENSION COORDO        (NB NOEUDS)
!
    if (nbcoor .eq. 0) nbnoeu = 0
    if (nbcoor .ne. 0) nbnoeu = dimcoo(nbcoor)
!
! -   DIMENSION GROUPENO      (NB DE GROUPES DE NOEUDS)
!
    nbgrno = dimgrp(1)
!
! -    DIMENSION GROUPEMA     (NB DE GROUPES DE MAILLES)
!
    nbgrma = dimgrp(2)
!
! -   DIMENSION TITRE (NB DE LIGNES DU TITRE)
!
    nbltit = dimtit(1)
!
! -   NOMBRE TOTAL DE NOEUDS LUS DANS LE TYPE MAILLE
!
    do i = 1, nbmmai
        nbnoma = nbnoma + nbtmai(i)
    end do
!
! -   NOMBRE TOTAL DE NOEUDS LUS DANS LES GROUPES NOEUDS
!
    nbnogn = nbtgrp(1)
!
! -   NOMBRE TOTAL DE MAILLES LUES DANS  LES GROUPES MAILLES
!
    nbmagm = nbtgrp(2)
!
! -   DEBUG
!
    if (nbg .ge. 0) then
        write(ifm,*)'NBCOOR = ',nbcoor
        write(ifm,*)'NBMAIL = ',nbmail
        write(ifm,*)'NBNOEU = ',nbnoeu
        write(ifm,*)'NBGRNO = ',nbgrno
        write(ifm,*)'NBGRMA = ',nbgrma
        write(ifm,*)'NBLTIT = ',nbltit
        write(ifm,*)'NBNOMA = ',nbnoma
        write(ifm,*)'NBNOGN = ',nbnogn
        write(ifm,*)'NBMAGM = ',nbmagm
        write(ifm,*)' '
        do i = 1, nbmcoo
            write(ifm,*)'DIMCOO ',i,' = ',dimcoo(i)
        end do
        do i = 1, nbmmai
            write(ifm,*)'DIMMAI ',i,' = ',dimmai(i)
        end do
        do i = 1, nbmgrp
            write(ifm,*)'DIMGRP ',i,' = ',dimgrp(i)
        end do
        do i = 1, nbmtit
            write(ifm,*)'DIMTIT ',i,' = ',dimtit(i)
        end do
        write(ifm,*)'-------- FIN DEBUG -------------------'
    endif
!
! -   FIN  DE LECTURE DU FICHIER
!
    if (nbnoeu .eq. 0) then
        call utmess('F', 'MODELISA5_2')
        ier = 1
    endif
    if (nbmail .eq. 0) then
        call utmess('F', 'MODELISA5_3')
        ier = 1
    endif
!
    if (ier .eq. 1) then
        call utmess('F', 'MODELISA4_94')
    endif
!
!
!  3  CREATION  DES OBJETS TEMPORAIRES A TRANSCODER SUR LA VOLATILE
!     ET DES OBJETS PERMANENTS NON TRANSCODABLES SUR LA GLOBALE
!     -------------------------------------------------------------
!
! -   OBJET NOMMAI    = REPERTOIRE NOMS DE MAILLES  K8 SUR GLOBALE
!
    call jecreo(nommai, 'G N K8')
    call jeecra(nommai, 'NOMMAX', nbmail)
!
! -   OBJET NOMNOE    = REPERTOIRE NOMS DE NOEUDS K8 SUR GLOBALE
!
    call jecreo(nomnoe, 'G N K8')
    call jeecra(nomnoe, 'NOMMAX', nbnoeu)
!
!
    nbgrmp = nbgrma
    nbgrnp = nbgrno
!
! -   OBJET TITRE             = VECTEUR DE K80
!
    call jecreo(titre, 'G V K80')
    if (nbltit .ne. 0) then
        call jeecra(titre, 'LONMAX', nbltit)
    else
        nbltit = 1
        call jeecra(titre, 'LONMAX', 1)
        call jeveuo(titre, 'E', iad)
        call enlird(dat)
        zk80(iad)=dat
    endif
!
! -   OBJET TYPMAIL   = FAMILLE CONTIGUE D'ELEMENTS I (NUM TYPE ELE)
!                       POINTEUR DE NOM       = NOMMAI
!
    call wkvect(typmai, 'G V I', nbmail, ibid)
!
! -   CHAMPS DE GEOMETRIE AUX NOEUDS
!
! -   OBJET COORDO.VALE = VECTEUR DE R8 CONTENANT LES COORDONNEES
!
    call jecreo(cooval, 'G V R')
    call jeecra(cooval, 'LONMAX', nbnoeu*3)
    call codent(nbcoor, 'G', dimesp)
    call jeecra(cooval, 'DOCU', cval=dimesp)
!
! -   OBJET COORDO.DESC = VECTEUR 3*IS DESCRIPTEUR DU CHAMP
!
! -   RECUPERATION DU NUMERO IDENTIFIANT LE TYPE DE CHAM_NO GEOMETRIE
!
    call jenonu(jexnom('&CATA.GD.NOMGD', 'GEOM_R'), ntgeo)
!
    call jecreo(coodsc, 'G V I')
    call jeecra(coodsc, 'LONMAX', 3)
    call jeecra(coodsc, 'DOCU', 0, 'CHNO')
    call jeveuo(coodsc, 'E', iad)
    zi(iad) = ntgeo
    zi(iad+1) = -3
    zi(iad+2) = 14
!
! -   OBJET COORDO.REFE = VECTEUR 2*K24 NOM DU MAILLAGE !!!
!
    call wkvect(cooref, 'G V K24', 4, iad)
    zk24(iad) = nomu
!
! -   OBJET GROUPNOV  = FAMILLES CONTIGUES DE VECTEURS N*K8 VOLATILE
!                       POINTEUR DE NOM       = GROUPNOV.$$NOM
!                       POINTEUR DE LONGUEUR  = GROUPNOV.$$LONC
!                       LONGUEUR TOTALE       = NBNOGN
!
    if (nbgrno .ne. 0) then
        call jecreo(gpptnv, 'V N K24')
        call jeecra(gpptnv, 'NOMMAX', nbgrno)
        call jecrec(grpnov, 'V V K8', 'NO '//gpptnv, 'CONTIG', 'VARIABLE',&
                    nbgrno)
        call jeecra(grpnov, 'LONT', nbnogn)
    endif
!
! -      OBJET GROUPMAV = FAMILLE CONTIGUE DE VECTEURS N*K8 VOLATILE
!                         POINTEUR DE NOM       = GROUPMAV.$$NOM
!                         POINTEUR DE LONGUEUR  = GROUPMAV.$$LONC
!                         LONGUEUR TOTALE       = NBMAGM
!
    if (nbgrma .ne. 0) then
        call jecreo(gpptmv, 'V N K24')
        call jeecra(gpptmv, 'NOMMAX', nbgrma)
        call jecrec(grpmav, 'V V K8', 'NO '//gpptmv, 'CONTIG', 'VARIABLE',&
                    nbgrma)
        call jeecra(grpmav, 'LONT', nbmagm)
    endif
!
! -     OBJET CONXV     = FAMILLE CONTIGUE DE VECTEURS N*K8 VOLATILE
!                         POINTEUR DE NOM       = CONXV.$$NOM
!                         POINTEUR DE LONGUEUR  = CONXV.$$LONC
!                         LONGUEUR TOTALE       = NBNOMA
!
    call jecrec(conxv, 'V V K8', 'NO', 'CONTIG', 'VARIABLE',&
                nbmail)
    call jeecra(conxv, 'LONT', nbnoma)
!
! -   OBJET GROUPENO   = FAMILLE DISPERSEE DE VECTEURS N*IS
!                       POINTEUR DE LONGUEUR  = GROUPENO.$$LONC
!                       LONGUEUR TOTALE       = NBNOGN
!
    if (nbgrno .ne. 0) then
        call jecreo(gpptnn, 'G N K24')
        call jeecra(gpptnn, 'NOMMAX', nbgrnp)
        call jecrec(grpnoe, 'G V I', 'NO '//gpptnn, 'DISPERSE', 'VARIABLE',&
                    nbgrnp)
    endif
!
! -   OBJET GROUPEMA  = FAMILLE CONTIGUE DE VECTEURS N*IS
!                       POINTEUR DE LONGUEUR  = GROUPEMA.$$LONC
!                       LONGUEUR TOTALE       = NBMAGM
!
    if (nbgrma .ne. 0) then
        call jecreo(gpptnm, 'G N K24')
        call jeecra(gpptnm, 'NOMMAX', nbgrmp)
        call jecrec(grpmai, 'G V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE',&
                    nbgrmp)
    endif
!
! -   OBJET CONNEX    = FAMILLE CONTIGUE DE VECTEURS N*IS
!                       POINTEUR DE NOM       = NOMMAI
!                       POINTEUR DE LONGUEUR  = CONNEX.$$LONC
!                       LONGUEUR TOTALE       = NBNOMA
!
    call jecrec(connex, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbmail)
    call jeecra(connex, 'LONT', nbnoma)
!
! -   OBJET ADAPMA   = INFORMATION SUR L'ADAPTATION DE MAILLAGE
!
    call wkvect(adapma, 'G V I', 1, iad)
    zi(iad) = 0
!
!
!  4  SECONDE LECTURE DU FICHIER MAILLAGE POUR STOCKAGE DES OBJETS
!     ------------------------------------------------------------
!
!     REMBOBINAGE DU FICHIER
!
    rewind(ifl)
!
! -   LECTURE PREMIER ITEM EN DEBUT DE LIGNE
!
    numlti = 0
    numneu = 0
    numele = 0
    numnod = 0
    numgrn = 0
    numgrm = 0
!
900 continue
    deblig = -1
    call liritm(ifl, icl, iv, rv, cv,&
                cnl, deblig, 2)
!
! -   ITEM = MOT CLE  FIN OU FINSF OU AUTRE ?
!
    call tesfin(icl, iv, cv, irtet)
    if (irtet .eq. 1) then
        goto 800
    else if (irtet .eq. 2) then
        goto 900
    endif
!
! -   STOCKAGE DES DONNEES POUR CHAQUE TYPE DE  MOT CLE
!
    call stktit(ifl, icl, iv, rv, cv,&
                cnl, mcltit, nbmtit, numlti, titre,&
                irtet)
    if (irtet .eq. 1) then
        goto 800
    else if (irtet .eq. 2) then
        goto 900
    endif
!
    call stkcoo(ifl, icl, iv, rv, cv,&
                cnl, mclcoo, nbmcoo, numneu, cooval,&
                nomnoe, irtet)
    if (irtet .eq. 1) then
        goto 800
    else if (irtet .eq. 2) then
        goto 900
    endif
!
    call stkgrp(ifl, icl, iv, rv, cv,&
                cnl, mclgrp, nbmgrp, numgrn, numgrm,&
                grpnov, grpmav, irtet)
    if (irtet .eq. 1) then
        goto 800
    else if (irtet .eq. 2) then
        goto 900
    endif
!
    call stkmai(ifl, icl, iv, rv, cv,&
                cnl, mclmai, nbmmai, numele, numnod,&
                conxv, typmai, fmtmai, irtet)
    if (irtet .eq. 1) then
        goto 800
    else if (irtet .eq. 2) then
        goto 900
    endif
!
    goto 900
800 continue
!
!
!
!
!  6  TRANSCODAGE EN REPRESENTATION INTERNE ET STOCKAGE
!     -------------------------------------------------
!
! -   TRANSCODAGE DE CONNEX
!
    do i = 1, nbmail
        call jenuno(jexnum(conxv, i), nomn)
        call jeveuo(jexnum(conxv, i), 'L', jvcnx)
        call jelira(jexnum(conxv, i), 'LONMAX', nbno)
        call jenonu(jexnom(nomu//'.NOMMAI', nomn), ibid)
        call jeecra(jexnum(connex, ibid), 'LONMAX', nbno)
        call jeveuo(jexnum(connex, ibid), 'E', jgcnx)
        do j = 1, nbno
            nom = zk8(jvcnx+j-1)
            call jenonu(jexnom(nomnoe, nom), num)
            zi(jgcnx+j-1) = num
            if (num .eq. 0) then
                valk(1) = nom
                valk(2) = nomn
                call utmess('F', 'MODELISA5_4', nk=2, valk=valk)
                ier = 1
            endif
        end do
    end do
!
!
! -     TRANSCODAGE DE GROUPENO
!
    if (nbgrno .ne. 0) then
        call wkvect('&&OP0001.NOEUD', 'V V I', nbnoeu, jnoeu)
        call wkvect('&&OP0001.NOEUD2', 'V V I', nbnoeu, jnoeu2)
        do i = 1, nbgrno
!
!         REMISE A ZERO DE L'OBJET "&&OP001.NOEUD2" :
            do ii = 1, nbnoeu
                zi(jnoeu2-1+ii)=0
            end do
!
            call jenuno(jexnum(grpnov, i), nomg)
            call jeveuo(jexnum(grpnov, i), 'L', jvg)
            call jelira(jexnum(grpnov, i), 'LONUTI', nbno)
!         --- ON VERIFIE QUE TOUS LES NOEUDS SONT DISTINCTS ---
            nbno1 = 0
            do im1 = 1, nbno
                nom1 = zk8(jvg+im1-1)
                ASSERT(nom1.ne.' ')
                call jenonu(jexnom(nomnoe, nom1), num)
                if (num .eq. 0) then
                    ier = ier + 1
                    valk(1) = nom1
                    valk(2) = nomg
                    call utmess('F', 'MODELISA5_5', nk=2, valk=valk)
                    goto 610
                endif
                zi(jnoeu2-1+num)=zi(jnoeu2-1+num)+1
                if (zi(jnoeu2-1+num) .ge. 2) then
                    valk(1) = nom1
                    valk(2) = nomg
                    call utmess('A', 'MODELISA5_6', nk=2, valk=valk)
                    goto 610
                endif
                nbno1 = nbno1 + 1
                zi(jnoeu+nbno1-1) = num
610             continue
            end do
            call jecroc(jexnom(grpnoe, nomg))
            call jeecra(jexnom(grpnoe, nomg), 'LONMAX', max(nbno1, 1))
            call jeecra(jexnom(grpnoe, nomg), 'LONUTI', nbno1)
            call jeveuo(jexnom(grpnoe, nomg), 'E', jgg)
            zi(jgg)=-ismaem()
            do j = 0, nbno1-1
                zi(jgg+j) = zi(jnoeu+j)
            end do
        end do
        call jedetr('&&OP0001.NOEUD')
        call jedetr('&&OP0001.NOEUD2')
    endif
!
!
! -     TRANCODAGE DE GROUPEMA
!
    if (nbgrma .ne. 0) then
        call wkvect('&&OP0001.MAILLE', 'V V I', nbmail, jmail)
        call wkvect('&&OP0001.MAILLE2', 'V V I', nbmail, jmail2)
        do i = 1, nbgrma
!
!         REMISE A ZERO DE L'OBJET "&&OP001.MAILLE2" :
            do ii = 1, nbmail
                zi(jmail2-1+ii)=0
            end do
            call jenuno(jexnum(grpmav, i), nomg)
            call jeveuo(jexnum(grpmav, i), 'L', jvg)
            call jelira(jexnum(grpmav, i), 'LONUTI', nbma)
!         --- ON VERIFIE QUE TOUTES LES MAILLES SONT DISTINCTES ---
            nbma1 = 0
            do im1 = 1, nbma
                nom1 = zk8(jvg+im1-1)
                call jenonu(jexnom(nommai, nom1), num)
                if (num .eq. 0) then
                    ier = ier + 1
                    valk(1) = nom1
                    valk(2) = nomg
                    call utmess('F', 'MODELISA5_7', nk=2, valk=valk)
                    goto 710
                endif
                zi(jmail2-1+num)=zi(jmail2-1+num)+1
                if (zi(jmail2-1+num) .ge. 2) then
                    valk(1) = nom1
                    valk(2) = nomg
                    call utmess('A', 'MODELISA5_8', nk=2, valk=valk)
                    goto 710
                endif
                nbma1 = nbma1 + 1
                zi(jmail+nbma1-1) = num
710             continue
            end do
            call jecroc(jexnom(grpmai, nomg))
            call jeecra(jexnom(grpmai, nomg), 'LONMAX', max(nbma1, 1))
            call jeecra(jexnom(grpmai, nomg), 'LONUTI', nbma1)
            call jeveuo(jexnom(grpmai, nomg), 'E', jgg)
            zi(jgg)=-ismaem()
            do j = 0, nbma1-1
                zi(jgg+j) = zi(jmail+j)
            end do
        end do
        call jedetr('&&OP0001.MAILLE')
        call jedetr('&&OP0001.MAILLE2')
    endif
!
! -     FIN DE TRANSCODAGE
!
    if (ier .ne. 0) then
        call utmess('F', 'MODELISA5_9')
    endif
!
! -     DUMP DES OBJETS DEMANDES
!
    if (dimdbg(1) .ne. 0) then
        do j = 1, dimdbg(1)
            call jeexin(nomdbg(j, 1), iret)
            if (iret .gt. 0) then
                call jeimpo(ifm, nomdbg(j, 1), 'DUMP DE '//nomdbg(j, 1))
            endif
        end do
    endif
!
! -   MENAGE
!
    call jedetr(nomu//'.CONXV')
    call jedetr(nomu//'.GROUPNOV')
    call jedetr(nomu//'.GROUPMAV')
    call jedetr(gpptnv)
    call jedetr(gpptmv)
!
! FERMETURE DU FICHIER
!
    call ulopen(-ifl, ' ', ' ', ' ', ' ')
!
!
    call jedema()
end subroutine
