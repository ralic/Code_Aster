subroutine lrmgrp(grpnoe, gpptnn, nbgrno, jnogno, jlggno,&
                  grpmai, gpptnm, nbgrma, jnogma, jlggma,&
                  nomgro, numgro, nument, nbfam)
!
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
!     CREATION DES GROUPES DE NOEUDS ET DE MAILLES
!    -------------------------------------------------------------------
!
! ENTREES :
!   GRPNOE : NOM DE L'OBJET QUI CONTIENT LES GROUPES DE NOEUDS
!   NBGRNO : NOMBRE DE GROUPES DE NOEUDS
!   JNOGNO : POINTEUR SUR LA LISTE DES NOMS DE CHAQUE GROUPE DE
!            NOEUDS A CREER
!   JLGGNO : POINTEUR SUR LA TAILLE DE CHAQUE GROUPE DE
!            NOEUDS A CREER
!   GRPMIA : NOM DE L'OBJET QUI CONTIENT LES GROUPES DE NOEUDS
!   NBGRMA : NOMBRE DE GROUPES DE MAILLES
!   JNOGMA : POINTEUR SUR LA LISTE DES NOMS DE CHAQUE GROUPE DE
!            MAILLES A CREER
!   JLGGMA : POINTEUR SUR LA TAILLE DE CHAQUE GROUPE DE
!            MAILLES A CREER
!   NOMGRO : COLLECTION DES NOMS DES GROUPES A CREER
!   NUMGRO : COLLECTION DES NUMEROS DES GROUPES A CREER
!   NUMENT : COLLECTION DES NUMEROS DES ENTITES DANS LES GROUPES
!   NBFAM  : NOMBRE DE FAMILLES A EXPLORER
! ======================================================================
!            ATTENTION : CET ALGORITHME A ETE OPTIMISE LE 16/10/2002
!                        ETRE PRUDENT DANS LES AMELIORATIONS FUTURES ...
!                        LES SITUATIONS PENALISANTES SONT CELLES-CI :
!                        QUELQUES DIZAINES DE MILLIERS DE NOEUDS OU DE
!                        MAILLES ET QUELQUES CENTAINES DE GROUPES
!                        EXEMPLE : ON EST PASSE DE 6,5 JOURS A 1MN40S
!                        AVEC UN GROS MAILLAGE :
!                        . 286 017 NOEUDS EN 2 GROUPES ET
!                        . 159 130 MAILLES EN 5 624 GROUPES.
!
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/jucroc.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbgrno, jnogno, jlggno
    integer :: nbgrma, jnogma, jlggma
    integer :: nbfam
    character(len=*) :: nomgro, numgro, nument
    character(len=24) :: grpnoe, grpmai, gpptnn, gpptnm
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'LRMGRP' )
!
    integer :: ifm, nivinf
!
    integer :: jgrp, jfnomg, jfnumg, jfnum
    integer :: igrp, ifam, iret, nbg, naux, iaux, ig
    integer :: adnogn, adadgn
    integer :: adnogm, adadgm
    integer :: adnogr, adadgr, nbgr, nugrm1
!
    character(len=24) :: ntnogn, ntadgn
    character(len=24) :: ntnogm, ntadgm
    character(len=24) :: nomgrp
    character(len=32) :: nomj
!
!====
! 1. PREALABLES
!====
!
! 1.1. ==> RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, nivinf)
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'DEBUT DE '//nompro
    endif
    1001 format(/,10('='),a,10('='),/)
!
! 1.2. ==> NOMS DES TABLEAUX DE TRAVAIL
!               12   345678   9012345678901234
    ntnogn = '&&'//nompro//'.GRPNO_DEJA_VUS_'
    ntadgn = '&&'//nompro//'.ADRESSE_GRPNO__'
    ntnogm = '&&'//nompro//'.GRPMA_DEJA_VUS_'
    ntadgm = '&&'//nompro//'.ADRESSE_GRPMA__'
!
!====
! 2. CREATION DES STRCUTURES GENERALES DES GROUPES
!    CREATION DES TABLEAUX DE TRAVAIL
!    CREATION DES GROUPES
!====
!
! 2.1. ==> POUR LES NOEUDS
!
    if (nbgrno .gt. 0) then
!
        call jecreo(gpptnn, 'G N K24')
        call jeecra(gpptnn, 'NOMMAX', nbgrno)
        call jecrec(grpnoe, 'G V I', 'NO '//gpptnn, 'DISPERSE', 'VARIABLE',&
                    nbgrno)
        call wkvect(ntnogn, 'V V K24', nbgrno, adnogn)
        call wkvect(ntadgn, 'V V I', nbgrno, adadgn)
!
        do 21 , igrp = 0 , nbgrno-1
!
        iaux = zi (jlggno+igrp)
        if (iaux .gt. 0) then
            nomgrp = zk24(jnogno+igrp)
            call jucroc(grpnoe, nomgrp, 0, iaux, jgrp)
            zk24(adnogn+igrp) = nomgrp
            zi (adadgn+igrp) = jgrp
        endif
!
21      continue
!
    endif
!
! 2.2. ==> POUR LES MAILLES
!
    if (nbgrma .gt. 0) then
!
        call jecreo(gpptnm, 'G N K24')
        call jeecra(gpptnm, 'NOMMAX', nbgrma)
        call jecrec(grpmai, 'G V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE',&
                    nbgrma)
        call wkvect(ntnogm, 'V V K24', nbgrma, adnogm)
        call wkvect(ntadgm, 'V V I', nbgrma, adadgm)
!
        do 22 , igrp = 0 , nbgrma-1
!
        iaux = zi (jlggma+igrp)
        if (iaux .gt. 0) then
            nomgrp = zk24(jnogma+igrp)
            call jucroc(grpmai, nomgrp, 0, iaux, jgrp)
            zk24(adnogm+igrp) = nomgrp
            zi (adadgm+igrp) = jgrp
        endif
!
22      continue
!
    endif
!
!====
! 3. ON EXPLORE TOUTES LES FAMILLES
!====
!
    do 30 , ifam = 1 , nbfam
!
    nomj = jexnum(nomgro,ifam)
    call jeexin(nomj, iret)
!
    if (iret .gt. 0) then
!
! 3.1. ==> POINTEURS
!          NBG : NOMBRE DE GROUPES DEFINISSANT LA FAMILLE
!          JFNUMG : POINTEUR SUR LE NUMERO DU GROUPE . PERMET DE
!                   TRIER ENTRE LES GROUPES DE NOEUDS ET DE MAILLE
!          JFNUM  : POINTEUR SUR LE CONTENU DE LA FAMILLE
!
        call jeveuo(nomj, 'L', jfnomg)
        call jelira(nomj, 'LONMAX', nbg)
!
        nomj = jexnum(numgro,ifam)
        call jeveuo(nomj, 'L', jfnumg)
!
        nomj = jexnum(nument,ifam)
        call jeveuo(nomj, 'L', jfnum)
        call jelira(nomj, 'LONMAX', naux)
!
! 3.2. ==> ON BOUCLE SUR LES NBG GROUPES DEFINISSANT LA FAMILLE
!          CHAQUE GROUPE EST CONNU PAR SON NOM.
!          ON TRANSFERE LA LISTE DES ENTITES DU TABLEAU DEFINI PAR
!          FAMILLE VERS LE TABLEAU DU GROUPE AU SENS ASTER
!
        do 32 , ig = 1 , nbg
!
        nomgrp = zk24(jfnomg+ig-1)
!
! 3.2.1. ==> REPERAGE DU TYPE
!
        if (zi(jfnumg+ig-1) .gt. 0) then
            adnogr = adnogn
            adadgr = adadgn
            nbgr = nbgrno
        else
            adnogr = adnogm
            adadgr = adadgm
            nbgr = nbgrma
        endif
!
! 3.2.2. ==> RECHERCHE DE L'ADRESSE OU ON DOIT COMMENCER D'ECRIRE LA
!            LISTE DES ELEMENTS DU GROUPE
!
        do 322 , iaux = 0 , nbgr-1
        if (zk24(adnogr+iaux) .eq. nomgrp) then
            jgrp = zi(adadgr+iaux)
            nugrm1 = iaux
            goto 3221
        endif
322      continue
!
        call utmess('F', 'MED_20', sk=nomgrp)
!
3221      continue
!
! 3.2.3. ==> TRANSFERT
!
        do 323 , iaux = 0 , naux-1
        zi(jgrp) = zi(jfnum+iaux)
        jgrp = jgrp + 1
323      continue
!
! 3.2.4. ==> MEMORISATION DE LA NOUVELLE ADRESSE
!
        zi(adadgr+nugrm1) = jgrp
!
32      continue
!
    endif
!
    30 end do
!
!====
! 4. LA FIN
!====
!
! --- MENAGE
    call jedetr('&&'//nompro//'.GRPNO_DEJA_VUS_')
    call jedetr('&&'//nompro//'.ADRESSE_GRPNO__')
    call jedetr('&&'//nompro//'.GRPMA_DEJA_VUS_')
    call jedetr('&&'//nompro//'.ADRESSE_GRPMA__')
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'FIN DE '//nompro
    endif
!
end subroutine
