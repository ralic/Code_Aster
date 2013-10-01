subroutine lisdef(oper, optkz, opti, valkz, vali)
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
!
    implicit none
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/iscode.h"
#include "asterfort/isdeco.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/liscva.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: valkz, optkz
    character(len=4) :: oper
    integer :: vali(2), opti
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! ROUTINE CENTRALE POUR LES CARACTERISTIQUES DES CHARGES
!
!
! ----------------------------------------------------------------------
!
!
! IN  OPER   : TYPE DE DEMANDE
!              'IDGE' - IDENTIFICATION DES GENRES D'UNE CHARGE
!              'IDMC' - IDENTIFICATION DES MOTS-CLEFS D'UNE CHARGE
!              'TYPC' - RETOURNE LE TYPE DE LA CHARGE (REEL, COMP, FONC)
!              'OBJE' - NOM DE L'OBJET DEFINI DANS AFFE_CHAR_*
!                       (EN GENERAL, C'EST UNE CARTE)
!              'POSG' - POSITION DANS L'ENTIER CODE POUR UN GENRE DONNE
!              'POSM' - POSITION DANS L'ENTIER CODE POUR UN MOT-CLEF DONNE
!              'CART' - NOM DE LA CARTE POUR UN GENRE DONNE
!              'OPTI' - NOM DE L'OPTION DE CALCUL
!              'PARA' - NOM DU PARAMETRE POUR LE CALCUL
!              'IDNS' - LISTE DES INDXCH DES CHARGES
!              'LIGC' - RETOURNE LE TYPE DU LIGREL DE CALCUL
!              'MOTC' - RETOURNE LE MOT-CLEF DE L'INDEX DONNE
!              'GENR' - RETOURNE LE GENRE DE LA POSITION DANS L'ENTIER
!                       CODE DONNE
!              'LISG' - LISTE DES GENRES DISPONIBLES
! IN  OPTI   : OPTION (ENTIER)
!              'IDGE' - INUTILISE
!              'IDMC' - INUTILISE
!              'OBJE' - INDEX DE LA CHARGE
!              'TYPC' - CODE (ENTIER CODE) CONTENANT LES GENRES
!              'POSG' - INUTILISE
!              'POSM' - INUTILISE
!              'CART' - INUTILISE
!              'OPTI' - INDEX DE LA CHARGE
!              'PARA' - INDEX DE LA CHARGE
!              'IDNS' - POSITION DANS L'ENTIER CODE
!              'LIGC' - INDEX DE LA CHARGE
!              'MOTC' - INUTILISE
!              'GENR' - POSITION DANS L'ENTIER CODE
!              'LISG' - INUTILISE
! IN  OPTK   : OPTION (CHAINE)
!              'IDGE' - PREFIXE DE L'OBJET
!              'IDMC' - PREFIXE DE L'OBJET
!              'OBJE' - PREFIXE DE L'OBJET
!              'TYPC' - PREFIXE DE L'OBJET
!              'POSG' - GENRE DE LA CHARGE
!              'POSM' - MOT-CLEF
!              'CART' - GENRE DE LA CHARGE
!              'OPTI' - TYPE DE LA CHARGE (REEL/COMP/FONC)
!              'PARA' - TYPE DE LA CHARGE (REEL/COMP/FONC)
!              'IDNS' - NOM DE L'OBJET JEVEUX A CREER
!              'LIGC' - INUTILISE
!              'MOTC' - MOT-CLEF A REPERER
!              'GENR' - INUTILISE
!              'LISG' - NOM DE L'OBJET JEVEUX A CREER
! OUT VALI   : REPONSE (ENTIER)
!              'IDGE' - CODE (ENTIER CODE) CONTENANT LES GENRES
!              'IDMC' - CODE (ENTIER CODE) CONTENANT LES MOTS-CLEFS
!              'OBJE' - 1 SI L'OBJET EST UNE CARTE
!              'TYPC' - INUTILISE
!              'POSG' - POSITION DANS L'ENTIER CODE
!              'POSM' - POSITION DANS L'ENTIER CODE
!              'CART' - 1 SI L'OBJET EST UNE CARTE, 0 SINON
!              'OPTI' - INUTILISE
!              'PARA' - INUTILISE
!              'IDNS' - NOMBRE DE CHARGES
!              'LIGC' - INUTILISE
!              'MOTC' - INDXCH DE LA CHARGE
!              'GENR' - INUTILISE
!              'LISG' - NOMBRE DE GENRES DISPONIBLES
! OUT VALK   : REPONSE (CHAINE)
!              'IDGE' - INUTILISE
!              'IDMC' - INUTILISE
!              'OBJE' - NOM DE L'OBJET
!              'TYPC' - TYPE DE LA CHARGE
!                     'REEL'    - CHARGE CONSTANTE REELLE
!                     'COMP'    - CHARGE CONSTANTE COMPLEXE
!                     'FONC_F0' - CHARGE FONCTION QUELCONQUE
!                     'FONC_FT' - CHARGE FONCTION DU TEMPS
!              'POSG' - INUTILISE
!              'POSM' - INUTILISE
!              'CART' - NOM DE LA CARTE DE l'OBJET
!              'OPTI' - NOM DE L'OPTION DE CALCUL
!              'PARA' - NOM DU PARAMETRE POUR LE CALCUL
!              'IDNS' - INUTILISE
!              'LIGC' - TYPE DU LIGREL DE CALCUL: LIGRCH OU LIGRMO
!              'MOTC' - INUTILISE
!              'GENR' - NOM DU GENRE
!              'LISG' - INUTILISE
!
! ----------------------------------------------------------------------
!
    integer :: nbtyth
    parameter   (nbtyth = 28)
    character(len=6) :: nomob(nbtyth)
    character(len=24) :: motcl(nbtyth)
    character(len=24) :: genre(nbtyth)
    integer :: gencod(nbtyth), mcfcod(nbtyth)
    character(len=16) :: optiof(nbtyth), optior(nbtyth), optioc(nbtyth)
    character(len=8) :: paraf(nbtyth), parar(nbtyth), parac(nbtyth)
    character(len=6) :: typlig(nbtyth)
!
    integer :: indxch, iret, genrec(1), ibid, iposit, nbch, i, itypob
    character(len=16) :: option, typeco
    character(len=24) :: typcha, gencha, nomobj, parcha, genold, motcle
    character(len=8) :: charge, typech, lpain, nomgd
    integer :: tabcod(30), tabcox(60), idd, index2, iexi, motclc(2)
    character(len=19) :: carte, chamno
    character(len=24) :: liscns
    character(len=6) :: ligcal, nomcar
    integer :: jlisci, jlisck
    logical :: lfirst, ldoub
    logical :: lveas, lveac, lveag
    character(len=13) :: prefob
!
! --- OBJETS DEFINISSANT LES CHARGEMENTS: ON NE MET QUE CEUX DEFINIS
! --- PAR UNE CARTE - AUTRES OBJETS: __*
!
    data nomob/&
     &     '.CIMPO'          ,'__ELIM'          ,&
     &     '.FORNO'          ,'.EPSIN'          ,'.SIINT'          ,&
     &     '.PRESS'          ,'.FLUX'           ,'.VNOR'           ,&
     &     '.IMPE'           ,'__EVOC'          ,'.PESAN'          ,&
     &     '.ROTAT'          ,'.SIGIN'          ,'.FELEC'          ,&
     &     '.FL1'            ,'.ONDE'           ,'.ONDPL'          ,&
     &     '.VEASS'          ,'.F1D2D'          ,'.F3D3D'          ,&
     &     '.F2D2D'          ,'.F1D3D'          ,'.F2D3D'          ,&
     &     '.F1D1D'          ,'.FCO3D'          ,'.FCO2D'          ,&
     &     '__VEAS'          ,'__VEAG'          /
!
! --- MOT_CLEF DEFINISSANT LE CHARGEMENT DANS AFFE_CHAR_*
!
    data motcl /&
     &     'DIRI_DUAL'       ,'DIRI_ELIM'       ,&
     &     'FORCE_NODALE'    ,'EPSI_INIT'       ,'SIGM_INTERNE'    ,&
     &     'PRES_REP'        ,'FLUX_THM_REP'    ,'VITE_FACE'       ,&
     &     'IMPE_FACE'       ,'EVOL_CHAR'       ,'PESANTEUR'       ,&
     &     'ROTATION'        ,'RELA_CINE_BP'    ,'FORCE_ELEC'      ,&
     &     'INTE_ELEC'       ,'ONDE_FLUI'       ,'ONDE_PLANE'      ,&
     &     'VECT_ASSE_CHAR'  ,'FORCE_CONTOUR'   ,'FORCE_INTERNE#3D',&
     &     'FORCE_INTERNE#2D','FORCE_ARETE'     ,'FORCE_FACE'      ,&
     &     'FORCE_POUTRE'    ,'FORCE_COQUE#3D'  ,'FORCE_COQUE#2D'  ,&
     &     'VECT_ASSE'       ,'VECT_ASSE_GENE'  /
!
! --- POSITION DANS L'ENTIER CODE POUR LE MOT_CLEF DE LA CHARGE
!
    data mcfcod /&
     &     01                ,02                ,&
     &     03                ,04                ,05                ,&
     &     06                ,07                ,08                ,&
     &     09                ,10                ,11                ,&
     &     12                ,13                ,14                ,&
     &     15                ,16                ,17                ,&
     &     18                ,19                ,20                ,&
     &     21                ,22                ,23                ,&
     &     24                ,25                ,26                ,&
     &     27                ,28                /
!
! --- GENRE DE LA CHARGE
!
    data genre /&
     &     'DIRI_DUAL'       ,'DIRI_ELIM'       ,&
     &     'NEUM_MECA'       ,'NEUM_MECA'       ,'SIGM_INTERNE'    ,&
     &     'NEUM_MECA'       ,'NEUM_MECA'       ,'VITE_FACE'       ,&
     &     'IMPE_FACE'       ,'EVOL_CHAR'       ,'NEUM_MECA'       ,&
     &     'NEUM_MECA'       ,'SIGM_CABLE'      ,'FORCE_ELEC'      ,&
     &     'INTE_ELEC'       ,'ONDE_FLUI'       ,'ONDE_PLANE'      ,&
     &     'VECT_ASSE_CHAR'  ,'NEUM_MECA'       ,'NEUM_MECA'       ,&
     &     'NEUM_MECA'       ,'NEUM_MECA'       ,'NEUM_MECA'       ,&
     &     'NEUM_MECA'       ,'NEUM_MECA'       ,'NEUM_MECA'       ,&
     &     'VECT_ASSE'       ,'VECT_ASSE_GENE'  /
!
! --- POSITION DANS L'ENTIER CODE POUR LE GENRE DE LA CHARGE
!
    data gencod /&
     &     02                ,01                ,&
     &     03                ,03                ,05                ,&
     &     03                ,03                ,06                ,&
     &     07                ,08                ,03                ,&
     &     03                ,09                ,14                ,&
     &     10                ,11                ,12                ,&
     &     13                ,03                ,03                ,&
     &     03                ,03                ,03                ,&
     &     03                ,03                ,03                ,&
     &     04                ,15                /
!
    data typlig /&
     &     'LIGRCH'          ,'LIGRCH'          ,&
     &     'LIGRCH'          ,'LIGRMO'          ,'LIGRMO'          ,&
     &     'LIGRMO'          ,'LIGRMO'          ,'LIGRMO'          ,&
     &     'LIGRMO'          ,'LIGRMO'          ,'LIGRMO'          ,&
     &     'LIGRMO'          ,'LIGRMO'          ,'LIGRMO'          ,&
     &     'LIGRMO'          ,'LIGRMO'          ,'LIGRMO'          ,&
     &     'LIGRMO'          ,'LIGRMO'          ,'LIGRMO'          ,&
     &     'LIGRMO'          ,'LIGRMO'          ,'LIGRMO'          ,&
     &     'LIGRMO'          ,'LIGRMO'          ,'LIGRMO'          ,&
     &     ' '               ,' '               /
!
!
! --- NOM DE L'OPTION - CAS REEL
!
    data optior /&
     &     'MECA_DDLI_R'     ,' '               ,&
     &     'CHAR_MECA_FORC_R','CHAR_MECA_EPSI_R',' '               ,&
     &     'CHAR_MECA_PRES_R','CHAR_MECA_FLUX_R',' '               ,&
     &     ' '               ,' '               ,'CHAR_MECA_PESA_R',&
     &     'CHAR_MECA_ROTA_R',' '               ,'CHAR_MECA_FRELEC',&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,'CHAR_MECA_FR1D2D','CHAR_MECA_FR3D3D',&
     &     'CHAR_MECA_FR2D2D','CHAR_MECA_FR1D3D','CHAR_MECA_FR2D3D',&
     &     'CHAR_MECA_FR1D1D','CHAR_MECA_FRCO3D','CHAR_MECA_FRCO2D',&
     &     ' '               ,' '               /
!
! --- NOM DE L'OPTION - CAS COMPLEXE
!
    data optioc /&
     &     'MECA_DDLI_C'     ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     'CHAR_MECA_FC1D1D',' '               ,' '               ,&
     &     ' '               ,' '               /
!
! --- NOM DE L'OPTION - CAS FONCTION REELLE
!
    data optiof /&
     &     'MECA_DDLI_F'     ,' '               ,&
     &     'CHAR_MECA_FORC_F','CHAR_MECA_EPSI_F',' '               ,&
     &     'CHAR_MECA_PRES_F','CHAR_MECA_FLUX_F',' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,'CHAR_MECA_FF1D2D','CHAR_MECA_FF3D3D',&
     &     'CHAR_MECA_FF2D2D','CHAR_MECA_FF1D3D','CHAR_MECA_FF2D3D',&
     &     'CHAR_MECA_FF1D1D','CHAR_MECA_FFCO3D','CHAR_MECA_FFCO2D',&
     &     ' '               ,' '               /
!
! --- NOM DU PARAMETRE - CAS REEL
!
    data parar /&
     &     'PDDLIMR'         ,' '               ,&
     &     'PFORNOR'         ,'PEPSINR'         ,' '               ,&
     &     'PPRESSR'         ,'PFLUXR'          ,' '               ,&
     &     ' '               ,' '               ,'PESANR'          ,&
     &     'PROTATR'         ,' '               ,'PFRELEC'         ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,'PFR1D2D'         ,'PFR3D3D'         ,&
     &     'PFR2D2D'         ,'PFR1D3D'         ,'PFR2D3D'         ,&
     &     'PFR1D1D'         ,'PFRCO3D'         ,'PFRCO2D'         ,&
     &     ' '               ,' '               /
!
! --- NOM DU PARAMETRE - CAS COMPLEXE
!
    data parac /&
     &     'PDDLIMC'         ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,' '               ,' '               ,&
     &     'PFC1D1D'         ,' '               ,' '               ,&
     &     ' '               ,' '               /
!
! --- NOM DU PARAMETRE - CAS FONCTION REELLE
!
    data paraf /&
     &     'PDDLIMF'         ,' '               ,&
     &     'PFORNOF'         ,'PEPSINF'         ,' '               ,&
     &     'PPRESSF'         ,'PFLUXF'          ,' '               ,&
     &     ' '               ,' '               ,'PPESANR'         ,&
     &     'PROTATR'         ,' '               ,'PFRELEC'         ,&
     &     ' '               ,' '               ,' '               ,&
     &     ' '               ,'PFF1D2D'         ,'PFF3D3D'         ,&
     &     'PFF2D2D'         ,'PFF1D3D'         ,'PFF2D3D'         ,&
     &     'PFF1D1D'         ,'PFFCO3D'         ,'PFFCO2D'         ,&
     &     ' '               ,' '               /
!
! ----------------------------------------------------------------------
!
    call jemarq()
! ----------------------------------------------------------------------
! --- IDENTIFICATION DES GENRES D'UNE CHARGE
! ----------------------------------------------------------------------
    if (oper .eq. 'IDGE') then
        genrec(1) = 0
        call isdeco(genrec(1), tabcod, 30)
        prefob = optkz
        charge = prefob(1:8)
!
! ----- REPERAGE DES CHARGEMENTS
!
        do indxch = 1, nbtyth
            nomobj = nomob(indxch)
            iposit = 0
            if (nomobj .eq. '__ELIM') then
!
! --------- CHARGEMENT CINEMATIQUE (AFFE_CHAR_CINE)
!
                call dismoi('C', 'TYPE_CHARGE', charge, 'CHARGE', ibid,&
                            typcha, iret)
                if ((typcha(1:4).eq.'CIME') .or. (typcha(1:4) .eq.'CITH') .or.&
                    (typcha(1:4).eq.'CIAC')) then
                    iposit = gencod(indxch)
                endif
            else if (nomobj.eq.'__EVOC') then
!
! --------- CHARGEMENT EVOL_CHAR
!
                nomobj = charge(1:8)//'.CHME.EVOL.CHAR'
                call jeexin(nomobj, iret)
                if (iret .ne. 0) iposit = gencod(indxch)
            else if (nomobj.eq.'__VEAS') then
!
! --------- CHARGEMENT VECT_ASSE
!
                chamno = charge
                call jeexin(chamno(1:19)//'.VALE', iret)
                if (iret .ne. 0) then
                    call gettco(charge, typeco)
                    if (typeco .eq. 'CHAM_NO_SDASTER') iposit = gencod( indxch)
                endif
            else if (nomobj.eq.'__VEAG') then
!
! --------- CHARGEMENT VECT_ASSE_GENE
!
                chamno = charge
                call jeexin(chamno(1:19)//'.VALE', iret)
                if (iret .ne. 0) then
                    call gettco(charge, typeco)
                    if (typeco .eq. 'VECT_ASSE_GENE') iposit = gencod( indxch)
                endif
            else if (nomobj.eq.'.VEASS') then
!
! --------- CHARGEMENT VECT_ASSE_CHAR
!
                call jeexin(prefob(1:13)//nomobj, iexi)
                if (iexi .gt. 0) iposit = gencod(indxch)
            else
!
! --------- CHARGEMENTS DEFINIS PAR UNE CARTE
!
                carte = prefob(1:13)//nomobj
                call exisd('CARTE', carte, iexi)
                if (iexi .eq. 1) iposit = gencod(indxch)
            endif
!
! ------- CHARGEMENT IDENTIFIE: ACTIVATION DANS LA TABLE D'ENCODAGE
!
            if ((iposit.ge.1) .and. (iposit.le.30)) then
                tabcod(iposit) = 1
            endif
        end do
!
! ----- CODAGE DE L'ENTIER
!
        call iscode(tabcod, genrec(1), 30)
        vali(1) = genrec(1)
! ----------------------------------------------------------------------
! --- IDENTIFICATION DES MOTS-CLEFS D'UNE CHARGE
! ----------------------------------------------------------------------
    else if (oper.eq.'IDMC') then
        motclc = 0
        call isdeco(motclc, tabcox, 60)
        prefob = optkz
        charge = prefob(1:8)
!
! ----- REPERAGE DES CHARGEMENTS
!
        do indxch = 1, nbtyth
            nomobj = nomob(indxch)
            iposit = 0
            if (nomobj .eq. '__ELIM') then
!
! --------- CHARGEMENT CINEMATIQUE (AFFE_CHAR_CINE)
!
                call dismoi('C', 'TYPE_CHARGE', charge, 'CHARGE', ibid,&
                            typcha, iret)
                if ((typcha(1:4).eq.'CIME') .or. (typcha(1:4) .eq.'CITH') .or.&
                    (typcha(1:4).eq.'CIAC')) then
                    iposit = mcfcod(indxch)
                endif
            else if (nomobj.eq.'__EVOC') then
!
! --------- CHARGEMENT EVOL_CHAR
!
                nomobj = charge(1:8)//'.CHME.EVOL.CHAR'
                call jeexin(nomobj, iret)
                if (iret .ne. 0) iposit = mcfcod(indxch)
            else if (nomobj.eq.'__VEAS') then
!
! --------- CHARGEMENT VECT_ASSE
!
                chamno = charge
                call jeexin(chamno(1:19)//'.VALE', iret)
                if (iret .ne. 0) then
                    call gettco(charge, typeco)
                    if (typeco .eq. 'CHAM_NO_SDASTER') iposit = mcfcod( indxch)
                endif
            else if (nomobj.eq.'__VEAG') then
!
! --------- CHARGEMENT VECT_ASSE_GENE
!
                chamno = charge
                call jeexin(chamno(1:19)//'.VALE', iret)
                if (iret .ne. 0) then
                    call gettco(charge, typeco)
                    if (typeco .eq. 'VECT_ASSE_GENE') iposit = mcfcod( indxch)
                endif
            else if (nomobj.eq.'.VEASS') then
!
! --------- CHARGEMENT VECT_ASSE_CHAR
!
                call jeexin(prefob(1:13)//nomobj, iexi)
                if (iexi .gt. 0) iposit = mcfcod(indxch)
            else
!
! --------- CHARGEMENTS DEFINIS PAR UNE CARTE
!
                carte = prefob(1:13)//nomobj
                call exisd('CARTE', carte, iexi)
                if (iexi .eq. 1) iposit = mcfcod(indxch)
            endif
!
! ------- CHARGEMENT IDENTIFIE: ACTIVATION DANS LA TABLE D'ENCODAGE
!
            if ((iposit.ge.1) .and. (iposit.le.60)) then
                tabcox(iposit) = 1
            endif
        end do
!
! ----- CODAGE DE L'ENTIER
!
        call iscode(tabcox, motclc, 60)
        vali(1) = motclc(1)
        vali(2) = motclc(2)
! ----------------------------------------------------------------------
! --- RETOURNE LE TYPE DE LA CHARGE (REEL, COMP, FONC)
! ----------------------------------------------------------------------
    else if (oper.eq.'TYPC') then
        prefob = optkz
        typech = ' '
        lveas = .false.
        lveac = .false.
        lveag = .false.
!
! ----- DECODAGE DES GENRES
!
        genrec(1) = opti
        call isdeco(genrec(1), tabcod, 30)
!
! ----- DETECTION VECT_ASSE_CHAR
!
        do indxch = 1, nbtyth
            if (genre(indxch) .eq. 'VECT_ASSE_CHAR') iposit = gencod( indxch)
        end do
        if (tabcod(iposit) .eq. 1) lveac = .true.
!
! ----- DETECTION VECT_ASSE
!
        do indxch = 1, nbtyth
            if (genre(indxch) .eq. 'VECT_ASSE') iposit = gencod(indxch)
        end do
        if (tabcod(iposit) .eq. 1) lveas = .true.
!
! ----- DETECTION VECT_ASSE_GENE
!
        do indxch = 1, nbtyth
            if (genre(indxch) .eq. 'VECT_ASSE_GENE') iposit = gencod( indxch)
        end do
        if (tabcod(iposit) .eq. 1) lveag = .true.
!
! ----- DETECTION TYPE DU CHAMP
!
        if (lveac .or. lveas .or. lveag) then
            call liscva(prefob, chamno)
            call jelira(chamno//'.VALE', 'TYPE', cval=nomgd)
            if (nomgd(1:1) .eq. 'R') then
                typech = 'REEL'
            else if (nomgd(1:1).eq.'C') then
                typech = 'COMP'
            else
                ASSERT(.false.)
            endif
        else
            charge = prefob(1:8)
            call dismoi('C', 'TYPE_CHARGE', charge, 'CHARGE', ibid,&
                        typcha, iret)
            if (iret .eq. 1) then
                call utmess('F', 'CHARGES_3')
            endif
            if (typcha(5:7) .eq. '_RE') then
                typech = 'REEL'
            else if (typcha(5:7).eq.'_RI') then
                typech = 'COMP'
            else if (typcha(5:6).eq.'_F') then
                typech = 'FONC'
            else
                ASSERT(.false.)
            endif
        endif
!
! ----- CAS D'UNE FONCTION: EST-ELLE FONCTION DU TEMPS ?
!
        if (typech .eq. 'FONC') then
            typech = 'FONC_F0'
            lfirst = .false.
            do indxch = 1, nbtyth
                carte = prefob(1:13)//nomob(indxch)
                call exisd('CARTE', carte, iexi)
                if (iexi .eq. 1) then
                    call dismoi('F', 'PARA_INST', carte, 'CARTE', ibid,&
                                parcha, iret)
                    if (parcha .eq. 'OUI') typech = 'FONC_FT'
                endif
            end do
        endif
        valkz = typech
! ----------------------------------------------------------------------
! --- NOM DE L'OBJET DEFINI DANS AFFE_CHAR_*
! ----------------------------------------------------------------------
    else if (oper.eq.'OBJE') then
        indxch = opti
        prefob = optkz
        itypob = -1
        nomobj = ' '
        if ((indxch.gt.0) .and. (indxch.le.nbtyth)) then
            nomobj = prefob(1:13)//nomob(indxch)
            if (nomob(indxch)(1:2) .eq. '__') then
                if (genre(indxch) .eq. 'EVOL_CHAR') then
                    itypob = 0
                    nomobj = prefob(1:13)//'.EVOL.CHAR'
                else if (genre(indxch).eq.'DIRI_ELIM') then
                    itypob = 0
                    charge = prefob(1:8)
                    nomobj = charge(1:8)//'.TYPE'
                else if (genre(indxch).eq.'VECT_ASSE') then
                    itypob = 0
                    chamno = prefob(1:8)
                    nomobj = chamno(1:19)//'.VALE'
                else if (genre(indxch).eq.'VECT_ASSE_GENE') then
                    itypob = 0
                    chamno = prefob(1:8)
                    nomobj = chamno(1:19)//'.VALE'
                else
                    ASSERT(.false.)
                endif
            else
                carte = prefob(1:13)//nomob(indxch)
                itypob = 1
                nomobj = carte
            endif
        else
            ASSERT(.false.)
        endif
        ASSERT(itypob.ge.0)
        valkz = nomobj
        vali(1) = itypob
! ----------------------------------------------------------------------
! --- POSITION DANS L'ENTIER CODE POUR UN GENRE DONNE
! ----------------------------------------------------------------------
    else if (oper.eq.'POSG') then
        gencha = optkz
        iposit = 0
        do indxch = 1, nbtyth
            if (genre(indxch) .eq. gencha) iposit = gencod(indxch)
        end do
        if ((iposit.le.0) .or. (iposit.gt.30)) ASSERT(.false.)
        vali(1) = iposit
! ----------------------------------------------------------------------
! --- POSITION DANS L'ENTIER CODE POUR UN MOT-CLEF DONNE
! ----------------------------------------------------------------------
    else if (oper.eq.'POSM') then
        motcle = optkz
        iposit = 0
        do indxch = 1, nbtyth
            if (motcl(indxch) .eq. motcle) iposit = mcfcod(indxch)
        end do
        if ((iposit.le.0) .or. (iposit.gt.60)) ASSERT(.false.)
        vali(1) = iposit
! ----------------------------------------------------------------------
! --- NOM DE LA CARTE A PARTIR DU MOT-CLEF
! ----------------------------------------------------------------------
    else if (oper.eq.'CART') then
        motcle = optkz
        nomcar = ' '
        itypob = 1
        do indxch = 1, nbtyth
            if (motcl(indxch) .eq. motcle) nomcar = nomob(indxch)
        end do
        if (nomcar(1:2) .eq. '__') itypob = 0
        valkz = nomcar
        vali(1) = itypob
! ----------------------------------------------------------------------
    else if (oper.eq.'OPTI') then
        indxch = opti
        typech = optkz
        option = ' '
        if ((indxch.gt.0) .and. (indxch.le.nbtyth)) then
            if (typech .eq. 'REEL') then
                option = optior(indxch)
            else if (typech.eq.'COMP') then
                option = optioc(indxch)
            else if (typech(1:4).eq.'FONC') then
                option = optiof(indxch)
            else
                ASSERT(.false.)
            endif
        else
            ASSERT(.false.)
        endif
        valkz = option
! ----------------------------------------------------------------------
    else if (oper.eq.'PARA') then
        indxch = opti
        typech = optkz
        lpain = ' '
        if ((indxch.gt.0) .and. (indxch.le.nbtyth)) then
            if (typech .eq. 'REEL') then
                lpain = parar(indxch)
            else if (typech.eq.'COMP') then
                lpain = parac(indxch)
            else if (typech(1:4).eq.'FONC') then
                lpain = paraf(indxch)
            else
                ASSERT(.false.)
            endif
        else
            ASSERT(.false.)
        endif
        valkz = lpain
! ----------------------------------------------------------------------
! --- GENRE A PARTIR DE LA POSITION DANS L'ENTIER CODE
! ----------------------------------------------------------------------
    else if (oper.eq.'GENR') then
        iposit = opti
        do indxch = 1, nbtyth
            if (iposit .eq. gencod(indxch)) gencha = genre(indxch)
        end do
        valkz = gencha
! ----------------------------------------------------------------------
! --- MOT-CLEF A PARTIR DE LA POSITION DANS L'ENTIER CODE
! ----------------------------------------------------------------------
    else if (oper.eq.'MOTC') then
        indxch = opti
        motcle = motcl(indxch)
        valkz = motcle
!
! ----------------------------------------------------------------------
    else if (oper.eq.'IDNS') then
        liscns = optkz
        iposit = opti
!
! ----- GENRE DU CHARGEMENT
!
        do indxch = 1, nbtyth
            if (iposit .eq. gencod(indxch)) gencha = genre(indxch)
        end do
!
! ----- NOMBRE DE CHARGEMENTS DE CE GENRE
!
        nbch = 0
        do indxch = 1, nbtyth
            if (genre(indxch) .eq. gencha) nbch = nbch + 1
        end do
        ASSERT(nbch.gt.0)
!
! ----- CREATION DE L'OBJET
!
        call wkvect(liscns, 'V V I', nbch, jlisci)
!
! ----- REMPLISSAGE DE L'OBJET
!
        i = 0
        do indxch = 1, nbtyth
            if (genre(indxch) .eq. gencha) then
                i = i+1
                zi(jlisci-1+i) = indxch
            endif
        end do
        ASSERT(i.eq.nbch)
        vali(1) = nbch
! ----------------------------------------------------------------------
    else if (oper.eq.'LIGC') then
        indxch = opti
        ligcal = ' '
        if ((indxch.gt.0) .and. (indxch.le.nbtyth)) then
            ligcal = typlig(indxch)
        else
            ASSERT(.false.)
        endif
        valkz = ligcal
! ----------------------------------------------------------------------
    else if (oper.eq.'LISG') then
        liscns = optkz
        idd = 1
        call wkvect(liscns, 'V V K24', nbtyth, jlisck)
        do indxch = 1, nbtyth
            gencha = genre(indxch)
            if (gencha .ne. ' ') then
                if (indxch .eq. 1) then
                    zk24(jlisck-1+indxch) = gencha
                else
                    ldoub = .false.
                    do index2 = 1, nbtyth
                        genold = zk24(jlisck-1+index2)
                        if ((genold.eq.gencha) .and. (genold.ne.' ')) then
                            ldoub = .true.
                            goto 72
                        endif
                    end do
 72                 continue
                    if (.not.ldoub) then
                        idd = idd + 1
                        zk24(jlisck-1+idd) = gencha
                    endif
                endif
            endif
        end do
        ASSERT(idd.le.nbtyth)
        vali(1) = idd
!
    else
        ASSERT(.false.)
!
    endif
!
    call jedema()
end subroutine
