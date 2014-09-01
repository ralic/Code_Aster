subroutine dismdy(questi, nomobz, repi, repkz, ierd)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterc/r8vide.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsvpar.h"
! ----------------------------------------------------------------------
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
! person_in_charge: hassan.berro at edf.fr
!
!     --     DISMOI(DYNA_PHYS/DYNA_GENE)
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN RESULTAT DYNAMIQUE DE TYPE DYNA_GENE/PHYS
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
! ----------------------------------------------------------------------
!
    integer :: repi, ierd
    character(len=*) :: questi, nomobz, repkz
!
    aster_logical :: oktref
    integer :: corent(6), ibid, jref, intyre, nbrefs, inddeb, indfin, senpar, numocc
    integer :: indcha, ir, index, l1, l2, l3, nbcham, jdesc
    character(len=4) :: accref(6), typref, indref
    character(len=8) :: resdyn, numcha, intf, k8bid, basemo
    character(len=16) :: typrep
    character(len=24) :: numddl, cortre(6), questl, typcon, nomcha, nomgd
    character(len=32) :: repk
    integer, pointer :: indi(:) => null()
!
!     --- TYPE DE REFERENCES QUE NOUS POUVONS RECUPERER [REF_XXXX_****]
    data  accref /  'RIGI'    ,   'MASS'    ,   'AMOR'    ,    'INTD'    ,    'INTS'    ,  'MESU' /
!     --- CORRESPONDANCE AVEC LES TYPE DE REF DANS LA COLLECT. REFD
    data  cortre /'DYNAMIQUE' , 'DYNAMIQUE' , 'DYNAMIQUE' , 'INTERF_DYNA', 'INTERF_STAT', 'MESURE'/
!     --- CORRESPONDANCE AVEC LE NUMERO D'ENTREE DANS UN OBJ DU REFD
    data  corent /    2       ,     3       ,     4       ,       2      ,       2      ,     2   /
!
    call jemarq()
    repi = 0
    repk = ' '
    ierd = 0
!     --- RECOPIE DES ARGUMENTS IN DANS DES VARIABLES DE LONGUEURS FIXES
    resdyn = nomobz
    questl = questi
!
!     --- TEST INITIAL SUR LA STRUCTURE DE DONNEES DYNAMIQUE EN ENTREE
    call jeexin(resdyn//'           .REFD', ir)
    if (ir .eq. 0) goto 99
!
!     ------------------
!     --- QUESTION 1 ---
!     ------------------------------------------------------------------
!     --- RESULTAT SUR DES COORDONNEES PHYSIQUES OU GENERALISEES ?
    if (questl(1:9) .eq. 'TYPE_COOR') then
!       --- RECUPERER LE (UN) NUME_DDL DU RESULTAT DYNAMIQUE
        call dismoi('NUME_DDL', resdyn, 'RESU_DYNA', repk=numddl)
        call gettco(numddl, typcon)
        repk = 'PHYS'
        if (typcon(1:13) .eq. 'NUME_DDL_GENE') repk = 'GENE'
        goto 88
!
!     ------------------
!     --- QUESTION 2 ---
!     ------------------------------------------------------------------
!     --- NOM DE LA BASE DE PROJECTION
    else if (questl(1:11).eq.'BASE_MODALE') then
        call dismoi('NUME_DDL', resdyn, 'RESU_DYNA', repk=numddl, arret='C',&
                    ier=ir)
        if ((ir .eq. 0) .and. (numddl .ne. ' ')) then
            call jeveuo(numddl(1:14)//'.NUME.REFN', 'L', jref)
            repk = zk24(jref)
            goto 88
        else if (numddl .eq. ' ') then
            call dismoi('REF_MESU_PREM', resdyn, 'RESU_DYNA', repk=repk, arret='C',&
                        ier=ir)
            goto 88
        else
            goto 99
        endif
!
!     ------------------
!     --- QUESTION 3 ---
!     ------------------------------------------------------------------
!
!     - 3.1 - MATRICES/OBJETS DE REFERENCES ACCESSIBLES SELON LES REGLES
!     QUESTION = REF _ [TYPE OBJ] _ [INDICE D'OCCURENCE]
!                         /K4/        /K4/
!     REF_MASS_0001       MASS        0001   : OCC #1 DE M
!     REF_RIGI_PREM       RIGI        PREM   : PREMIERE OCC DE K
!     REF_AMOR_0002       AMOR        0002   : OCC #2 DE A
!     REF_INTD_DERN       INTD        DERN   : DERNIERE OCC INTERF_DYNA
!     REF_INST_PREM       INTS        PREM   : PREMIERE OCC INTERF_STAT
!     ------------------------------------------------------------------
!    #123_5678_0123                     '> CALL CODENT(INDI,'D0',INDIK4)
!     NOTE : LA QUESTION 'REF_MASS_0001' EQUIVAUT 'REF_MASS_PREM'
!     ------------------------------------------------------------------
!
!     - 3.2 - NOMBRE DE MATRICES/OBJETS DE REFERENCE
!     QUESTION    = REF _ [TYPE OBJ] _ NOMB
!     REF_RIGI_NOMB          RIGI      : NOMBRE D'ENTREES AVEC MATR RIGI
!     REF_MASS_NOMB          MASS      : NOMBRE D'ENTREES AVEC MATR MASS
!                             .
!                             .
    else if (questl(1:3).eq.'REF') then
        typref = questl(5:8)
!
!       --- VERIFIER LA VALIDITE DE LA REQUETE
        oktref = .false.
        do ibid = 1, 6
            if (typref .eq. accref(ibid)) then
                oktref = .true.
                intyre = ibid
            endif
        end do
        if (.not.(oktref)) goto 99
!
!       --- LIRE ET VERIFIER LA VALIDITE DE L'INDICE D'OCCURENCE
        call jelira(resdyn//'           .REFD', 'NUTIOC', nbrefs, k8bid)
        indref = questl(10:13)
        inddeb = 1
        indfin = nbrefs
        senpar = 1
        numocc = 1
        if (indref .eq. 'NOMB') then
            numocc = 0
        else if (indref .eq. 'PREM') then
            numocc = 1
        else if (indref .eq. 'DERN') then
            inddeb = nbrefs
            indfin = 1
            senpar = -1
            numocc = 1
        else
            read(indref,'(i4)') numocc
            if ((numocc.gt.nbrefs) .or. (numocc.le.0)) goto 99
        endif
!
!       --- CHERCHER L'ENTREE DE REFERENCE DANS LA COLLECTION REFD
        repi = 0
        do ibid = inddeb, indfin, senpar
            call jeveuo(jexnum(resdyn//'           .REFD', ibid), 'L', jref)
            if (cortre(intyre) .eq. zk24(jref)) then
                repi = repi+1
                if (repi .eq. numocc) then
                    repk = zk24(jref+corent(intyre))
!             --- RETOURNER EGALEMENT L'INDICE ABSOLUE DE L'ENTREE
                    repi = ibid
                    goto 88
                endif
            endif
        end do
!
!       --- TRAITEMENT SPECIAL POUR LA QUESTION : REF_****_NOMB
        if (numocc .eq. 0) goto 88
!       --- SORTIE DE LA BOUCLE SANS TROUVER L'INDICE DEMANDEE => ERREUR
        goto 99
!
!     ------------------
!     --- QUESTION 4 ---
!     ------------------------------------------------------------------
!     --- NUME_DDL D'UN CHAMP DU RESULTAT DYNAMIQUE
!         QUESTION = NUME_CHAM_[INDICE DU CHAMP]
!                                 /K8/ --> CALL CODENT(INDI,'D0',INDIK8)
!     --- EXEMPLES : 'NUME_CHAM_00000001', 'NUME_CHAM_00003842', ...
    else if (questl(1:9).eq.'NUME_CHAM') then
        call jeveuo(resdyn//'           .INDI', 'L', vi=indi)
        call jelira(resdyn//'           .INDI', 'LONUTI', nbrefs, k8bid)
        numcha = questl(11:18)
        indcha = 1
        read(numcha,'(i8)') indcha
        index = 0
        do ibid = 1, nbrefs
            if (indcha .le. indi(ibid)) then
                index = ibid
                goto 41
            endif
        end do
 41     continue
!       --- SI LE NUMERO DU CHAMP N'EST PAS VALIDE PAR RAPPORT AU .INDI
        if ((index .eq. 0) .and. (indi(1) .eq. -1)) then
            index = 1
        else if (index.eq.0) then
            goto 99
        endif
!
!       --- RECUPERER LE NUMEDDL DANS LE .REFD ET LE RENVOYER
        call jeveuo(jexnum(resdyn//'           .REFD', index), 'L', jref)
        repk = zk24(jref+1)
        goto 88
!
!     ------------------
!     --- QUESTION 5 ---
!     ------------------------------------------------------------------
!     --- QUELLE EST LA NUMEROTATION CORRESPONDANTE DE CE RESU_DYNA ?
!       --- RENVOI DEUX INFORMATIONS IMPORTANTES :
!       --- 1) ENTIER DONNANT LE NOMBRE DE NUME_DDL DIFFERENTS TROUVES
!       --- 2) LE NOM DU 1ER NUMEDDL DE REFERENCE (OU PROF_CHNO SELON LE CAS)
!
    else if (questl(1:8).eq.'NUME_DDL') then
        call jelira(resdyn//'           .REFD', 'NUTIOC', nbrefs, k8bid)
        if (nbrefs .eq. 0) goto 99
!
        call jeveuo(jexnum(resdyn//'           .REFD', 1), 'L', jref)
        numddl = zk24(jref+1)
!
        repi = 1
        do ibid = 1, nbrefs
            call jeveuo(jexnum(resdyn//'           .REFD', ibid), 'L', jref)
            if (numddl .ne. zk24(jref+1)) repi = repi +1
        end do
        repk = numddl
        goto 88
!
!     ------------------
!     --- QUESTION 6 ---
!     ------------------------------------------------------------------
!     --- QUEL EST LE TYPE DE LA BASE MODALE (MODE_MECA) ?
!       --- RENVOI UNE CHAINE K24 AVEC :
!         - 1)  OU 2) CLASSIQUE  OU 3) CYCLIQUE OU 3) RITZ
!
    else if (questl(1:9).eq.'TYPE_BASE') then
!       TESTER D'ABORD LE TYPE DES CHAMPS DEPL PRESENTS DANS LA SD (COMPLEXE / REEL)
        call rsexch(' ', resdyn, 'DEPL', 1, nomcha,&
                    ir)
        if (ir .eq. 0) then
            call dismoi('NOM_GD', nomcha, 'CHAMP', repk=nomgd)
            if (nomgd(1:6) .eq. 'DEPL_C') then
                repk = ' '
                goto 88
            endif
        else
            goto 99
        endif
!       IL N'EXISTE QUE DE DEPL_R
        call jelira(resdyn//'           .REFD', 'NUTIOC', nbrefs, k8bid)
        call dismoi('NB_CHAMP_UTI', resdyn, 'RESULTAT', repi=nbcham, arret='C',&
                    ier=ir)
        if ((nbrefs .eq. 0) .or. (nbcham .eq. 0)) goto 99
        repk = ' '
        if (nbrefs .gt. 1) then
            repk = 'RITZ'
            if (nbrefs .eq. 2) then
                call dismoi('REF_INTD_PREM', resdyn, 'RESU_DYNA', repk=intf, arret='C',&
                            ier=ir)
                if (ir .eq. 0) then
                    repk = 'CLASSIQUE'
                    call dismoi('NOM_MODE_CYCL', intf, 'INTERF_DYNA', repk=k8bid)
                    if (k8bid .ne. ' ') repk = 'CYCLIQUE'
                endif
            endif
        else
            call rsvpar(resdyn, 1, 'FACT_PARTICI_DX', ibid, r8vide(),&
                        k8bid, l1)
            call rsvpar(resdyn, 1, 'FACT_PARTICI_DY', ibid, r8vide(),&
                        k8bid, l2)
            call rsvpar(resdyn, 1, 'FACT_PARTICI_DZ', ibid, r8vide(),&
                        k8bid, l3)
            if ((l1+l2+l3) .eq. 300) repk = 'RITZ'
        endif
        goto 88
!
!
!     ------------------
!     --- QUESTION 7 ---
!     ------------------------------------------------------------------
!     --- COMBIEN DE CHAMPS SONT STOCKES (ARCHIVES) DANS LE RESULTAT DYNAMIQUE ?
    else if (questl(1:9).eq.'NB_CHAMPS') then
!       --- RESULTATS DYNA_GENE ET DYNA_PHYS ONT TOUS LES DEUX CET OBJET
        call jeexin(resdyn//'           .ORDR', l1)
        if (l1 .eq. 0) goto 99
        call jelira(resdyn//'           .ORDR', 'LONMAX', nbcham, k8bid)
        repi = nbcham
        goto 88
!
!
!     ------------------
!     --- QUESTION 8 ---
!     ------------------------------------------------------------------
!     --- POUR UN RESULTAT SUR BASE GENERALISE, S'AGIT IL D'UNE PROJECTION SIMPLE
!         OU NON ?
    else if (questl(1:11).eq.'PROJ_SIMPLE') then
        repk = 'OUI'
        call dismoi('BASE_MODALE', resdyn, 'RESU_DYNA', repk=basemo)
        typrep = 'MODE_MECA'
        if (basemo(1:8) .ne. ' ') call gettco(basemo, typrep)
        if (typrep(1:9) .ne. 'MODE_MECA') repk = 'NON'
        goto 88
!
!     ------------------
!     --- QUESTION 9 ---
!     ------------------------------------------------------------------
!     --- POUR UN RESULTAT SUR BASE GENERALISE, PRENONS NOUS EN COMPTE UNE CORRECTION
!         STATIQUE AUX CHARGEMENTS ?
    else if (questl(1:9).eq.'CORR_STAT') then
        repk = 'NON'
        call jeveuo(resdyn//'           .DESC','L',jdesc)
        if (zi(jdesc+7-1).eq.1) repk = 'OUI'
        goto 88
!
!
    endif
!
!     <<--- RENVOYER ICI EN CAS DE PROBLEME DANS LA REQUETE
 99 continue
    repi = 0
    repk = ' '
    ierd = 1
!
!     <<--- RENVOYER ICI EN CAS DE SUCCES DE LA REQUETE
 88 continue
    repkz = repk
!
    call jedema()
end subroutine
