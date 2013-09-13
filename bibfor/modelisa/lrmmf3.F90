subroutine lrmmf3(fid, nomamd, rangfa, carafa, nbnoeu,&
                  famnoe, nmatyp, jfamma, jnumty, vaatfa,&
                  nogrfa, tabaux, nomgro, numgro, nument,&
                  infmed, nivinf, ifm, vecgrm, nbcgrm,&
                  nbgrlo)
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
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     LECTURE DU MAILLAGE - FORMAT MED - LES FAMILLES - 3
!     -    -     -                 -         -          -
!-----------------------------------------------------------------------
!    . SI LA DESCRIPTION D'UNE FAMILLE CONTIENT DES NOMS DE GROUPES, ON
!    VA CREER AUTANT DE GROUPES QUE DECRITS ET ILS PORTERONT CES NOMS.
!    TOUTES LES ENTITES DE LA FAMILLE APPARTIENDRONT A CES GROUPES. UN
!    GROUPE PEUT APPARAITRE DANS LA DESCRIPTION DE PLUSIEURS FAMILLES.
!    SON CONTENU SERA DONC ENRICHI AU FUR ET A MESURE DE L'EXPLORATION
!    DES FAMILLES.
!    . SI LA DESCRIPTION D'UNE FAMILLE NE CONTIENT PAS DE NOMS DE GROUPE
!    ON CREERA DES GROUPES EN SE BASANT SUR UNE IDENTITE D'ATTRIBUTS.
!    LE NOM DE CHAQUE GROUPE EST 'GN' OU 'GM' SELON QUE C'EST UN GROUPE
!    DE NOEUDS OU DE MAILLES, SUIVI DE LA VALEUR DE L'ATTRIBUT. UN MEME
!    ATTRIBUT PEUT APPARAITRE DANS LA DESCRIPTION DE PLUSIEURS FAMILLES.
!    LE CONTENU DU GROUPE ASSOCIE SERA DONC ENRICHI AU FUR ET A MESURE
!    DE L'EXPLORATION DES FAMILLES.
!
!    LE PREMIER CAS APPARAIT QUAND ON RELIT UN FICHIER MED CREE PAR
!    ASTER, OU PAR UN LOGICIEL QUI UTILISERAIT LA NOTION DE GROUPE DE
!    LA MEME MANIERE.
!    LE SECOND CAS A LIEU QUAND LE FICHIER MED A ETE CREE PAR UN
!    LOGICIEL QUI IGNORE LA NOTION DE GROUPE.
!
! ENTREES :
!   FID    : IDENTIFIANT DU FICHIER MED
!   NOMAMD : NOM DU MAILLAGE MED
!   RANGFA : RANG DE LA FAMILLE EN COURS D'EXAMEN
!   NBNOEU : NOMBRE DE NOEUDS
!   FAMNOE : NUMERO DE FAMILLE POUR CHAQUE NOEUD
!   NMATYP : NOMBRE DE MAILLES DU MAILLAGE PAR TYPE DE MAILLES
!   JFAMMA : POUR UN TYPE DE MAILLE, ADRESSE DANS LE TABLEAU DES
!            FAMILLES D'ENTITES
!   JNUMTY : POUR UN TYPE DE MAILLE, ADRESSE DANS LE TABLEAU DES
!            RENUMEROTATIONS
!   VECGRM : VECTEUR DE CORRESPONDANCE DES NOMS DE GROUPES MED / ASTER
!   NBCGRM : NOMBRE DE CORRESPONDANCE
! SORTIES :
!   NOMGRO : COLLECTION DES NOMS DES GROUPES A CREER
!   NUMGRO : COLLECTION DES NUMEROS DES GROUPES A CREER
!   NUMENT : COLLECTION DES NUMEROS DES ENTITES DANS LES GROUPES
! TABLEAUX DE TRAVAIL
!   VAATFA : VALEUR DES ATTRIBUTS ASSOCIES A CHAQUE FAMILLE.
!   NOGRFA : NOM DES GROUPES ASSOCIES A CHAQUE FAMILLE.
!   TABAUX :
! DIVERS
!   INFMED : NIVEAU DES INFORMATIONS SPECIFIQUES A MED A IMPRIMER
!   NIVINF : NIVEAU DES INFORMATIONS GENERALES
!   IFM    : UNITE LOGIQUE DU FICHIER DE MESSAGE
! ENTREES/SORTIES :
!   CARAFA : CARACTERISTIQUES DE CHAQUE FAMILLE
!     CARAFA(1,I) = NOMBRE DE GROUPES
!     CARAFA(2,I) = NOMBRE D'ATTRIBUTS
!     CARAFA(3,I) = NOMBRE D'ENTITES
!-----------------------------------------------------------------------
!
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/as_mfafai.h"
#include "asterfort/as_mfanfg.h"
#include "asterfort/as_mfaofi.h"
#include "asterfort/as_mfaona.h"
#include "asterfort/as_mfinvr.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/desgfa.h"
#include "asterfort/initch.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jucroc.h"
#include "asterfort/juveca.h"
#include "asterfort/lxlgut.h"
#include "asterfort/lxnoac.h"
#include "asterfort/utmess.h"
    integer :: ntymax
    parameter (ntymax = 69)
!
! 0.1. ==> ARGUMENTS
!
    integer :: fid, nbcgrm
    integer :: rangfa, carafa(3, *)
    integer :: nbnoeu, famnoe(nbnoeu)
    integer :: nmatyp(ntymax), jfamma(ntymax), jnumty(ntymax)
    integer :: vaatfa(*)
    integer :: tabaux(*)
    integer :: infmed
    integer :: ifm, nivinf, nbgrlo, major, minor, rel, cret
!
    character(len=*) :: nomgro, numgro, nument
    character(len=*) :: nogrfa(*)
    character(len=*) :: nomamd, vecgrm
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'LRMMF3' )
!
    integer :: codret, i, codre2
    integer :: vali
    integer :: iaux, jaux, kaux, jau2
    integer :: ityp, inom, itmp, ivgrm
    integer :: numfam, mi(2)
    integer :: nbattr, nbgrou, nbenfa
    integer :: idatfa(200)
    integer :: adnomg, adnumg, adnume, nvnbgr
    integer :: ilmed, ilnew, nogrlo, jnogrl, jnogrc
    logical :: ierr, renomm, errgm
    real(kind=8) :: mr
    character(len=80) :: kbid, newgrm
!
    character(len=2) :: saux02
    character(len=8) :: saux08
    character(len=24) :: k24b, saux24
    character(len=64) :: nomfam
    character(len=80) :: valk(4)
    character(len=200) :: descat(200)
!
!
!     ------------------------------------------------------------------
!
    ierr = .false.
    if (nivinf .ge. 2) then
        write (ifm,1001) nompro
        1001 format( 60('-'),/,'DEBUT DU PROGRAMME ',a)
    endif
!
!====
! 0. TABLEAU DE CORRESPONDANCE NOM MED - NOM ASTER
!====
    if (nbcgrm .gt. 0) then
        call jeveuo(vecgrm, 'L', ivgrm)
    endif
!
!====
! 1. CARACTERISTIQUES DE LA FAMILLE
!====
!
! 1.1. ==> LECTURE DANS LE FICHIER MED
!
!     NOMBRE MAXI D'ATTRIBUTS
    nbattr = 200
    nomfam = ' '
    call initch(descat, nbattr)
    call as_mfinvr(fid, major, minor, rel, cret)
    if (major .eq. 3) then
        call as_mfafai(fid, nomamd, rangfa, nomfam, numfam,&
                       nogrfa, codret)
        nbattr = 0
    else
        call as_mfaona(fid, nomamd, rangfa, nbattr, codre2)
        call as_mfaofi(fid, nomamd, rangfa, nomfam, numfam,&
                       idatfa, vaatfa, descat, nbattr, nogrfa,&
                       codret)
    endif
    if (codret .ne. 0) then
        saux08='mfaofi'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
    call as_mfanfg(fid, nomamd, rangfa, nbgrou, codret)
!
    call jeveuo('&&LRMMF1.NOM_GR_LONG', 'E', jnogrl)
    call jelira('&&LRMMF1.NOM_GR_LONG', 'LONMAX', nogrlo)
    call jeveuo('&&LRMMF1.NOM_GR_COURT', 'E', jnogrc)
!
! 1.2. ==> INFORMATION EVENTUELLE
!
    if (infmed .ge. 3) then
        if (numfam .gt. 0) then
            jaux = 1
        else
            jaux = 2
        endif
        kaux = -1
        call desgfa(jaux, numfam, nomfam, nbgrou, nogrfa,&
                    nbattr, vaatfa, kaux, kaux, ifm,&
                    codret)
    endif
!
!====
! 2. SI LA FAMILLE N'EST PAS LA FAMILLE NULLE, DECODAGE
!====
!
    if (numfam .ne. 0) then
!
        if (infmed .ge. 2) then
            call utmess('I', 'MED_14', sk=nomfam, si=numfam)
        endif
!
! 2.0. ==> CONTROLE DE LA LONGUEUR DES NOMS DES GROUPES
!
        if (nbgrou .gt. 0) then
!
            do 20 , iaux = 1 , nbgrou
!   2.0.1. --- RENOMMAGE PAR L'UTILISATEUR
            renomm = .false.
            errgm = .false.
            if (nbcgrm .gt. 0) then
                ilmed = lxlgut(nogrfa(iaux))
                do 910 i = 1, nbcgrm
                    kbid = zk80(ivgrm-1+i*2-1)
                    ilnew = lxlgut(kbid)
                    if (nogrfa(iaux)(1:ilmed) .eq. kbid(1:ilnew)) then
                        kbid = zk80(ivgrm-1+i*2)
                        ilnew = lxlgut(kbid)
                        renomm = .true.
                        valk(1) = nogrfa(iaux)
                        valk(2) = kbid(1:ilnew)
                        if (infmed .ge. 2) then
                            call utmess('I', 'MED_16', nk=2, valk=valk, si=iaux)
                        endif
                        newgrm = kbid(1:ilnew)
                    endif
!
!          --- VERIFIER QUE LES NOUVEAUX NOMS N'EXISTENT PAS DEJA
                    kbid = zk80(ivgrm-1+i*2)
                    ilnew = lxlgut(kbid)
                    if (nogrfa(iaux)(1:ilmed) .eq. kbid(1:ilnew)) then
                        errgm = .true.
                        valk(1) = zk80(ivgrm-1+i*2-1)
                        valk(2) = kbid(1:ilnew)
                        call utmess('E', 'MED_9', nk=2, valk=valk)
                    endif
910              continue
            endif
            if (.not. renomm) then
                if (infmed .ge. 2) then
                    call utmess('I', 'MED_15', sk=nogrfa(iaux), si=iaux)
                endif
            else
                nogrfa(iaux) = newgrm
            endif
            if (errgm) then
                call utmess('F', 'MED_18')
            endif
!
!   2.0.2. --- SUPPRESSION DES CARACTERES INTERDITS (ACCENTS...)
            call lxnoac(nogrfa(iaux), newgrm)
            if (nogrfa(iaux) .ne. newgrm) then
                jau2 = lxlgut(nomfam)
                mi(1) = iaux
                valk(1) = nomfam(1:jau2)
                valk(2) = nogrfa(iaux)
                valk(3) = newgrm(1:24)
                call utmess('A', 'MED_10', nk=3, valk=valk, si=mi(1))
                nogrfa(iaux) = newgrm(1:24)
            endif
!
!   2.0.3. --- CONTROLE QUE LA LONGUEUR <= 8
            jaux = lxlgut(nogrfa(iaux))
            if (jaux .gt. 24) then
                jau2 = lxlgut(nomfam)
                mi(1) = iaux
                valk(1) = nomfam(1:jau2)
                valk(2) = nogrfa(iaux)
                valk(3) = nogrfa(iaux)(1:24)
                call utmess('A', 'MED_7', nk=3, valk=valk, si=mi(1))
!
!   2.0.3. --- CONTROLE QUE LE NOM EST NON VIDE
            else if (jaux.eq.0) then
                jau2 = lxlgut(nomfam)
                mi(1) = iaux
                valk(1) = nomfam(1:jau2)
                call utmess('F', 'MED_11', sk=valk(1), si=mi(1))
            endif
20          continue
!
        endif
!
! 2.1. ==> IL FAUT AU MOINS UN GROUPE OU UN ATTRIBUT
!
        if (nbgrou .eq. 0 .and. nbattr .eq. 0) then
            call utmess('A', 'MED_13', sk=nomfam)
        endif
!
! 2.2. ==> COHERENCE DES NOMBRES DE GROUPES OU D'ATTRIBUTS
!
        valk(1) = nomfam
        if (nbgrou .ne. carafa(1,rangfa)) then
            mi(1) = carafa(1,rangfa)
            mi(2) = nbgrou
            valk(2) = 'groupes'
        endif
        if (nbattr .ne. carafa(2,rangfa)) then
            mi(1) = carafa(2,rangfa)
            mi(2) = nbattr
            valk(2) = 'attributs'
        endif
        if (( nbgrou.ne.carafa(1,rangfa) ) .or. ( nbattr.ne.carafa(2, rangfa) )) then
            call utmess('F', 'MED_8', nk=2, valk=valk, ni=2,&
                        vali=mi)
        endif
!
! 2.3. ==> CREATION :
!        COLLECTION INVERSE       FAM I -> NUMNO(MA)X,NUMNO(MA)Y..
!     ET VECTEUR DES LONGUEURS    FAM I -> NBNUMNO(MA)
!       (POUR EVITER DE FAIRE DES TONNES DE JELIRA APRES)
!          MEMORISATION DU DU NOMBRE D'ENTITES QUE LA FAMILLE CONTIENT
!
        nbenfa = 0
!
! 2.3.1. ==> POUR UNE FAMILLE DE NOEUDS : LE TABLEAU TABAUX CONTIENDRA
!            LA LISTE DES NOEUDS DE LA FAMILLE
!
        if (numfam .gt. 0) then
!
            do 231 , iaux = 1 , nbnoeu
            if (numfam .eq. famnoe(iaux)) then
                nbenfa = nbenfa + 1
                tabaux(nbenfa) = iaux
            endif
231          continue
!
! 2.3.2. ==> POUR UNE FAMILLE DE MAILLES : LE TABLEAU TABAUX CONTIENDRA
!            LA LISTE DES MAILLES DE LA FAMILLE, TYPE PAR TYPE.
!
        else if (numfam.lt.0) then
!
            do 232 , ityp = 1 , ntymax
            if (nmatyp(ityp) .ne. 0) then
                do 2321 , iaux = 1 , nmatyp(ityp)
                if (numfam .eq. zi(jfamma(ityp)+iaux-1)) then
                    nbenfa = nbenfa + 1
                    tabaux(nbenfa) = zi(jnumty(ityp)+iaux-1)
                endif
2321              continue
            endif
232          continue
!
        endif
!
        carafa(3,rangfa) = nbenfa
!
! 2.4. ==> MEMORISATION DES NUMEROS DES ENTITES DE LA FAMILLE
!
        if (nbenfa .gt. 0) then
!
            call jucroc(nument, saux24, rangfa, nbenfa, adnume)
            adnume = adnume - 1
            do 24 , iaux = 1 , nbenfa
            zi(adnume+iaux) = tabaux(iaux)
24          continue
!
        endif
!
! 2.5. ==> CREATION DES NOMS DES GROUPES ASSOCIES
!         POUR FORMER LES COLLECTIONS FAM I -> NOMGNO X , NOMGMA Y ...
!                                     FAM J -> NUMGNO X , NUMGMA Y ...
!         ON MET LE NUMERO DE GROUPE A +-99999999. AINSI, LE PROGRAMME
!         DE CREATION, LRMNGR, FERA UNE NUMEROTATION AUTOMATIQUE.
!         CONVENTION : SI GROUPE DE MAILLES => NUMGRP< 0 SINON NUMGRP>0
!
!         SI AUCUN GROUPE N'A ETE DEFINI, ON CREE DES GROUPES DONT LE
!         NOM EST BATI SUR LA VALEUR DES ATTRIBUTS. ATTENTION, ASTER
!         REFUSE LES SIGNES '-' DANS LES NOMS DES GROUPES ... DE MEME,
!         IL FAUT DISTINGUER LES GROUPES DE NOEUDS ET DE MAILLES
!         SINON, ON COPIE LES NOMS PRESENTS DANS LE DESCRIPTIF DE LA
!         FAMILLE.
!
        if (nbenfa .gt. 0) then
!
            iaux = max ( 1, nbattr, nbgrou )
            call jucroc(nomgro, saux24, rangfa, iaux, adnomg)
            call jucroc(numgro, saux24, rangfa, iaux, adnumg)
!
            if (numfam .gt. 0) then
                saux02 = 'GN'
                jaux = 99999999
            else
                saux02 = 'GM'
                jaux = -99999999
            endif
!
            if (nbgrou .eq. 0) then
!
                nvnbgr = nbgrlo+nbattr
                if (nvnbgr .gt. nogrlo) then
                    call juveca('&&LRMMF1.NOM_GR_LONG    ', nvnbgr)
                    call juveca('&&LRMMF1.NOM_GR_COURT   ', nvnbgr)
                    call jeveuo('&&LRMMF1.NOM_GR_LONG', 'E', jnogrl)
                    call jeveuo('&&LRMMF1.NOM_GR_COURT', 'E', jnogrc)
                endif
!
                do 251 , iaux = 1 , nbattr
                call codent(vaatfa(iaux), 'G', saux24)
                if (vaatfa(iaux) .lt. 0) then
                    saux24(3:8) = 'M'//saux24(2:6)
                else
                    saux24(3:8) = 'P'//saux24(1:5)
                endif
                saux24(1:2) = saux02
                zk24(adnomg-1+iaux) = saux24
                zi(adnumg-1+iaux) = jaux
                zk80(jnogrl-1+iaux+nbgrlo) = saux24
                zk24(jnogrc-1+iaux+nbgrlo) = saux24
251              continue
                nbgrlo = nbgrlo + nbattr
!
            else
!
                nvnbgr = nbgrlo+nbgrou
                if (nvnbgr .gt. nogrlo) then
                    call juveca('&&LRMMF1.NOM_GR_LONG    ', nvnbgr)
                    call juveca('&&LRMMF1.NOM_GR_COURT   ', nvnbgr)
                    call jeveuo('&&LRMMF1.NOM_GR_LONG', 'E', jnogrl)
                    call jeveuo('&&LRMMF1.NOM_GR_COURT', 'E', jnogrc)
                endif
!
                do 252 , iaux = 1 , nbgrou
!
                k24b = nogrfa(iaux)(1:24)
                inom = indik8 ( zk24(adnomg), k24b, 1, iaux )
                if (inom .ne. 0) then
                    ierr = .true.
                    vali = iaux
                    valk (1) = ' '
                    itmp = lxlgut(nogrfa(iaux))
                    valk (2) = nogrfa(iaux)(1:itmp)
                    itmp = lxlgut(nogrfa(inom))
                    valk (3) = nogrfa(inom)(1:itmp)
                    valk (4) = k24b
                    call utmess('E', 'MED_22', nk=4, valk=valk, si=vali)
                endif
                zk24(adnomg-1+iaux) = k24b
                zi(adnumg-1+iaux) = jaux
                zk80(jnogrl-1+iaux+nbgrlo) = nogrfa(iaux)
                zk24(jnogrc-1+iaux+nbgrlo) = k24b
252              continue
                nbgrlo = nbgrlo + nbgrou
!
            endif
!
        endif
!
    endif
!
!     ERREUR LORS DE LA VERIFICATION DES NOMS DE GROUPES:
    ASSERT(.not.ierr)
!
    if (nivinf .ge. 2) then
!
        write (ifm,4001) nompro
        4001 format(/,'FIN DU PROGRAMME ',a,/,60('-'))
!
    endif
!
end subroutine
