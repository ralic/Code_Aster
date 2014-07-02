subroutine mltpre(mat19, renumz)
! person_in_charge: olivier.boiteau at edf.fr
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mlnmin.h"
#include "asterfort/preml0.h"
#include "asterfort/preml1.h"
#include "asterfort/preml2.h"
#include "asterfort/premla.h"
#include "asterfort/premlc.h"
#include "asterfort/premld.h"
#include "asterfort/prnchk.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: renumz
    character(len=1) :: base
    character(len=19) :: mat19, solv19
    character(len=14) :: nu
    integer :: nec, i, icol
    character(len=8) :: nomgd
    character(len=8) :: renum, renum2
    integer :: ismhc, iprno, diag
    integer :: lgadjn, adjnc1, adjnc2, desc
    integer :: supnd, anc, nouv, fils, frere, lgsn, lfront, nbass
    integer :: debfsn, adpile, adress, nblign, lgbloc, ncbloc, lbd1, lbd2, rl
    integer :: rl1, rl2, xadj1, xadj2, seq, parend, supnd2, trav1, trav2
    integer :: trav3, trav4, p, q, invp, perm, invsup
    integer :: lmat, lgind, global, local, decal
    integer :: mrl, ino, nbsn, iddl, nbloc, neq, nrl, lgpile
    integer :: n2, lt, lglist, deb, vois1, vois2, suit1, suit2, globa, loca
    integer :: iret, optnum, ier, jrenu
    integer :: noeud, ddl, permnd, invpnd, spndnd, ddlmoy, xadjd
    character(len=24) :: nomloc, nomad1, nomad2, nomglo, nomadi
    character(len=24) :: nomdeb, nomvoi, nomsui, nopglo, noploc
    character(len=24) :: nomt01, nomt02, nomt04, nomt05, nomt06, nomt07
    character(len=24) :: nomt08, nomt12, nomt13, nomt14
    character(len=24) :: nomt15, nomt16, nomt17, nomt18, nomt19, nomt20, nomt21
    character(len=24) :: nomt22, nomt23, nomt24, nomt25, nomt26, nomt27
    character(len=24) :: nomp01, nomp02, nomp03, nomp04, nomp05, nomp06, nomp07
    character(len=24) :: nomp08, nomp09, nomp10, nomp11, nomp12, nomp13
    character(len=24) :: nomp14, nomp15, nomp16, nomp17, nomp18, nomp19, nomp20
    aster_logical :: nivdbg, matgen
    character(len=24), pointer :: refa(:) => null()
    character(len=24), pointer :: slvk(:) => null()
    integer, pointer :: nueq(:) => null()
    integer, pointer :: smde(:) => null()
    integer, pointer :: deeq(:) => null()
    integer, pointer :: smdi(:) => null()
    integer, pointer :: delg(:) => null()
    data nomglo/'              .MLTF.GLOB'/
    data nomloc/'              .MLTF.LOCL'/
    data nomadi/'              .MLTF.ADNT'/
    data nopglo/'&&MLTPRE.NOMGLOBAL_PROV '/
    data noploc/'&&MLTPRE.NOM_LOCAL_PROV '/
    data nomad1/'&&MLTPRE.ADJNC1    .    '/
    data nomad2/'&&MLTPRE.ADJNC2    .    '/
    data nomdeb/'&&MLTPRE.DEBUT___VOISINS'/
    data nomvoi/'&&MLTPRE.LISTE___VOISINS'/
    data nomsui/'&&MLTPRE.SUITE___VOISINS'/
!     DATA NOMCOL/'&&MLTPRE.TABLEAU_COLONNE'/
    data nomt01/'&&MLTPRE.POINTEUR_TEMP01'/
    data nomt02/'&&MLTPRE.POINTEUR_TEMP02'/
    data nomt04/'&&MLTPRE.POINTEUR_TEMP04'/
    data nomt05/'&&MLTPRE.POINTEUR_TEMP05'/
    data nomt06/'&&MLTPRE.POINTEUR_TEMP06'/
    data nomt07/'&&MLTPRE.POINTEUR_TEMP07'/
    data nomt08/'&&MLTPRE.POINTEUR_TEMP08'/
    data nomt12/'&&MLTPRE.POINTEUR_TEMP12'/
    data nomt13/'&&MLTPRE.POINTEUR_TEMP13'/
    data nomt14/'&&MLTPRE.POINTEUR_TEMP14'/
    data nomt15/'&&MLTPRE.POINTEUR_TEMP15'/
    data nomt16/'&&MLTPRE.POINTEUR_TEMP16'/
    data nomt17/'&&MLTPRE.POINTEUR_TEMP17'/
    data nomt18/'&&MLTPRE.POINTEUR_TEMP18'/
    data nomt19/'&&MLTPRE.POINTEUR_TEMP19'/
    data nomt20/'&&MLTPRE.POINTEUR_TEMP20'/
    data nomt21/'&&MLTPRE.POINTEUR_TEMP21'/
    data nomt22/'&&MLTPRE.POINTEUR_TEMP22'/
    data nomt23/'&&MLTPRE.POINTEUR_TEMP23'/
    data nomt24/'&&MLTPRE.POINTEUR_TEMP24'/
    data nomt25/'&&MLTPRE.POINTEUR_TEMP25'/
    data nomt26/'&&MLTPRE.POINTEUR_TEMP26'/
    data nomt27/'&&MLTPRE.POINTEUR_TEMP27'/
!
!
    nivdbg=.false.
    call jemarq()
!     -- ON INTERROMPT LA MESURE CPU.RESO.4 PENDANT CPU.RESO.3 :
    call uttcpu('CPU.RESO.4', 'FIN', ' ')
    call uttcpu('CPU.RESO.3', 'DEBUT', ' ')
!
!
    call dismoi('NOM_NUME_DDL', mat19, 'MATR_ASSE', repk=nu)
    call jelira(nu//'.SMOS.SMDI', 'CLAS', cval=base)
!
!
!     -- RENUM : RENUMEROTATION SOUHAITEE POUR LA RESOLUTION
    if (renumz .eq. ' ') then
        call dismoi('SOLVEUR', mat19, 'MATR_ASSE', repk=solv19)
        call jeveuo(solv19//'.SLVK', 'L', vk24=slvk)
        ASSERT(slvk(1).eq.'MULT_FRONT')
        renum=slvk(4)
    else
        renum=renumz
    endif
!
!
!     -- SI LE STOCKAGE STOC_MLTF EXISTE DEJA ET QU'IL CORRESPOND
!        A LA BONNE METHODE DE RENUMEROTATION, ON NE FAIT RIEN.
!        SINON, ON (RE)CONSTRUIT UN STOC_MLTF
!     ------------------------------------------------------------
    call jeexin(nu//'.MLTF.ADNT', iret)
    if (iret .gt. 0) then
        call jeveuo(nu//'.MLTF.RENU', 'L', jrenu)
!       -- RENUM2: RENUMEROTATION ASSOCIEE A .MLTF
        renum2=zk8(jrenu)
        if (renum .eq. renum2) then
!         -- IL N'Y A RIEN A FAIRE :
            goto 999
        else
            call detrsd('MLTF', nu//'.MLTF')
        endif
    endif
!
!
!
    call wkvect(nu//'.MLTF.RENU', base//' V K8', 1, jrenu)
    zk8(jrenu) = renum
!
    call mlnmin(nu, nomp01, nomp02, nomp03, nomp04,&
                nomp05, nomp06, nomp07, nomp08, nomp09,&
                nomp10, nomp11, nomp12, nomp13, nomp14,&
                nomp15, nomp16, nomp17, nomp18, nomp19,&
                nomp20)
    nomglo(1:14) = nu
    nomloc(1:14) = nu
    nomadi(1:14) = nu
!
    call jeexin(nomglo, iret)
    if (iret .ne. 0) call jedetr(nomglo)
    call jeexin(nomloc, iret)
    if (iret .ne. 0) call jedetr(nomloc)
    call jeexin(nopglo, iret)
    if (iret .ne. 0) call jedetr(nopglo)
    call jeexin(noploc, iret)
    if (iret .ne. 0) call jedetr(noploc)
!
    call jeveuo(nu//'.NUME.DELG', 'L', vi=delg)
    call jeveuo(nu//'.NUME.DEEQ', 'L', vi=deeq)
    call jeveuo(nu//'.NUME.NUEQ', 'L', vi=nueq)
    call jeveuo(jexnum(nu//'.NUME.PRNO', 1), 'L', iprno)
    call jeveuo(nu//'.SMOS.SMHC', 'L', icol)
    call jeveuo(nu//'.SMOS.SMDI', 'L', vi=smdi)
    call jeveuo(nu//'.SMOS.SMDE', 'L', vi=smde)
!
    neq = smde(1)
    do i = 1, neq
        ASSERT(nueq(i).eq.i)
    end do
    call jelibe(nu//'.NUME.NUEQ')
    lmat = smdi(neq)
!     LA STRUCTURE NOMADI A LA LONGUEUR EXACTE: LMAT
!     ET N' EST PAS SURDIMENSIONNEE COMME SMOS.SMHC
    call wkvect(nomadi, base//' V I ', lmat, ismhc)
    do i = 0, lmat - 1
        zi(ismhc+i) = zi4(icol+i)
    end do
    call jelibe(nu//'.SMOS.SMHC')
    call jeveuo(mat19//'.REFA', 'L', vk24=refa)
    if (refa(10) .ne. 'NOEU') then
        ASSERT(refa(10).eq.'GENE')
        matgen=.true.
    else
        matgen=.false.
    endif
!
!
!     CALCUL DE MRL, NBRE MAXIMUM DE RELATIONS LINEAIRES
!     SERT A DIMENSIONNER AU PLUS JUSTE LE TABLEAU RL
    mrl = 1
    do iddl = 1, neq
        if (delg(iddl) .ne. 0) then
            ino = deeq(1+2* (iddl-1))
            if (ino .eq. 0) mrl = mrl + 1
        endif
!
    end do
!
!     OBTENTION DE NEC
    call dismoi('NOM_GD', nu, 'NUME_DDL', repk=nomgd)
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nec)
!
    call wkvect(nomp01, base//' V I ', 5, desc)
    call wkvect(nomp02, base//' V I ', neq+1, diag)
!     COPIE DE SMDI DANS DIAG
    do i = 1, neq
        zi(diag+i) = smdi(i)
    end do
    call jelibe(nu//'.SMOS.SMDI')
    zi(diag) = 0
!
!
    call wkvect(nomt12, ' V V I ', neq, p)
    call wkvect(nomt13, ' V V I ', neq, q)
!     ALLOCATION POUR LES CONDITIONS AUX LIMITES (DOUBLES LAGRANGES)
    call wkvect(nomt17, ' V V I ', neq, lbd1)
    call wkvect(nomt19, ' V V I ', neq, rl1)
    call wkvect(nomt20, ' V V I ', neq, rl2)
    call wkvect(nomt18, ' V V I ', neq, lbd2)
    call wkvect(nomt21, ' V V I ', 4*mrl, rl)
!-----------------------------------------------------------------
!     PREML0 TRAITE LES CONDITIONS AUX LIMITES PAR DOUBLE LAGRANGE
!
!     DONNEES   :
!     DELG, DEEQ, PRNO
!
!     RESULTATS :
!     N2        : NBRE DE DDL HORS LAGRANGE
!     LBD1(1:NEQ): LISTE DES LAGRANGE1 DE BLOQUAGE
!     LBD2(1:NEQ): LISTE DES LAGRANGE2 DE BLOQUAGE
!     RL1(1:NEQ): LISTE DES LAGRANGE1 DE RELA.LINEAIRE
!     RL2(1:NEQ): LISTE DES LAGRANGE2 DE RELA.LINEAIRE
!     RL(1:4,1:NEQ): TAB. DE TRAVAIL DES RELA.LINEAIRES
!     P ET Q : CORRESPONDANCE  DES DDL NON LAGRANGE ENTRE
!     LA NUMEROTATION (1:NEQ) ET (1:N2)
!     NRL       : NBRE DE RELATIONS LINEAIRES ENTRE DDL
!     LT        : PLACE NECESSAIRE POUR STOCKER LES VOISINAGES
!     SUPPLEMENTAIRES INDUITS PAR LES RELA. LINEAIRES
!------------------------------------------------------------
!
    call preml0(neq, n2, zi(diag), zi(ismhc), delg,&
                zi(iprno), deeq, nec, zi(p), zi(q),&
                zi(lbd1), zi(lbd2), zi(rl), zi(rl1), zi(rl2),&
                nrl, lt, lmat)
!
    lglist = 1
    ier = 0
    if (nrl .ne. 0) lglist = lt
    call wkvect(nomdeb, ' V V I ', neq, deb)
 50 continue
    call wkvect(nomvoi, ' V V I ', lglist, vois1)
    call wkvect(nomsui, ' V V I ', lglist, suit1)
!
!     PREMLA FABRIQUE LA LISTE CHAINE :( DEB, VOIS, SUIT)
!     VOISINAGE DE TOUS LES DDL RELIES PAR UNE RELATION LINEAIRE
!
    call premla(neq, zi(diag), zi(ismhc), lglist, nrl,&
                zi(rl), zi(deb), zi(vois1), zi(suit1), ier)
!
    if (ier .gt. 0) then
        call jedetr(nomvoi)
        call jedetr(nomsui)
        lglist = 2*lglist
        goto 50
    endif
    lglist = -ier
!
!------------------------------------------------------------------
!     PREML1 1)
!     FABRIQUE LA STRUCTURE (XADJ2,ADJNC2) QUI EST LA
!     RESTRICTION DE ADJNC1 AUX DDL NON LAGRANGE (1:N2)
!     2) APPEL GENMMD OU AMDBAR MINIMUM DEGRE SIMPLE OU
!     APPROXIMATE
!     DONNEES
!
!     DIAG, COL, DELG
!
!     RESULTATS
!
!     INVP, PERM : RENUMEROTATION DU MINIMUM DEGREE
!     SEQ (ALIAS PARENT)  : ARBORESCENCE
!     ADRESS     : POINTEURS POUR FACTORISATION SYMBOLIQUE
!     LGIND      : LONGUEUR DE TABLEAU POUR LA FACT. SYMB.
!     SUPND2(1:NBSN): DEFINITION DES SUPER-NOEUDS
!
!     TABLEAU DE TRAVAIL :
!     ADJNC2   : DETRUIT APRES PREML1
!     TRAV1,TRAV2,TRAV3,TRAV4
!
    call wkvect(nomp03, base//' V I ', neq+1, adress)
    lgadjn = 2* (lmat-neq)
 60 continue
    if (nrl .ne. 0) lgadjn = lgadjn + 2*lglist
!     NRL EST LE NOMBRE DE RELATIONS LINEAIRES ENTRE DDL
!     ON REND ARTIFICIELLEMENT UN LIEN ENTRE TOUS CES DL
!     IL SONT RANGES DANS UNE LISTE CHAINE ( DEB, VOIS,SUIT)
!     DE LONGUEUR LGLIST CALCULEE PAR PREMLA
!
!
!     OPTNUM =0 INDIQUE L'APPEL A GENMMD
!     OPTNUM =1 APPEL A AMDBAR : APPROXIMATE MINIMUM  DEGREE
!     OPTNUM =2 APPEL A METIS : MULTI LEVEL BISSECTION
    if (renum .eq. 'MDA') then
        optnum = 1
    else if (renum.eq.'MD') then
        optnum = 0
    else if (renum.eq.'METIS') then
        optnum = 2
    else
        call utmess('F', 'ALGELINE_91', sk=renum)
    endif
    if (optnum .eq. 1) then
!     POUR AMDBAR ON AUGMENTE LA LONGUEUR DE ADJNC2
        lgadjn = lgadjn + 2*neq
    endif
    if (lgadjn .le. 0) lgadjn=1
    call wkvect(nomad2, ' V V I ', lgadjn, adjnc2)
!
    do i = 0, lgadjn - 1
        zi(i+adjnc2) = 0
    end do
    call wkvect(nomp20, base//' V I ', neq, seq)
    call wkvect(nomt04, ' V V I ', (neq+1), supnd2)
    call wkvect(nomt14, ' V V I ', neq, invp)
    call wkvect(nomt15, ' V V I ', neq, perm)
    call wkvect(nomt02, ' V V I ', (neq+1), xadj2)
    call wkvect(nomt05, ' V V I ', (neq+1), trav1)
    call wkvect(nomt06, ' V V I ', neq, trav2)
    call wkvect(nomt07, ' V V I ', neq, trav3)
    call wkvect(nomt08, ' V V I ', neq, trav4)
    call wkvect(nomt16, ' V V I ', neq, invsup)
! ALLOCATIONS SUPPLEMENTAIRES POUR LA RENUMEROTATION PAR NOEUDS
    call wkvect(nomt26, ' V V I ', neq+1, noeud)
    call wkvect(nomt22, ' V V I ', neq+1, ddl)
    call wkvect(nomt23, ' V V I ', neq, invpnd)
    call wkvect(nomt24, ' V V I ', neq, permnd)
    call wkvect(nomt25, ' V V I ', neq, spndnd)
    call wkvect(nomt27, ' V V I ', neq+1, xadjd)
!
    call preml1(neq, n2, zi(diag), delg, zi(ismhc),&
                zi(xadj2), zi(adjnc2), zi(seq), zi(adress), zi(supnd2),&
                zi(trav1), zi(trav2), zi(trav3), zi(trav4), zi(p),&
                zi(q), zi(invp), zi(perm), lgind, ddlmoy,&
                nbsn, optnum, lgadjn, nrl, zi(deb),&
                zi(vois1), zi(suit1), ier, nec, zi(iprno),&
                deeq, zi(noeud), zi(ddl), zi(invpnd), zi(permnd),&
                zi(spndnd), zi(xadjd), matgen)
    call jedetr(nomt25)
    call jedetr(nomt22)
    call jedetr(nomt23)
    call jedetr(nomt24)
    call jedetr(nomt26)
    call jedetr(nomt27)
    call jelibe(nu//'.NUME.PRNO')
    call jelibe(nu//'.NUME.DEEQ')
    call jedetr(nomad2)
    if (ier .gt. 0) then
        lgadjn = ier
        goto 60
    endif
    call jedetr(nomt02)
    call jelibe(nomdeb)
    call jelibe(nomvoi)
    call jelibe(nomsui)
!----------------------------------------------------------------
!     PREMLC TERMINE LA NOUVELLE LA NOUVELLE NUMEROTATION ISSUE
!     ------ DU MIN. DEGRE EN AJOUTANT LES LAMBDA DE LAGRANGE
!     EN RESPECTANT LES CONTRAINTES D ENCADREMENT
!
!
!     DONNEES : SEQ ( (ALIAS PARENT) INUTILE APRES) NOMT03
!     SUPND2 ( INUTILE APRES) NOMT04
!     P,Q    ( INUTILE APRES) NOMT12,NOMT13
!     INVP, PERM
!     LBD1,LBD2,RL,RL1, RL2
!     DONNEES MODIFIEES
!     LGIND LONGUEUR DE GLOBAL ET LOCAL ( CREES PAR PREML2)
!     RESULTATS
!     SUPND   : DEFINITION DEFINITIVE DES SUPER-NOEUDS
!     (SUPND2+LAMBDA)
!     PAREND  : ARBORESCENCE
!     NOUV, ANC : RENUMEROTATION ISSUE DU MD ENTRE TOUS LES DDL.
    call wkvect(nomp04, base//' V I ', neq+1, supnd)
    call wkvect(nomp05, base//' V I ', neq, parend)
    call wkvect(nomp14, base//' V I ', neq, anc)
    call wkvect(nomp19, base//' V I ', neq, nouv)
    call premlc(neq, zi(diag), zi(ismhc), zi(seq), zi(parend),&
                zi(anc), zi(nouv), zi(supnd), zi(supnd2), zi(trav1),&
                zi(trav4), zi(p), zi(q), zi(lbd1), zi(lbd2),&
                zi(rl), zi(rl1), zi(rl2), nrl, zi(invp),&
                zi(perm), lgind, ddlmoy, nbsn)
    call jedetr(nomt15)
    call jedetr(nomt17)
    call jedetr(nomt18)
    call jedetr(nomt19)
    call jedetr(nomt20)
    call jedetr(nomt21)
!----------------------------------------------------------------
!     COEFFICIENT DE SECURITE POUR LGIND QUI SERA TESTE DANS FACSMB
!     AU FUR ET A MESURE DE LA FABRICATION DE GLOBAL ET LOCAL
!      LGIND = (LGIND*15)/10
    lgind = max(lgind,10000)
!
!
    call wkvect(nomt01, ' V V I ', (neq+1), xadj1)
    call wkvect(nomad1, ' V V I ', lgadjn, adjnc1)
    call jeveuo(nomdeb, 'L', deb)
    call jeveuo(nomvoi, 'L', vois2)
    call jeveuo(nomsui, 'L', suit2)
    do i = 0, lgadjn - 1
        zi(i+adjnc1) = 0
    end do
!---------------------------------------------------------------
!     PREMLD  APPELLE CALADJ QUI FABRIQUE ADJNC1
!     DONNEES
!     DIAG ,COL,RL,NRL
!
!     RESULTATS
!     XADJ1,ADJNC1: LISTE DES VOISINS DE CHAQUE DDL DE NO + PETIT
!     OU + GRAND
!     TAB. DE TRAVAIL
!     TRAV1,TRAV2 VOIS SUIV : SERVENT DE LISTE CHAINEE POUR AJOUTER
!     LES CONNEXIONS ENTRE DDL RELA. LINEAIRE
!-----------------------------------------------------------------
    call premld(neq, zi(diag), zi(ismhc), zi(xadj1), zi(adjnc1),&
                zi(trav1), zi(deb), zi(vois2), zi(suit2), lgadjn,&
                nrl)
!
    call jelibe(nomdeb)
    call jelibe(nomvoi)
    call jelibe(nomsui)
!
!----------------------------------------------------------
!     PREML2 APPELLE :
!     FACSMB : FACTORISATION SYMBOLIQUE
!     MLTPOS : RENUMEROTATION DE L ARBORESCENCE
!     MLTBLC : DECOUPAGE EN BLOCS DE LA FACTORISEE
!     MLTPAS : PLACE DES TERMES INITIAUX DANS LA FACTORISEE
!     QUI SERONT DANS NOMADI A LA PLACE DE COL
!
!
    call wkvect(nomp06, base//' V I ', nbsn, fils)
    call wkvect(nomp07, base//' V I ', nbsn, frere)
    call wkvect(nomp08, base//' V I ', nbsn, lgsn)
    call wkvect(nomp09, base//' V I ', nbsn, lfront)
    call wkvect(nomp10, base//' V I ', nbsn, nbass)
    call wkvect(nomp12, base//' V I ', nbsn+1, debfsn)
    call wkvect(nomp13, base//' V I ', nbsn, adpile)
    call wkvect(nomp15, base//' V I ', nbsn, nblign)
    call wkvect(nomp16, base//' V I ', nbsn, lgbloc)
    call wkvect(nomp17, base//' V I ', nbsn, ncbloc)
    call wkvect(nomp18, base//' V I ', nbsn, decal)
 90 continue
    call wkvect(nopglo, ' V V S ', lgind, global)
    call wkvect(noploc, ' V V S ', lgind, local)
    call preml2(neq, zi(diag), zi(ismhc), delg, zi(xadj1),&
                zi(adjnc1), lgpile, zi(adress), zi(parend), zi(fils),&
                zi(frere), zi(anc), zi(nouv), zi(supnd), zi(trav1),&
                zi(trav2), zi(trav3), zi(trav4), zi(invsup), zi4(local),&
                zi4(global), zi(lfront), zi(nblign), zi(decal), zi(lgsn),&
                zi(supnd2), zi(debfsn), zi(seq), lmat, zi(adpile),&
                zi(p), zi(q), zi(invp), zi(nbass), zi(ncbloc),&
                zi(lgbloc), nbloc, lgind, nbsn, ier)
!
!      CALL JELIBE(NU//'.NUME.DELG')
!
    if (ier .ne. 0) then
        call jedetr(nopglo)
        call jedetr(noploc)
        lgind = ier
        goto 90
    endif
    call jelibe(nomadi)
    if (nivdbg) then
!     ON APPELLE PRNCHK POUR VERIFIER LA COHERENCE
!     ENTRE L'ARBORESCENCE ET LA RENUMEROTATION
!     EN PARTICULIER A ACTIVER LORS D'UN PROBLEME AVEC METIS
!     COMME ON A DEJA VU AVEC DES ELEMENTS 3D FILAIRES OU DES
!     POUTRES (FICHES 10312 ET 10468)
        call prnchk(nbsn, zi(adress), zi4(global), zi(fils), zi(frere),&
                    zi(lgsn), zi(lfront), zi(invsup), zi(seq))
    endif
    zi(desc) = neq
    zi(desc+1) = nbsn
    zi(desc+2) = nbloc
    zi(desc+3) = lgpile
    zi(desc+4) = lmat
!
    call jelibe(nomp01)
    call jedetr(nomp02)
    call jedetr(nomad1)
    call jedetr(nomt01)
    call jedetr(nomt05)
    call jedetr(nomt06)
    call jedetr(nomt07)
    call jedetr(nomt08)
    call jedetr(nomt16)
    call jedetr(nomt04)
    call jedetr(nomt12)
    call jedetr(nomt13)
    call jedetr(nomt14)
    call jedetr(nomdeb)
    call jedetr(nomvoi)
    call jedetr(nomsui)
    call jedetr(nomp05)
    call jedetr(nomp12)
!
!     LGIND DEVIENT LA LONGUEUR REELLE DE GLOBAL ET LOCALS
!     ON RECOPIE GLOBAL ET LOCAL DANS NOMGLO ET NOMLOC
!     SUIVANT LEUR LONGUEUR EXACTE
    lgind = zi(adress+nbsn) - 1
    call wkvect(nomglo, base//' V S ', lgind, globa)
    call wkvect(nomloc, base//' V S ', lgind, loca)
    do i = 0, lgind - 1
        zi4(globa+i) = zi4(global+i)
        zi4(loca+i) = zi4(local+i)
    end do
    call jedetr(nopglo)
    call jedetr(noploc)
999 continue
!
    call uttcpu('CPU.RESO.3', 'FIN', ' ')
    call uttcpu('CPU.RESO.4', 'DEBUT', ' ')
    call jedema()
end subroutine
