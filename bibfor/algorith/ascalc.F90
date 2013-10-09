subroutine ascalc(resu, masse, mome, psmo, stat,&
                  nbmode, neq, nordr, knomsy, nbopt,&
                  ndir, monoap, muapde, nbsup, nsupp,&
                  typcmo, temps, comdir, typcdi, tronc,&
                  amort, spectr, asspec, nomsup, reasup,&
                  depsup, tcosup, corfre, f1gup, f2gup)
! aslint: disable=W1306,W1504
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/asacce.h"
#include "asterfort/ascorm.h"
#include "asterfort/asdir.h"
#include "asterfort/asecon.h"
#include "asterfort/asefen.h"
#include "asterfort/assert.h"
#include "asterfort/asstoc.h"
#include "asterfort/astron.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/vprecu.h"
#include "asterfort/wkvect.h"
!
    integer :: ndir(*), tcosup(*), nordr(*), nsupp(*)
    real(kind=8) :: amort(*), spectr(*), asspec(*), depsup(*), reasup(*)
    real(kind=8) :: f1gup, f2gup
    character(len=*) :: resu, masse, mome, psmo, stat, typcmo, typcdi, knomsy(*)
    character(len=*) :: nomsup(*)
    logical :: monoap, muapde, comdir, tronc, corfre
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
!
!     UTILISE PAR LA COMMANDE : COMB_SISM_MODAL
!
!     ------------------------------------------------------------------
! IN  : RESU   : NOM UTILISATEUR DE LA COMMANDE
! IN  : MASSE  : MATRICE ASSEMBLEE
! IN  : MOME   : MODES MECANIQUES
! IN  : PSMO   : PSEUDO-MODES (SI PRISE EN COMPTE DE LA TRONCATURE)
! IN  : STAT   : MODE STATIQUES (CAS MULTI-SUPPORT)
! IN  : NBMODE : NOMBRE DE MODES
! IN  : NEQ    : NOMBRE D'EQUATIONS
! IN  : NORDR  : NUMERO D'ORDRE DES MODES MECANIQUES
! IN  : KNOMSY : LES OPTIONS DE CALCUL
! IN  : NBOPT  : NOMBRE D'OPTION DE CALCUL
! IN  : NDIR   : DIRECTIONS DE CALCUL
! IN  : MONOAP : =.TRUE.  , CAS DU MONO-SUPPORT
!                =.FALSE. , CAS DU MULTI-SUPPORT
! IN  : MUAPDE : =.TRUE.  , CAS DU MULTI-SUPPORTS DECORRELES
!                =.FALSE. , CAS DU MULTI-SUPPORTS CORRELES
! IN  : NBSUP  : NOMBRE DE SUPPORT
! IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
! IN  : TYPCMO : TYPE DE RECOMBINAISON DES MODES
! IN  : TEMPS  : DUREE FORTE DU SEISME (TYPCMO='DSC')
! IN  : COMDIR : =.TRUE.  , COMBINAISON DES DIRECTIONS
!                =.FALSE. , PAS DE COMBINAISON DES DIRECTIONS
! IN  : TYPCDI : TYPE DE COMBINAISON DES DIRECTIONS
! IN  : TRONC  : =.TRUE.  , PRISE EN COMPTE DE LA TRONCATURE
!                =.FALSE. , PAS DE PRISE EN COMPTE DE LA TRONCATURE
! IN  : AMORT  : VECTEUR DES AMORTISSEMENTS MODAUX
! IN  : SPECTR : VECTEUR DES SPECTRES MODAUX
! IN  : ASSPEC : VECTEUR DES ASYMPTOTES DES SPECTRES AUX SUPPORTS
! IN  : NOMSUP : VECTEUR DES NOMS DES SUPPORTS
! IN  : REASUP : VECTEUR DES REACTIONS MODALES AUX SUPPORTS
! IN  : DEPSUP : VECTEUR DES DEPLACEMENTS DES SUPPORTS
! IN  : TCOSUP : TYPE DE RECOMBINAISON DES SUPPORTS
! IN  : CORFRE : =.TRUE.  , CORRECTION DES FREQUENCES
! IN  : F1GUP  : FREQUENCE F1 POUR LA METHODE DE GUPTA
! IN  : F2GUP  : FREQUENCE F2 POUR LA METHODE DE GUPTA
!     ------------------------------------------------------------------
    integer ::  id, iopt, iret, jcrer, jcrep, jdir, jmod, jrep1, jtabs
    integer :: jval, nbmode, nbopt, nbpara, nbpari, nbpark, nbparr, nbsup, ndepl
    integer :: neq, jrep2, nbdis(nbsup), noc, ioc, jnoe1, n1, nno, is, ino, igr
    integer :: ngr, jgrn, jdgn, ier, ncompt, nintra
    parameter     ( nbpara = 5 )
    real(kind=8) :: temps
    logical :: prim, secon, glob
    character(len=4) :: ctyp
    character(len=8) :: k8b, noeu, noma
    character(len=15) :: motfa1
    character(len=16) :: nomsy, nomsy2, nopara(nbpara)
    character(len=19) :: kvec, kval, moncha
    character(len=24) :: kvx1, kvx2, kve2, kve3, kve4, kve5, obj1, obj2
    character(len=24) :: grnoeu
    integer :: iarg
!
    data  nopara /        'OMEGA2'          , 'MASS_GENE'       ,&
     &  'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ'  /
!     ------------------------------------------------------------------
!
    call jemarq()
    kvec = '&&ASCALC.VAL_PROPRE'
    kval = '&&ASCALC.GRAN_MODAL'
    kvx1 = '&&ASCALC.REP_MO1'
    kvx2 = '&&ASCALC.REP_MO2'
    kve2 = '&&ASCALC.C_REP_MOD_PER'
    kve3 = '&&ASCALC.REP_DIR'
    kve4 = '&&ASCALC.TABS'
    kve5 = '&&ASCALC.C_REP_MOD_RIG'
    call rsexch('F', mome, 'DEPL', 1, moncha,&
                ier)
!
    call getfac('COMB_DEPL_APPUI', ndepl)
    if (ndepl .ne. 0) then
        prim = .true.
        secon =.true.
        glob = .false.
    else
        prim = .false.
        secon =.false.
        glob = .true.
    endif
!
!
!  ----         CAS DECORRELE            ----
!  ---- INITIALISATION DU TABLEAU CONCERNANT
!  ---- LES REGROUPEMENTS EN INTRA-GROUPE
    do 50 is = 1, nbsup
        nbdis(is) = 0
 50 end do
    nintra = nbsup
    noc = nbsup
!
!  ---- CONSTITUTION DES GROUPES D'APPUI ----
    if ((.not.monoap) .and. muapde) then
        motfa1 = 'GROUP_APPUI'
        call getfac(motfa1, noc)
!  ---- SI GROUP_APPUI EST PRESENT ----
        if (noc .ne. 0) then
            do 100 ioc = 1, noc
                call getvtx(motfa1, 'NOEUD', iocc=ioc, nbval=0, nbret=n1)
                if (n1 .ne. 0) then
                    nno = -n1
                    call wkvect('&&ASCALC.NOEUD', 'V V K8', nno, jnoe1)
                    call getvtx(motfa1, 'NOEUD', iocc=ioc, nbval=nno, vect=zk8(jnoe1),&
                                nbret=n1)
                    do 101 ino = 1, nno
                        noeu = zk8(jnoe1+ino-1)
                        call getvtx(motfa1, 'NOEUD', iocc=ioc, nbval=0, nbret=n1)
                        do 102 is = 1, nbsup
                            do 103 id = 1, 3
                                if (nomsup((id-1)*nbsup+is) .eq. noeu) then
                                    if (nbdis(is) .ne. 0) then
                                        call utmess('F', 'SEISME_29')
                                    endif
                                    nbdis(is) = ioc
                                endif
103                         continue
102                     continue
101                 continue
                    call jedetr('&&ASCALC.NOEUD')
                else
                    call dismoi('NOM_MAILLA', masse, 'MATR_ASSE', repk=noma)
                    obj1 = noma//'.GROUPENO'
                    obj2 = noma//'.NOMNOE'
                    call getvem(noma, 'GROUP_NO', motfa1, 'GROUP_NO', ioc,&
                                iarg, 0, k8b, n1)
                    if (n1 .ne. 0) then
                        ngr = -n1
                        call wkvect('&&ASCALC.GROUP_NO', 'V V K24', ngr, jgrn)
                        call getvem(noma, 'GROUP_NO', motfa1, 'GROUP_NO', ioc,&
                                    iarg, ngr, zk24(jgrn), n1)
                        do 110 igr = 1, ngr
                            grnoeu = zk24(jgrn+igr-1)
                            call jeexin(jexnom(obj1, grnoeu), iret)
                            if (iret .eq. 0) then
                                ier = ier + 1
                                ASSERT(iret .ne. 0)
                            endif
                            call jelira(jexnom(obj1, grnoeu), 'LONUTI', nno)
                            call jeveuo(jexnom(obj1, grnoeu), 'L', jdgn)
!
                            do 111 ino = 1, nno
                                call jenuno(jexnum(obj2, zi(jdgn+ino-1) ), noeu)
                                do 112 is = 1, nbsup
                                    do 113 id = 1, 3
                                        if (nomsup((id-1)*nbsup+is) .eq. noeu) then
                                            if (nbdis(is) .ne. 0) then
                                                call utmess('F', 'SEISME_29')
                                            endif
                                            nbdis(is) = ioc
                                        endif
113                                 continue
112                             continue
111                         continue
110                     continue
                        call jedetr('&&ASCALC.GROUP_NO')
                    endif
                endif
100         continue
            ncompt = 0
            if (noc .eq. 1) then
                do 120 is = 1, nbsup
                    ncompt = ncompt + nbdis(is)
120             continue
                if (ncompt .eq. nbsup) then
                    call utmess('F', 'SEISME_30')
                endif
            endif
        endif
!  ---- SI GROUP_APPUI EST ABSENT ----
        ncompt = 0
        do 130 is = 1, nbsup
            if (nbdis(is) .eq. 0) then
                ncompt = ncompt + 1
                nbdis(is) = noc + ncompt
            endif
130     continue
        nintra = noc + ncompt
    else
!  ---- SI LES EXCITATIONS SONT CORRELEES ----
        do 140 is = 1, nbsup
            nbdis(is) = is
140     continue
        nintra = nbsup
    endif
!
!
!     --- BOUCLE SUR LES OPTIONS DE CALCUL "NOMSY" ---
    do 10 iopt = 1, nbopt
        nomsy = knomsy(iopt)
        nomsy2 = nomsy
        if (nomsy(1:4) .eq. 'VITE') nomsy2 = 'DEPL'
        if (nomsy(1:4) .eq. 'ACCE') nomsy2 = 'DEPL'
        call vprecu(mome, nomsy2, nbmode, nordr, kvec,&
                    nbpara, nopara(1), k8b, kval, k8b,&
                    neq, nbmode, ctyp, nbpari, nbparr,&
                    nbpark)
        call jeveuo(kvec, 'L', jmod)
        call jeveuo(kval, 'L', jval)
        call wkvect(kvx1, 'V V R', 3*neq*nbsup, jrep1)
        call wkvect(kvx2, 'V V R', 3*neq*nbsup, jrep2)
        call wkvect(kve2, 'V V R', 3*neq*nbsup, jcrep)
        call wkvect(kve3, 'V V R', 3*neq, jdir)
        call wkvect(kve4, 'V V R', nbsup*neq, jtabs)
        call wkvect(kve5, 'V V R', 3*neq*nbsup, jcrer)
!
!        ---------------------------------------------------------------
!                        REPONSE PRIMAIRE OU GLOBAL
!        ---------------------------------------------------------------
!
!        --- BOUCLE SUR LES DIRECTIONS ----
        do 20 id = 1, 3
            if (ndir(id) .eq. 1) then
!
!              --- CALCUL DES REPONSE MODALES ---
!
!              --- COMBINAISON DES REPONSES MODALES ---
!
                call ascorm(monoap, typcmo, nbsup, nsupp, neq,&
                            nbmode, zr(jrep1), zr(jrep2), amort, zr(jval),&
                            id, temps, zr(jcrer), zr(jcrep), zr(jtabs),&
                            nomsy, zr(jmod), reasup, spectr, corfre,&
                            muapde, tcosup, nintra, nbdis, f1gup,&
                            f2gup)
!
!              --- PRISE EN COMPTE DES EFFETS D'ENTRAINEMENT ---
!              --- DANS LE CAS DE CALCUL DE REPONSE GLOBALE  ---
!
                if ((.not.monoap) .and. glob) then
                    call asefen(muapde, nomsy2, id, stat, neq,&
                                nbsup, ndir, nsupp, masse, nomsup,&
                                depsup, zr(jcrep), nintra, nbdis)
                endif
!
!              --- PRISE EN COMPTE DE LA TRONCATURE ---
!              --- DANS LE CAS DE CALCUL DE REPONSE GLOBALE  ---
!
                if (tronc) then
                    call astron(nomsy, psmo, monoap, muapde, nbsup,&
                                nsupp, neq, nbmode, id, zr(jmod),&
                                zr(jval), spectr, nomsup, reasup, zr(jcrer),&
                                zr(jcrep))
                endif
!
!              ----CALCUL DE L ACCELERATION ABSOLUE
!
                call asacce(nomsy, monoap, muapde, nbsup, neq,&
                            nbmode, id, moncha, zr(jmod), zr(jval),&
                            spectr, zr( jcrer), zr(jcrep), nbdis)
!
!              --- CALCUL DES RECOMBINAISONS PAR DIRECTIONS---
                call asdir(monoap, muapde, id, neq, nbsup,&
                           nsupp, tcosup, zr(jcrep), zr(jdir))
            endif
 20     continue
!
!        --- STOCKAGE ---
!
        call asstoc(mome, resu, nomsy, neq, zr(jdir),&
                    ndir, comdir, typcdi, glob, prim)
!
!        ---------------------------------------------------------------
!                            REPONSE SECONDAIRE
!        ---------------------------------------------------------------
        if (secon) then
!
!            --- PRISE EN COMPTE DES EFFETS D'ENTRAINEMENT ---
!            --- DANS LE CAS DE CALCUL DE REPONSE GLOBALE  ---
!
            if (nomsy(1:11) .ne. 'ACCE_ABSOLU') then
                call asecon(nomsy, neq, mome, resu)
            endif
!
        endif
!
        call jedetr(kvec)
        call jedetr(kval)
        call jedetr(kvx1)
        call jedetr(kvx2)
        call jedetr(kve2)
        call jedetr(kve3)
        call jedetr(kve4)
        call jedetr(kve5)
!
 10 end do
!
    call jedema()
end subroutine
