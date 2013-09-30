subroutine prosmo(matrez, limat, nbmat, basez, numedd,&
                  lsym, rouc)
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
!.======================================================================
    implicit none
!
!     PROSMO  --  LE BUT DE CETTE ROUTINE EST DE CONSTRUIRE LA MATR_ASSE
!                 DE NOM MATRES QUI VA RESULTER DE LA COMBINAISON
!                 LINEAIRE DES NBMAT MATR_ASSE DE LA LISTE LISMAT
!                 DE NOMS DE MATR_ASSE. LES MATRICES ONT UN STOCKAGE
!                 MORSE
!
!
!   ARGUMENT        E/S  TYPE         ROLE
!    MATREZ         OUT    K*     NOM DE LA MATR_ASSE RESULTANT DE LA
!                                 COMBINAISON LINEAIRE DES MATR_ASSE
!                                 DE LA LISTE LISMAT.
!    LIMAT          IN    K24     LISTE DES MATR_ASSE A COMBINER
!                                 DES MATR_ASSE A COMBINER.
!    NBMAT          IN    I       ON FAIT LA COMBINAISON LINEAIRE
!                                 DES NBMAT PREMIERS MATR_ASSE DE LA
!                                 LISTE LIMAT.
!    BASEZ          IN    K*      NOM DE LA BASE SUR LAQUELLE ON
!                                 CONSTRUIT LA MATR_ASSE.
!    NUMEDD         IN    K14    NOM DU NUME_DDL SUR LEQUEL S'APPUIERA
!                                 LA MATR_ASSE MATREZ
!        SI NUMEDD  =' ', LE NOM DU NUME_DDL SERA OBTENU PAR GCNCON
!        SI NUMEDD /=' ', ON PRENDRA NUMEDD COMME NOM DE NUME_DDL
!
!    LSYM           IN    L      /.TRUE.  : MATRICE SYMETRIQUE
!                                /.FALSE. : MATRICE NON-SYMETRIQUE
!    ROUC           IN    K1     /'R ' : MATRICE REELLE
!                                /'C'  : MATRICE COMPLEXE
!
!.========================= DEBUT DES DECLARATIONS ====================
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedup1.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jevtbl.h"
#include "asterfort/jexnum.h"
#include "asterfort/uttrii.h"
#include "asterfort/wkvect.h"
!
    real(kind=8) :: tmax
! -----  ARGUMENTS
    integer :: nbmat
    logical :: lsym
    character(len=*) :: matrez, basez, numedd
    character(len=*) :: limat(nbmat)
    character(len=1) :: rouc
! -----  VARIABLES LOCALES
    character(len=1) :: base
    character(len=14) :: numddl, numdd1, numddi
    character(len=19) :: matres, mat1, mati
    character(len=24) :: ksmhc, ksmdi, krefa, kconl, kvalm
    character(len=24) :: krefi, kliste
    integer :: lgbl, jhtc, i, iadi, jeq, nbter, jibl, jpbl, ibl1, lcumu, kbl
    integer :: jbl1
    integer :: iblav, idhcoi, icum, ismdi, lsmhc, nterm, idsmhc, l, jsmde
    integer :: itbloc, nbloc, kbloc, jrefa, idrefi, idconl, ieq
    integer :: ier, jsmde1, neq, k, htc
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    base = basez
    matres = matrez
!
!
! --- NOM DU NUME_DDL A CONSTRUIRE :
!     ----------------------------
    if (numedd .eq. ' ') then
        call gcncon('_', numddl(1:8))
        numddl(9:14) = '.NUDDL'
!
    else
        numddl = numedd
    endif
!
! --- NOM DE LA PREMIERE MATR_ASSE :
!     ----------------------------
    mat1 = limat(1)
!
! --- RECUPERATION DU NUME_DDL ATTACHE A LA PREMIERE MATR_ASSE  :
!     --------------------------------------------------------
    call dismoi('NOM_NUME_DDL', mat1, 'MATR_ASSE', repk=numdd1)
!
!
! --- RECOPIE DU PROF_CHNO DE LA PREMIERE MATRICE SUR LA MATRICE
! --- RESULTANTE :
!     ---------
    call jedupo(numdd1//'.NUME.DEEQ', base, numddl//'.NUME.DEEQ', .false.)
    call jedupo(numdd1//'.NUME.DELG', base, numddl//'.NUME.DELG', .false.)
    call jedupo(numdd1//'.NUME.LILI', base, numddl//'.NUME.LILI', .false.)
    call jedupo(numdd1//'.NUME.NUEQ', base, numddl//'.NUME.NUEQ', .false.)
    call jedupo(numdd1//'.NUME.PRNO', base, numddl//'.NUME.PRNO', .false.)
    call jedupo(numdd1//'.NUME.REFN', base, numddl//'.NUME.REFN', .false.)
    call jedupo(numdd1//'.NUME.NEQU', base, numddl//'.NUME.NEQU', .false.)
!
! --- RECOPIE DU .NSLV DE LA PREMIERE MATRICE SUR LA MATRICE
    call jedup1(numdd1//'.NSLV', base, numddl//'.NSLV')
!
! --- RECUPERATION DU NOMBRE D'EQUATIONS DE LA PREMIERE MATRICE
! --- A COMBINER (C'EST LE MEME POUR TOUTES LES MATRICES) :
!     ---------------------------------------------------
    call jeveuo(numdd1//'.SMOS.SMDE', 'L', jsmde1)
    neq = zi(jsmde1-1+1)
!
!
!     7) CONSTRUCTION DE L'OBJET KLISTE QUI CONTIENDRA LES DIFFERENTS
!        SMHC(MAT_I) MIS BOUT A BOUT (EQUATION PAR EQUATION) :
!        KLISTE(JEQ)=SMHC(IMAT_1)(JEQ)//SMHC(IMAT_2)(JEQ)//...
!        KLISTE EST ALLOUé PAR "BLOC" POUR EVITER D'UTILISER
!        TROP DE MEMOIRE
!     =========================================================
    kliste = '&&PROSMO.KLISTE'
!     RECUPERATION DE LA TAILLE DES BLOCS DONNEE DANS LA COMMANDE DEBUT:
    tmax = jevtbl('TAILLE_BLOC')
    lgbl = int(tmax*1024)
!
!     7-1) HTC : HAUTEUR CUMULEE DE KLISTE(JEQ)
!     --------------------------------------------------------
    call wkvect('&&PROSMO.HTC', 'V V I', neq, jhtc)
    do 20 i = 1, nbmat
        mati = limat(i)
        call dismoi('NOM_NUME_DDL', mati, 'MATR_ASSE', repk=numddi)
        call jeveuo(numddi//'.SMOS.SMDI', 'L', iadi)
        do 10 jeq = 1, neq
            if (jeq .eq. 1) then
                nbter = 1
!
            else
                nbter = zi(iadi-1+jeq) - zi(iadi-1+jeq-1)
            endif
            zi(jhtc-1+jeq) = zi(jhtc-1+jeq) + nbter
 10     continue
        call jelibe(numddi//'.SMOS.SMDI')
 20 end do
!
!     7-2) IBL : NUMERO DU BLOC DE KLISTE(JEQ) :
!          PBL : POSITION DE L'EQUATION JEQ DANS LE BLOC IBL  :
!     ------------------------------------------------------------
    call wkvect('&&PROSMO.IBL', 'V V I', neq, jibl)
    call wkvect('&&PROSMO.PBL', 'V V I', neq, jpbl)
    ibl1 = 1
    lcumu = 0
    do 30 jeq = 1, neq
        htc=zi(jhtc-1+jeq)
        ASSERT(htc.le.lgbl)
!       -- SI ON CHANGE DE BLOC :
        if (lcumu + htc .gt. lgbl) then
            ibl1 = ibl1 + 1
            lcumu = 0
        endif
        zi(jibl-1+jeq) = ibl1
        zi(jpbl-1+jeq) = lcumu
        lcumu = lcumu + htc
 30 end do
!
!     7-3) ALLOCATION DE KLISTE :
!     ------------------------------------------------------------
    if (ibl1 .eq. 1) lgbl = lcumu
    call jecrec(kliste, 'V V I', 'NU', 'DISPERSE', 'CONSTANT',&
                ibl1)
    call jeecra(kliste, 'LONMAX', lgbl)
    do 40 kbl = 1, ibl1
        call jeveuo(jexnum(kliste, kbl), 'E', jbl1)
        call jelibe(jexnum(kliste, kbl))
 40 end do
!
!     7-4) REMPLISSAGE DE KLISTE :
!     ------------------------------------------------------------
    call jedetr('&&PROSMO.HTC')
    call wkvect('&&PROSMO.HTC', 'V V I', neq, jhtc)
    iblav = 1
    call jeveuo(jexnum(kliste, iblav), 'E', jbl1)
    do 70 i = 1, nbmat
        mati = limat(i)
        call dismoi('NOM_NUME_DDL', mati, 'MATR_ASSE', repk=numddi)
        call jeveuo(numddi//'.SMOS.SMDI', 'L', iadi)
        call jeveuo(numddi//'.SMOS.SMHC', 'L', idhcoi)
        icum = 0
        do 60 jeq = 1, neq
            if (jeq .eq. 1) then
                nbter = 1
!
            else
                nbter = zi(iadi-1+jeq) - zi(iadi-1+jeq-1)
            endif
!
!         LE BLOC CONTENANT J DOIT-IL ETRE RAMENE EN MEMOIRE ?
            ibl1 = zi(jibl-1+jeq)
            if (iblav .ne. ibl1) then
                call jelibe(jexnum(kliste, iblav))
                call jeveuo(jexnum(kliste, ibl1), 'E', jbl1)
                iblav = ibl1
            endif
            do 50 k = 1, nbter
                zi(jbl1+zi(jpbl-1+jeq)+zi(jhtc-1+jeq)+k-1)=zi4(idhcoi+&
                icum+ (k-1))
 50         continue
            icum = icum + nbter
            zi(jhtc-1+jeq) = zi(jhtc-1+jeq) + nbter
 60     continue
        call jelibe(numddi//'.SMOS.SMDI')
        call jelibe(numddi//'.SMOS.SMHC')
 70 end do
    call jelibe(jexnum(kliste, iblav))
!
!
!     7-5) COMPACTAGE DE L'OBJET KLISTE
!     8)   ET CREATION  DU TABLEAU .SMDI
!     ===================================
    ksmdi = numddl//'.SMOS.SMDI'
    call wkvect(ksmdi, base//' V I', neq, ismdi)
!
    lsmhc = 0
    iblav = 1
    call jeveuo(jexnum(kliste, iblav), 'E', jbl1)
    do 80 jeq = 1, neq
!       LE BLOC CONTENANT JEQ DOIT-IL ETRE RAMENE EN MEMOIRE ?
        ibl1 = zi(jibl-1+jeq)
        if (iblav .ne. ibl1) then
            call jelibe(jexnum(kliste, iblav))
            call jeveuo(jexnum(kliste, ibl1), 'E', jbl1)
            iblav = ibl1
        endif
!
!       ON TRIE ET ORDONNE LA COLONNE (EN PLACE)
        nterm = zi(jhtc-1+jeq)
        call uttrii(zi(jbl1+zi(jpbl-1+jeq)), nterm)
        zi(jhtc-1+jeq) = nterm
        if (jeq .eq. 1) then
            ASSERT(nterm.eq.1)
            zi(ismdi+1-1) = nterm
!
        else
            zi(ismdi+jeq-1) = zi(ismdi+ (jeq-1)-1) + nterm
        endif
        lsmhc = lsmhc + nterm
 80 end do
    call jelibe(jexnum(kliste, iblav))
!
!
!     9) CREATION ET AFFECTATION DU TABLEAU .SMHC
!     ====================================================
    ksmhc = numddl//'.SMOS.SMHC'
    call wkvect(ksmhc, base//' V S', lsmhc, idsmhc)
    iblav = 1
    call jeveuo(jexnum(kliste, iblav), 'E', jbl1)
    l = 0
    do 100 jeq = 1, neq
!       LE BLOC CONTENANT JEQ DOIT-IL ETRE RAMENE EN MEMOIRE ?
        ibl1 = zi(jibl-1+jeq)
        if (iblav .ne. ibl1) then
            call jelibe(jexnum(kliste, iblav))
            call jeveuo(jexnum(kliste, ibl1), 'E', jbl1)
            iblav = ibl1
        endif
!
        nterm = zi(jhtc-1+jeq)
        do 90 k = 1, nterm
            l = l + 1
            zi4(idsmhc-1+l) = zi(jbl1+zi(jpbl-1+jeq)-1+k)
 90     continue
100 end do
    call jelibe(jexnum(kliste, iblav))
    call jedetr(kliste)
    call jedetr('&&PROSMO.HTC')
    call jedetr('&&PROSMO.IBL')
    call jedetr('&&PROSMO.PBL')
!
!
!     10) CREATION ET AFFECTATION DU TABLEAU .IABL
!     ========================================================
!
!
!     11) CREATION ET AFFECTATION DU TABLEAU .SMDE
!     =============================================
!
    call wkvect(numddl//'.SMOS.SMDE', base//' V I', 6, jsmde)
!
! --- RECUPERATION DE LA TAILLE DU BLOC DE LA MATRICE RESULTANTE
! --- (NOMBRE DE TERMES NON NULS DE LA MATRICE)
    itbloc = zi(ismdi+neq-1)
!
    zi(jsmde-1+1) = neq
    zi(jsmde-1+2) = itbloc
    zi(jsmde-1+3) = 1
!
!
!     12) CREATION ET AFFECTATION DE LA COLLECTION .VALM
!     ===================================================
    kvalm = matres//'.VALM'
    call jedetr(kvalm)
    if (lsym) then
        nbloc = 1
!
    else
        nbloc = 2
    endif
    call jecrec(kvalm, base//' V '//rouc, 'NU', 'DISPERSE', 'CONSTANT',&
                nbloc)
    call jeecra(kvalm, 'LONMAX', itbloc)
    do 110 kbloc = 1, nbloc
        call jecroc(jexnum(kvalm, kbloc))
110 end do
!
!
!     13) CREATION ET AFFECTATION DU TABLEAU .REFA
!     ============================================================
    krefa = matres//'.REFA'
    kconl = matres//'.CONL'
    call jeexin(krefa, ier)
    if (ier .eq. 0) then
        call wkvect(krefa, base//' V K24', 20, jrefa)
!
    else
        call jeveuo(krefa, 'E', jrefa)
    endif
    zk24(jrefa-1+2) = numddl
    if (lsym) then
        zk24(jrefa-1+9) = 'MS'
!
    else
        zk24(jrefa-1+9) = 'MR'
    endif
    zk24(jrefa-1+10) = 'NOEU'
!
    do 120 i = 1, nbmat
        mati = limat(i)
        krefi = mati//'.REFA'
        call jeveuo(krefi, 'L', idrefi)
        if (zk24(idrefi+1-1) .ne. ' ') then
            zk24(jrefa-1+1) = zk24(idrefi+1-1)
            goto 130
!
        endif
120 end do
130 continue
!
!
!     15) CREATION ET AFFECTATION DU VECTEUR .CONL
!     =============================================
    call wkvect(kconl, base//' V R', neq, idconl)
    do 140 ieq = 1, neq
        zr(idconl+ieq-1) = 1.d0
140 end do
!
    call jedema()
!
end subroutine
