subroutine mtcmbl(nbcomb, typcst, const, limat, matrez,&
                  ddlexc, numedd, elim)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cbval2.h"
#include "asterfort/cbvale.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/idenob.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedup1.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtconl.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/mtmchc.h"
#include "asterfort/prosmo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: nbcomb
    character(len=*) :: typcst(nbcomb), ddlexc
    character(len=*) :: matrez, numedd
    character(len=*) :: limat(nbcomb)
    character(len=5) :: elim
    real(kind=8) :: const(*)
!     ------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
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
!     COMBINAISON LINEAIRE DE MATRICES  :
!     -------------------------------------
!     MAT_RES= SOMME(ALPHA_I*MAT_I)
!
!       *  LES MATRICES (MAT_I) DOIVENT AVOIR LA MEME NUMEROTATION DES
!           DDLS MAIS ELLES PEUVENT AVOIR DES CONNECTIVITES DIFFERENTES
!          (I.E. DES STOCKAGES DIFFERENTS)
!       *  LES MATRICES (MAT_I) SONT REELLES OU COMPLEXES
!       *  LES MATRICES (MAT_I) SONT SYMETRIQUES OU NON
!       *  LES COEFFICIENTS (ALPHA_I) SONT REELS OU COMPLEXES
!       *  ON PEUT MELANGER MATRICES REELLES ET COMPLEXES ET LES TYPES
!          (R/C) DES COEFFICIENTS. ON PEUT FAIRE PAR EXEMPLE :
!          MAT_RES= ALPHA_R1*MAT_C1 + ALPHA_C2*MAT_R2
!       *  MAT_RES DOIT ETRE ALLOUEE AVANT L'APPEL A MTCMBL
!          CELA VEUT DIRE QUE SON TYPE (R/C) EST DEJA DETERMINE.
!       *  SI TYPE(MAT_RES)=R ET QUE CERTAINS (MAT_I/ALPHA_I) SONT C,
!          CELA VEUT SIMPLEMENT DIRE QUE MAT_RES CONTIENDRA LA PARTIE
!          REELLE DE LA COMBINAISON LINEAIRE (QUI EST COMPLEXE)
!
!---------------------------------------------------------------------
! IN  I  NBCOMB = NOMBRE DE MATRICES A COMBINER
! IN  V(K1) TYPCST = TYPE DES CONSTANTES (R/C)
! IN  V(R)  CONST  = TABLEAU DE R*8    DES COEFICIENTS
!     ATTENTION : CONST PEUT ETRE DE DIMENSION > NBCOMB CAR
!                 LES COEFS COMPLEXES SONT STOCKES SUR 2 REELS
! IN  V(K19) LIMAT = LISTE DES NOMS DES MATR_ASSE A COMBINER
! IN/JXOUT K19 MATREZ = NOM DE LA MATR_ASSE RESULTAT
!        CETTE MATRICE DOIT AVOIR ETE CREEE AU PREALABLE (MTDEFS)
! IN  K* DDLEXC = NOM DU DDL A EXCLURE ("LAGR"/" " )
!
! SI LES MATRICES COMBINEES N'ONT PAS LE MEME STOCKAGE, IL FAUT
! CREER UN NOUVEAU NUME_DDL POUR CE STOCKAGE :
! IN/JXOUT  K14 NUMEDD = NOM DU NUME_DDL SUR LEQUEL S'APPUIERA MATREZ
!        SI NUMEDD ==' ', LE NOM DU NUME_DDL SERA OBTENU PAR GCNCON
!        SI NUMEDD /=' ', ON PRENDRA NUMEDD COMME NOM DE NUME_DDL
! IN    K5  : / 'ELIM=' : SI LES MATRICES A COMBINER N'ONT PAS LES MEMES
!                         DDLS ELIMINES (CHAR_CINE) => ERREUR <F>
!             / 'ELIM1' : LA MATRICE RESULTAT AURA LES MEMES DDLS
!                         ELIMINES QUE LA 1ERE MATRICE DE LA LISTE LIMAT
!---------------------------------------------------------------------
!     -----------------------------------------------------------------
    character(len=1) :: base, bas2, typres
    character(len=8) :: typmat, kmpic, kmpic1, kmatd
    character(len=19) :: matemp, mat1, matres, mati
    character(len=24) :: valk(2)
!     -----------------------------------------------------------------
    integer :: jrefar, jrefa1, jrefai, ier, ibid, idlima, ier1
    integer :: i, lres, nbloc, jrefa, lgbloc
    logical :: reutil, symr, symi, matd
!     -----------------------------------------------------------------
!
    call jemarq()
!
    ASSERT(elim.eq.'ELIM=' .or. elim.eq.'ELIM1')
!
    matres = matrez
    mat1=limat(1)
    ASSERT(nbcomb.ge.1)
    call jelira(matres//'.REFA', 'CLAS', cval=base)
    call jelira(matres//'.VALM', 'TYPE', cval=typres)
    call jelira(matres//'.VALM', 'NMAXOC', nbloc)
    call jelira(matres//'.VALM', 'LONMAX', lgbloc)
    ASSERT(nbloc.eq.1.or.nbloc.eq.2)
    call jeveuo(matres//'.REFA', 'E', jrefar)
    ASSERT(zk24(jrefar-1+9) (1:1).eq.'M')
    symr = zk24(jrefar-1+9) .eq. 'MS'
    if (symr) then
        ASSERT(nbloc.eq.1)
    else
        ASSERT(nbloc.eq.2)
    endif
!
    ASSERT(ddlexc.eq.' '.or.ddlexc.eq.'LAGR')
    call wkvect('&&MTCMBL.LISPOINT', 'V V I', nbcomb, idlima)
    reutil=.false.
    do 10 i = 1, nbcomb
        ASSERT(typcst(i).eq.'R'.or.typcst(i).eq.'C')
        mati=limat(i)
        call jeveuo(mati//'.REFA', 'L', jrefai)
        if (zk24(jrefai-1+3) .eq. 'ELIMF') call mtmchc(mati, 'ELIML')
        call mtdscr(mati)
        call jeveuo(mati//'.&INT', 'E', zi(idlima+i-1))
        call jelira(mati//'.VALM', 'TYPE', cval=typmat)
        call jelira(mati//'.VALM', 'NMAXOC', nbloc)
        call jeveuo(mati//'.REFA', 'L', jrefai)
        symi = zk24(jrefai-1+9) .eq. 'MS'
        if (symi) then
            ASSERT(nbloc.eq.1)
        else
            ASSERT(nbloc.eq.2)
            ASSERT(.not.symr)
        endif
!        IF ((.NOT.SYMI).AND.SYMR) CHGSYM=.TRUE.
        if (mati .eq. matres) reutil=.true.
10  end do
!
!
!     -- SI LA MATRICE RESULTAT EST L'UNE DE CELLES A COMBINER,
!        IL NE FAUT PAS LA DETRUIRE !
!     ------------------------------------------------------------
    if (reutil) then
        matemp='&&MTCMBL.MATEMP'
        call mtdefs(matemp, matres, 'V', typres)
    else
        matemp = matres
    endif
    call jelira(matemp//'.REFA', 'CLAS', cval=bas2)
!
!
! --- VERIF. DE LA COHERENCE MPI DES MATRICES A COMBINER
!     ----------------------------------------------------
    call dismoi('F', 'MPI_COMPLET', mat1, 'MATR_ASSE', ibid,&
                kmpic1, ibid)
    if (kmpic1 .eq. 'OUI') then
        zk24(jrefar-1+11)='MPI_COMPLET'
    else
        zk24(jrefar-1+11)='MPI_INCOMPLET'
    endif
    matd = .false.
    call dismoi('F', 'MATR_DISTR', mat1, 'MATR_ASSE', ibid,&
                kmatd, ibid)
    if (kmatd .eq. 'OUI') then
        matd = .true.
        zk24(jrefar-1+11)='MATR_DISTR'
    endif
    do 19 i = 2, nbcomb
        mati=limat(i)
        call dismoi('F', 'MPI_COMPLET', mati, 'MATR_ASSE', ibid,&
                    kmpic, ibid)
        if (kmpic .ne. kmpic1) then
            valk(1)=mat1
            valk(2)=mati
            call utmess('F', 'CALCULEL6_55', nk=2, valk=valk)
        endif
        call dismoi('F', 'MATR_DISTR', mati, 'MATR_ASSE', ibid,&
                    kmatd, ibid)
!       IL EST NECESSAIRE QUE TOUTES LES MATRICES QU'ON CHERCHE A
!       COMBINER SOIT DU MEME TYPE (SOIT TOUTES DISTRIBUEES,
!       SOIT TOUTES COMPLETES MAIS SURTOUT PAS DE MELANGE !)
        if (kmatd .eq. 'OUI') then
            if (.not.matd) ASSERT(.false.)
        else
            if (matd) ASSERT(.false.)
        endif
19  end do
!
!
! --- VERIF. DE LA COHERENCE DES NUMEROTATIONS DES MATRICES A COMBINER
!     ------------------------------------------------------------------
    call jeveuo(mat1//'.REFA', 'L', jrefa1)
    ier1 = 0
    do 20 i = 2, nbcomb
        mati=limat(i)
        call jeveuo(mati//'.REFA', 'L', jrefai)
        if (zk24(jrefa1-1+2) .ne. zk24(jrefai-1+2)) ier1 = 1
        if (zk24(jrefa1-1+2) .ne. zk24(jrefai-1+2)) ier1 = 1
        if (zk24(jrefa1-1+1) .ne. zk24(jrefai-1+1)) then
            call utmess('F', 'ALGELINE2_9')
        endif
        if (elim .eq. 'ELIM=') then
            if (.not.idenob(mat1//'.CCID',mati//'.CCID')) then
                valk(1)=mat1
                valk(2)=mati
                call utmess('F', 'ALGELINE2_10', nk=2, valk=valk)
            endif
        endif
20  end do
!
!
!
! --- 2) COMBINAISON LINEAIRE DES .VALM DES MATRICES :
!     ================================================
!
! ---   CAS OU LES MATRICES A COMBINER ONT LE MEME PROFIL :
!       -------------------------------------------------
    if (ier1 .eq. 0) then
        call mtdscr(matemp)
        call jeveuo(matemp//'.&INT', 'E', lres)
        call cbvale(nbcomb, typcst, const, zi(idlima), typres,&
                    lres, ddlexc, matd)
!
! ---   CAS OU LES MATRICES A COMBINER N'ONT PAS LE MEME PROFIL :
!       -------------------------------------------------------
    else
!       SI LES MATRICES SONT DISTRIBUEE MAIS N'ONT PAS LE MEME
!       PROFIL, ON PLANTE !
        if (matd) then
            call utmess('F', 'ALGELINE5_1')
        endif
        call prosmo(matemp, limat, nbcomb, base, numedd,&
                    symr, typres)
        call mtdscr(matemp)
        call jeveuo(matemp//'.&INT', 'E', lres)
        call cbval2(nbcomb, typcst, const, zi(idlima), typres,&
                    lres, ddlexc)
    endif
!
!
! --- DDL ELIMINES :
!     ===================
    call jeveuo(matemp//'.REFA', 'L', jrefa)
    call jedetr(matemp//'.CCID')
    call jedetr(matemp//'.CCVA')
    call jedetr(matemp//'.CCLL')
    call jedetr(matemp//'.CCII')
    call jedup1(mat1//'.CCID', bas2, matemp//'.CCID')
    call jeexin(matemp//'.CCID', ier)
    if (ier .gt. 0) zk24(jrefa-1+3)='ELIML'
!
!
! --- CONSTRUCTION DU DESCRIPTEUR DE LA MATRICE RESULTAT :
!     ==================================================
    call mtdscr(matemp)
    call jeveuo(matemp(1:19)//'.&INT', 'E', lres)
!
!
! --- COMBINAISON LINEAIRE DES .CONL DES MATRICES SI NECESSAIRE :
!     =========================================================
    if (ddlexc .ne. 'LAGR') then
        call mtconl(nbcomb, typcst, const, zi(idlima), typres,&
                    lres)
    else
        call jedetr(zk24(zi(lres+1))(1:19)//'.CONL')
    endif
!
!
!     -- ON REMET LA MATRICE DANS L'ETAT 'ASSE' :
    call jeveuo(matres//'.REFA', 'E', jrefar)
    zk24(jrefar-1+8)='ASSE'
!
    if (reutil) then
        call copisd('MATR_ASSE', base, matemp, matres)
        call detrsd('MATR_ASSE', matemp)
    endif
!
    call jedetr('&&MTCMBL.LISPOINT')
!
    call jedema()
end subroutine
