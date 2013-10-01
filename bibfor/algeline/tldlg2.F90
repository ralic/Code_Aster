subroutine tldlg2(lmat, nprec, nmrig, vemrig)
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
! person_in_charge: jacques.pellet at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/csmbgg.h"
#include "asterfort/detrsd.h"
#include "asterfort/diagav.h"
#include "asterfort/dismoi.h"
#include "asterfort/imprsd.h"
#include "asterfort/infbav.h"
#include "asterfort/infmue.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/mtmchc.h"
#include "asterfort/mulfr8.h"
#include "asterfort/rgndas.h"
#include "asterfort/rldlg3.h"
#include "asterfort/rltfr8.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcrem.h"
#include "asterfort/wkvect.h"
    integer :: lmat, nprec, nmrig
    character(len=*) :: vemrig
!
!
! ----------------------------------------------------------------------
!
!  RECHERCHE DE MODES DE CORPS RIGIDE PAR DECOMPOSITION DE GAUSS
!
! ----------------------------------------------------------------------
!
!
!          POUR LE PROBLEME AUX VALEURS PROPRES :
!                        (K) X = 0
!          LA MATRICE (K) EST REELLE SYMETRIQUE
!          LES VALEURS PROPRES ET LES VECTEURS PROPRES SONT REELS
!          ON DECOMPOSE (K) SOUS LA FORME
!                        (  KEE   KER  )
!                        (  TKER  KRR  )
!          AVEC DIM DE KRR = NOMBRE DE MODES DE CORPS RIGIDE DE (K)
!          ET KEE INVERSIBLE
!          LES MODES DE CORPS RIGIDE SONT LES VECTEURS DE LA MATRICE
!                   (  KEE(-1)KER  )
!                   (     -ID(R)    )
!          ET OBTENUS AVEC DES ROUTINES DEVELOPPPEES POUR TRAITER LES
!          BLOCAGES CINEMATIQUES
!
! IN  LMAT : DESCRIPTEUR DE LA MATRICE DONT ON CHERCHE LES MODES RIGIDES
! IN  NPREC: SI DIFFERENT DE 0,NIVEAU DE PERTE DE DECIMALES A PARTIR
!            DUQUEL ON CONSIDERE QU'UN PIVOT EST NUL.SI 0, CE SERA 8
! OUT NMRIG: NOMBRE DE MODES DE CORPS RIGIDE
! IN/JXOUT : VEMRIG : NOM DE L'OBJET CONTENANT LES MODES DE CORPS RIGIDE
! IN/JXOUT : VEINPN : VECTEUR DES INDICES DE PIVOTS NULS
!
!
    character(len=8) :: nomno, nomcmp, tyddl, renum
    character(len=16) :: metres
    character(len=14) :: nu
    character(len=19) :: noma19, nomb19, ligrel
    character(len=40) :: infobl
    complex(kind=8) :: cbid
    integer :: ndeci, isingu, nom, neq, typvar, typsym
    integer :: lmatb, ndigi2, npivot, jdelg, ksing, nmrav, jksing
    integer :: ifm, niv
    integer :: pass, ieq, jeq, krig, jpomr, ibid, lxsol
    integer :: lcine
    integer :: jdigs, jrefab, jccid
    real(kind=8) :: epsb, d1, moydia
    cbid = dcmplx(0.d0, 0.d0)
!
! ----------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)
!
    nom=zi(lmat+1)
    neq=zi(lmat+2)
    typvar=zi(lmat+3)
    typsym=zi(lmat+4)
    noma19=zk24(nom)(1:19)
    call dismoi('F', 'NOM_NUME_DDL', noma19, 'MATR_ASSE', ibid,&
                nu, ibid)
    call jeveuo(nu//'.NUME.DELG', 'L', jdelg)
    ASSERT(nu.ne.' ')
    metres='MULT_FRONT'
    renum='METIS'
!
!
!
!     -- MONITORING ET VERIFICATIONS :
    if (niv .ge. 2) then
        write (ifm,*)&
     &    '<TLDLG2> RECHERCHE DE MODES RIGIDES DE LA MATRICE :',noma19
        write (ifm,*)'<TLDLG2> DESCRIPTION DE LA MATRICE ',noma19,':'
    endif
!
    if (typsym .eq. 1) then
        if (niv .ge. 2) write (ifm,*)'<TLDLG2> MATRICE SYMETRIQUE'
    else
        if (niv .ge. 2) write (ifm,*)'<TLDLG2> MATRICE NON-SYMETRIQUE'
        call utmess('F', 'ALGELINE3_46', sk=noma19)
    endif
    ASSERT(typsym.eq.1)
!
    if (typvar .eq. 1) then
        if (niv .ge. 2) write (ifm,*)'<TLDLG2> MATRICE REELLE'
    else
        if (niv .ge. 2) write (ifm,*)'<TLDLG2> MATRICE COMPLEXE'
        call utmess('F', 'ALGELINE3_47', sk=noma19)
    endif
    ASSERT(typvar.eq.1)
!
    if (niv .ge. 2) write (ifm,*)'<TLDLG2> METHODE MULT_FRONT'
!
!
!
!     -- PERTE DE DECIMALES POUR LAQUELLE ON CONSIDERE LE PIVOT NUL.
!        DEFAUT: 8
    if (nprec .eq. 0) then
        ndigi2=8
    else
        ndigi2=nprec
    endif
!
!
!     -- CE VECTEUR CONTIENDRA DES 0,ET LE NUMERO,LA OU ON A BLOQUE
    call wkvect('&&TLDLG2.POSMODRI', 'V V I ', neq, jpomr)
!
!     -- CREATION DE LA MATRICE DE TRAVAIL (COPIE DE NOMA19)
    nomb19='&&TLDLG2.COPIEMATA'
    call copisd('MATR_ASSE', 'V', noma19, nomb19)
    call mtdscr(nomb19)
    call jeveuo(nomb19//'.REFA', 'E', jrefab)
    call jeveuo(nomb19//'.&INT', 'E', lmatb)
!
!     -- PRISE EN COMPTE DES CHARGES CINEMATIQUES :
    ASSERT(zk24(jrefab-1+3).ne.'ELIMF')
    if (zk24(jrefab-1+3) .eq. 'ELIML') call mtmchc(nomb19, 'ELIMF')
    ASSERT(zk24(jrefab-1+3).ne.'ELIML')
!
!
!
!     -- CREATION DE L'OBJET .DIGS: DIAGONALE AVANT ET APRES
    call diagav(nomb19, neq, neq, typvar, epsb)
    call jeveuo(nomb19//'.DIGS', 'L', jdigs)
!
!     -- VALEUR ABSOLUE MOYENNE DE LA DIAGONALE
    moydia=0.d0
    do ieq = 1, neq
        moydia=moydia+abs(zr(jdigs-1+ieq))
    end do
!
!
!     -- 1. RECHERCHE DES PIVOTS NULS DE LA MATRICE :
!     ----------------------------------------------------------------
    call wkvect('&&TLDLG2.KSINGU', 'V V I ', neq, jksing)
    nmrig=0
!
!     -- DEBUT BOUCLE: TANT QU'IL EXISTE DES PIVOTS NULS
 20 continue
    pass=0
    nmrav=nmrig
!     -- FACTORISATION : SI NPIVOT.NE.0 ALORS KI SINGULIERE ET
!        NPIVOT CONTIENT LE NUMERO DE LIGNE DU PREMIER PIVOT .LT. EPSB
    call mulfr8(nomb19, npivot, neq, typsym, epsb,&
                renum)
!
    if (npivot .ge. 1) then
!       -- 1.1 LA FACT. S'EST ARRETEE SUR UN PIVOT VRAIMENT NUL
        nmrig=nmrig+1
        zi(jksing-1+nmrig)=npivot
    else
!       -- 1.2 LA FACT. A PEUT ETRE CALCULE DES PIVOTS QUASI-NULS
!          POUR EVITER DE FACTORISER PLUSIEURS FOIS (COUT), ON RELEVE
!          TOUS LES PETITS PIVOTS OBTENUS.
        do ieq = 1, neq
            d1=abs(zr(jdigs-1+ieq)/zr(jdigs+neq-1+ieq))
            if (d1 .gt. 0.d0) then
                ndeci=int(log10(d1))
            else
                ndeci=0
            endif
            if (ndeci .ge. ndigi2) then
                nmrig=nmrig+1
                zi(jksing-1+nmrig)=ieq
            endif
        end do
    endif
!
!     -- 1.3 SI ON A RENCONTRE DE NOUVEAUX PIVOTS NULS :
    do ksing = nmrav+1, nmrig
        pass=1
        isingu=zi(jksing-1+ksing)
        ASSERT(isingu.gt.0 .and. isingu.le.neq)
!       -- CE SERAIT BIZARRE QUE ISINGU SOIT UN DDL DE LAGRANGE :
        ASSERT(zi(jdelg-1+isingu).eq.0)
        zi(jpomr-1+isingu)=ksing
        if (niv .ge. 2) then
            write (ifm,*)'<TLDLG2> PIVOT NUL A LA LIGNE ',isingu
            call rgndas(nu, isingu, nomno, nomcmp, tyddl,&
                        ligrel, infobl)
            ASSERT(tyddl.eq.'A'.or.tyddl.eq.'D')
            write (ifm,*)'<TLDLG2> NOEUD ',nomno,' CMP ',nomcmp
        endif
    end do
!
    if (pass .ne. 0) then
        goto 40
    else
        goto 60
    endif
!
 40 continue
!
!     -- 1.3 REINITIALISATION DE B
    call detrsd('MATR_ASSE', nomb19)
    nomb19='&&TLDLG2.COPIEMATA '
    call copisd('MATR_ASSE', 'V', noma19, nomb19)
    call mtdscr(nomb19)
!     -- BLOCAGE 'CINEMATIQUE' DU/DES DDL A PIVOT NUL
    call jeveuo(nomb19//'.REFA', 'E', jrefab)
    if (zk24(jrefab-1+3) .eq. 'ELIMF') call mtmchc(nomb19, 'ELIML')
    ASSERT(zk24(jrefab-1+3).ne.'ELIMF')
    if (zk24(jrefab-1+3) .eq. 'ELIML') then
        call jeveuo(nomb19//'.CCID', 'E', jccid)
    else
        call wkvect(nomb19//'.CCID', 'V V I', neq+1, jccid)
    endif
    do ieq = 1, neq
        if (zi(jpomr-1+ieq) .gt. 0) zi(jccid-1+ieq)=1
    end do
    zi(jccid-1+neq+1)=nmrig
    zk24(jrefab-1+3)='ELIML'
    call mtmchc(nomb19, 'ELIMF')
    call diagav(nomb19, neq, neq, typvar, epsb)
    call jeveuo(nomb19//'.DIGS', 'L', jdigs)
    goto 20
!
!     -- FIN RECHERCHE DES PIVOTS NULS :
 60 continue
!
!
!     -- 1.4 : FIN DE BOUCLE RECHERCHE NMRIG :
    call jedetr('&&TLDLG2.KSINGU')
    if (niv .ge. 1) then
        write (ifm,9000)
        write (ifm,*)'<TLDLG2> NB DE MODES DE CORPS RIGIDES'//&
        ' DETECTES: ',nmrig
    endif
    if (nmrig .ge. 7) then
        call utmess('A', 'ALGELINE3_49')
    endif
!
!
!
!
!     -- 2. CALCUL DES MODES DE CORPS RIGIDE :
!     -----------------------------------------------------------------
    if (nmrig .ne. 0) then
        call mtdscr(nomb19)
        call jeveuo(nomb19//'.&INT', 'E', lmatb)
!
!       CONSTRUCTION DES SECONDS MEMBRES DU TYPE (KER -ID(R)) DANS
!       ZR(LXSOL, LXSOL+NEQ .... LXSOL+(NMRIG-1)*NEQ) POUR R VARIANT
!       DE 1 A NMRIG
!
        call wkvect(vemrig, 'V V R ', neq*nmrig, lxsol)
        call wkvect('&&TLDLG2.TLSECCIN', 'V V R ', neq*nmrig, lcine)
!
!       -- REMPLISSAGE POUR AVOIR FI=0 ET U0=-1 (NOTATION CSMBGG)
        krig=1
        do jeq = 1, neq
            if (zi(jpomr+jeq-1) .ne. 0) then
                zr(lcine+(krig-1)*neq+jeq-1)=-1.d0
                krig=krig+1
            endif
        end do
!
        do krig = 1, nmrig
!       -----------------------------------------------------------
!         CSMBGG : CALCUL DE LA CONTRIBUTION AU SECOND MEMBRE DES
!         DDLS IMPOSES LORSQU'ILS SONT TRAITEES PAR ELIMINATION :
!                ! K    K   ! (ON VEUT ELIMINER LES DDLS R)
!         K  =   !  EE   ER !
!                !  T       !
!                ! K    K   !
!                !  RE   RR !
!
!         LE TRAITEMENT PAR ELIMINATION CONSISTE A RESOUDRE :
!           ! K    0 !   ! X  !   ! FI  -K  U0!
!           !  EE    !   !  E !   !      ER   !
!           !        ! * !    ! = !           !
!           ! 0    1 !   ! X  !   ! 0     U0  !
!           !        !   !  R !   !           !
!
            call csmbgg(lmatb, zr(lxsol+(krig-1)*neq), zr(lcine+(krig- 1)*neq), [cbid], [cbid],&
                        'R')
        end do
!
!       -- REMARQUE : ON N'A PAS BESOIN D'UTILISER MRCONL CAR
!          LES DDLS DUALISES SONT MIS A 0. (ALPHA*0=0 !)
        call rldlg3(metres, lmatb, zr(lxsol), [cbid], nmrig)
    endif
    call detrsd('MATR_ASSE', nomb19)
!
!
!
!
!     -- NETTOYAGE ET SORTIE :
!     -------------------------
    if (niv .ge. 1) write (ifm,9010)
    call jedetr('&&TLDLG2.POSMODRI')
    call jedetr('&&TLDLG2.TLSECCIN')
    call jedema()
!
    9000 format (72x,/)
    9010 format (72('-'),/)
end subroutine
