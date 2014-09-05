subroutine assmam(base, matas, nbmat, tlimat, licoef,&
                  nu, motcle, itysca)
!----------------------------------------------------------------------
!  attention : cette routine ne doit pas etre appellee directement :
!              il faut appeler son "chapeau" : asmatr
!----------------------------------------------------------------------
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/cheksd.h"
#include "asterc/getres.h"
#include "asterfort/asmpi_barrier.h"
#include "asterfort/asmpi_comm_vect.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/assma1.h"
#include "asterfort/assma2.h"
#include "asterfort/assma3.h"
#include "asterfort/crelil.h"
#include "asterfort/detrsd.h"
#include "asterfort/digdel.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/infniv.h"
#include "asterfort/jaexin.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedbg2.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/nbno.h"
#include "asterfort/parti0.h"
#include "asterfort/teattr.h"
#include "asterfort/typmat.h"
#include "asterfort/utimsd.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerobj.h"
!
    character(len=*) :: base, matas, tlimat(*), nu
    integer :: nbmat, itysca
    real(kind=8) :: licoef(*)
    character(len=4) :: motcle
!-----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
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
!     ASSEMBLAGE MORSE AVEC PRECONDITIONNEMENT DES MATR_ELEM DE MAILLES
!     "LAGRANGE" PAR -(MAX(!A(I,I)!)+MIN(!A(I,I)!))/2
!-----------------------------------------------------------------------
! --- DESCRIPTION DES PARAMETRES
! INT K* BASE   : BASE SUR LAQUELLE ON VEUT CREER LA MATR_ASSE
! OUT K* MATAS  :L'OBJET MATAS DE TYPE MATR_ASSE EST CREE ET REMPLI
! IN  K* MATAS  : NOM DE L'OBJET DE TYPE MATR_ASSE A CREER
! IN  I  NBMAT  : NOMBRE DE MATR_ELEM  DE LA LISTE TLIMAT
! IN  K* TLIMAT : LISTE DES MATR_ELEM
! IN  I  LICOEF : LISTE DES COEFFICIENTS MULTIPLICATEURS DES MATR_ELEM
! IN  K* NU     : NOM DU NUMERO_DDL
! IN  K4 MOTCLE : 'ZERO' OU 'CUMU'
!                 'ZERO':SI UN OBJET DE NOM MATAS ET DE TYPE
!                        MATR_ASSE EXISTE ON L'ECRASE
!                 'CUMU':SI UN OBJET DE NOM MATAS ET DE TYPE
!                        MATR_ASSE EXISTE ON L'ENRICHI
! IN  I   ITYSCA  : TYPE (R/C) DE LA MATR_ASSE
!                          1 --> REELLES
!                          2 --> COMPLEXES
!-----------------------------------------------------------------------
    character(len=16) :: optio, optio2, codvoi, nomte
!-----------------------------------------------------------------------
    character(len=1) :: base1, typsca
    character(len=2) :: tt
    character(len=8) :: k8bid, nogdco, nogdsi, ma, ma2, mo, mo2, partit
    character(len=8) :: symel, kempic, kampic, exivf
    character(len=12) :: vge
    character(len=14) :: nudev, nu14
    character(len=16) :: k16bid, nomcmd
    character(len=19) :: matdev, mat19, resu, matel, ligre1
    character(len=1) :: matsym
    real(kind=8) :: c1, temps(6)
!
    aster_logical :: acreer, cumul, dbg, ldist
    aster_logical :: lmasym, lmesym, ldgrel
!
    integer :: admodl, i
    integer :: jdesc
    integer :: jadli, jadne, jnueq, jnulo1, jnulo2
    integer :: jposd1, jposd2, jtmp2, lgtmp2
    integer :: ibid, iconx1, iconx2, idbgav
    integer :: jprn1, jprn2, jresl, jrsvi
    integer :: iel, ier, ifm, igr
    integer :: ilima, ilimat, ilimo, ilinu
    integer :: imat, jnumsd, iresu
    integer :: iret, itbloc
    integer :: jrefa, jsmdi, jsmhc, jvalm(2)
    integer :: lcmodl, mode, n1, nbelm
    integer :: nblc, nbnomx, nbnoss, nbresu
    integer :: ncmp, nbvel, nec, nel, nequ, nbproc, vali(4)
    integer :: niv, nlili, nmxcmp, nnoe, jptvoi, jelvoi
    integer :: nugd, rang, ieq, idia, ellagr, jrepe, itypel, imatd, iexi
    character(len=24), pointer :: prtk(:) => null()
    integer, pointer :: smde(:) => null()
    character(len=24), pointer :: noli(:) => null()
    integer, pointer :: prti(:) => null()
    character(len=24), pointer :: relr(:) => null()
    character(len=16), pointer :: nvge(:) => null()
!
!-----------------------------------------------------------------------
!     FONCTIONS FORMULES :
!-----------------------------------------------------------------------
    mpi_int :: mrank, msize
!
#define zzngel(ili) zi(jadli+3*(ili-1))
#define zznelg(ili,igrel) zi(zi(jadli+3*(ili-1)+2)+igrel)- \
    zi(zi(jadli+3*(ili-1)+2)+igrel-1)-1
#define zzliel(ili,igrel,iel) zi(zi(jadli+3*(ili-1)+1)-1+ \
    zi(zi(jadli+3*(ili-1)+2)+igrel-1)+iel-1)
!----------------------------------------------------------------------
!
!
!
    call jemarq()
    dbg=.false.
    call jedbg2(idbgav, 0)
    call infniv(ifm, niv)
    call asmpi_barrier()
    call uttcpu('CPU.CALC.1', 'DEBUT', ' ')
    call uttcpu('CPU.ASSE.1', 'DEBUT', ' ')
    call uttcpu('CPU.ASSE.2', 'DEBUT', ' ')
!
    base1=base
    matdev=matas
    nudev=nu
    if (dbg) call cheksd(nudev, 'SD_NUME_DDL', iret)
!
    call dismoi('NOM_MODELE', nudev, 'NUME_DDL', repk=mo)
    call dismoi('NOM_MAILLA', mo, 'MODELE', repk=ma)
    call dismoi('NOM_MAILLA', nudev, 'NUME_DDL', repk=ma2)
    ASSERT(ma.eq.ma2)
    call dismoi('NB_NO_SS_MAX', ma, 'MAILLAGE', repi=nbnoss)
    call dismoi('NOM_GD', nudev, 'NUME_DDL', repk=nogdco)
    call dismoi('NOM_GD_SI', nogdco, 'GRANDEUR', repk=nogdsi)
    call dismoi('NB_CMP_MAX', nogdsi, 'GRANDEUR', repi=nmxcmp)
    ncmp=nmxcmp
    call dismoi('NUM_GD_SI', nogdsi, 'GRANDEUR', repi=nugd)
    nec=nbec(nugd)
    call jeveuo(jexatr('&CATA.TE.MODELOC', 'LONCUM'), 'L', lcmodl)
    call jeveuo(jexnum('&CATA.TE.MODELOC', 1), 'L', admodl)
    call jeexin(ma//'.CONNEX', iret)
    if (iret .gt. 0) then
        call jeveuo(ma//'.CONNEX', 'L', iconx1)
        call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', iconx2)
    else
        iconx1=0
        iconx2=0
    endif
!
    call jeexin(nudev//'.NUML.NULG', imatd)
    if (imatd .ne. 0) then
        call jeveuo(nudev//'.NUML.NUEQ', 'L', jnueq)
    else
        call jeveuo(nudev//'.NUME.NUEQ', 'L', jnueq)
    endif
!
!   ELLAGR : 0 : PAS D'ELEMENT DE LAGRANGE
!            1 : IL EXISTE DES ELEMENTS DE LAGRANGE
    ellagr=0
!   KAMPIC : 'OUI' -> LA MATR_ASSE EST 'MPI_COMPLET'
    kampic='OUI'
!
!
!
!
!     -- CALCUL DE :
!       LMASYM: .TRUE   : MATRICE ASSEMBLEE SYMETRIQUE
!               .FALSE. : MATRICE ASSEMBLEE NON-SYMETRIQUE
!       ACREER: .TRUE.  : IL FAUT CREER LA MATR_ASSE
!               .FALSE. : LA MATR_ASSE EXISTE DEJA
!       CUMUL : .TRUE.  : ON ACCUMULE DANS LA MATR_ASSE
!               .FALSE. : ON REMET LA MATR_ASSE A ZERO
!                         (ELLE DOIT EXISTER)
!       TT  : TT(1) : TYPE (R/C) DE CE QUE L'ON ASSEMBLE
!             TT(2) : TYPE (R/C) DE LA SD_MATR_ASSE
!     ------------------------------------------------------
    matsym=typmat(nbmat,tlimat)
    ASSERT(matsym.eq.'S' .or. matsym.eq.'N')
    lmasym=(matsym.eq.'S')
    if (motcle(1:4) .eq. 'ZERO') then
        cumul=.false.
        acreer=.true.
    else if (motcle(1:4).eq.'CUMU') then
        cumul=.true.
        acreer=.false.
        call jelira(matdev//'.VALM', 'NMAXOC', nblc)
        ASSERT(nblc.eq.1 .or. nblc.eq.2)
        if (nblc .eq. 2) lmasym=.false.
    else
        ASSERT(.false.)
    endif
    ASSERT((itysca.eq.1) .or. (itysca.eq.2))
    if (itysca .eq. 1) then
        tt='?R'
    else if (itysca.eq.2) then
        tt='?C'
    endif
    !
    call jelira(nudev//'.NUME.REFN', 'LONMAX', n1)
    ASSERT(n1.eq.4)
!
! ------------------------------------------------------------------
!
!     -- IL EXISTE DEUX FORMES DE CALCUL DISTRIBUE (DS LE SENS TOUS LES
!        PROCESSEURS NE CONNAISSENT QU'UNE PARTIE DE LA MATRICE) BASES
!        SUR UNE PARTITION:
!        * // DISTRIBUE
!        * // DISTRIBUE AVEC MUMPS + OPTION MATR_DISTIBUEE
!
!     -- IL EXISTE DEUX FORMES DE CALCUL CENTRALISE (TOUS LES PROC CON
!        NAISSENT LA MATRICE):
!        * // CENTRALISE
!        * // DISTRIBUE DANS ASSE_MATRICE
!
!        D'AUTRE PART ON TRACE DS LE REFA(11) DE LA MATRICE, SON CARAC
!        TERE COMPLET OU INCOMPLET AU SENS PARALLELISME:
!        * 'MPI_COMPLET': TOUS LES PROCS CONNAISSENT LA STRUCTURE DE
!           DONNEES, PAS BESOIN DE LA COMPLETER POUR FAIRE PAR EX. UN
!           PRODUIT MATRICE-VECTEUR. C'EST LE CAS D'UN CALCUL NON
!           DISTRIBUE (DISTRIBUE TYPE 1).
!        * 'MPI_INCOMPLET': CHAQUE PROC NE CONNAIT QUE QUELQUES COMPO
!           SANTES, IL FAUT ALORS PARFOIS COMPLETER PAR UNE COMM
!           MPI AD HOC (CF APPEL A ROUTINE MPICM1). C'EST LE CAS D'UN
!           CALCUL DISTRIBUE DE TYPE 2 (CF LISTE CI-DESSUS).
!
!         DS CES DEUX CAS DE FIGURE, LA SD EST DIMENSIONNEE DE LA MEME
!         FACON SUR TOUS LES PROCS (MEME TAILLE QU'EN SEQ, DES ZEROS COM
!         PLETENT LES TERMES NON CALCULES).
!         DANS LE TROISIEME CAS DE FIGURE (MATR_DISTRIBUEE), ON RETAILLE
!         AU PLUS JUSTE LEUR DIMENSION PAR PROC POUR GAGNER EN MEMOIRE.
!         ON NE PEUT PLUS COMPLETER SI SIMPLEMENT, CE CAS DE FIGURE EST
!         TAGGE PAR REFA(11)='MATR_DISTR'
!
!         EN BREF ON A 3 CAS DE FIGURES DE CALCUL ASTER ET ILS SE DECLI
!         NENT COMME SUIT VIS-A-VIS DES VARIABLES DE ASSMAM:
!        1/ CALCUL STD SEQ  OU CALCUL // CENTRALISE OU CALCUL //
!            DISTRIBUE AVEC LA CMDE ECLATE (ASSE_MATRICE) POUR LAQUELLE
!            ON A COMPLETE AU PREALABLE LES MATR_ELEMS(CALC_MATR_ELEM).
!            LDIST='F',KAMPIC='OUI',REFA(11)='MPI_COMPLET',,
!            IMATD=0
!        2/ CALCUL PARALLELE (AVEC OU SANS MUMPS) DISTRIBUE STD:
!            LDIST='T',KAMPIC='NON',REFA(11)='MPI_INCOMPLET',
!            IMATD=0
!        3/ CAS PARTICULIER DU PRECEDENT: SOLVEUR=MUMPS + OPTION MATR
!          DISTRIBUEE ACTIVEE
!            LDIST='T',KAMPIC='NON',REFA(11)='MATR_DISTR',
!            IMATD NON NUL
!
!         FINALEMENT LES VARIABLES KAMPIC ET LDIST TRADUISENT LES MEMES
!         CHOSES MAIS LEURS PROVENANCES SONT DISTINCTES:
!           - LDIST: EXISTENCE D'UNE SD_PARTITION
!           - KAMPIC: COMPLETUDE DES MATR_ELEMS
!           - LDGREL: LA SD_PARTITION EST DE TYPE 'GROUP_ELEM'
!         CELA PERMET DE CORROBORER LES INFORMATIONS
! ------------------------------------------------------------------
    rang=0
    ldist=.false.
    ldgrel=.false.
    call parti0(nbmat, tlimat, partit)
!
    if (partit .ne. ' ') then
        ldist=.true.
        call asmpi_info(rank=mrank, size=msize)
        rang = to_aster_int(mrank)
        nbproc = to_aster_int(msize)
        call jeveuo(partit//'.PRTI', 'L', vi=prti)
        if (prti(1) .ne. nbproc) then
            vali(1)=prti(1)
            vali(2)=nbproc
            call utmess('F', 'CALCULEL_13', ni=2, vali=vali)
        endif
        call jeveuo(partit//'.PRTK', 'L', vk24=prtk)
        ldgrel=prtk(1).eq.'GROUP_ELEM'
        if (.not.ldgrel) then
            call jeveuo(partit//'.NUPROC.MAILLE', 'L', jnumsd)
        endif
    endif
!
!     -- SI C'EST LA COMMANDE ASSE_MATRICE, LES RESUELEM ONT ETE
!        COMPLETES. ON SE RETROUVE DANS LE CAS LDIST=.FALSE.
    call getres(k8bid, k16bid, nomcmd)
    if (nomcmd(1:12) .eq. 'ASSE_MATRICE') then
        ldist=.false.
        ldgrel=.false.
    endif
!
!
!     -- ALLOCATION DES OBJETS .NUMLOX ET .POSDDX:
!     ----------------------------------------------
!     50 EST SUPPOSE ETRE LE + GD NOMBRE DE NOEUDS D'UNE MAILLE
!        STANDARD (JUSQU'A PRESENT : 27 (HEXA27))
    nbnomx=max(nbnoss,50)
    call wkvect('&&ASSMAM.NUMLO1', 'V V I', 2*nbnomx, jnulo1)
    call wkvect('&&ASSMAM.NUMLO2', 'V V I', 2*nbnomx, jnulo2)
    call wkvect('&&ASSMAM.POSDD1', 'V V I', nbnomx*nmxcmp, jposd1)
    call wkvect('&&ASSMAM.POSDD2', 'V V I', nbnomx*nmxcmp, jposd2)
!
!     -- ALLOCATION D'UN OBJET DE TRAVAIL UTILISE DANS ASRETM :
!        CE VECTEUR EST AGRANDI SI NECESSAIRE DANS ASRETM
    lgtmp2=400
    call wkvect('&&ASSMAM.TMP2', 'V V I', lgtmp2, jtmp2)
!
    if (acreer) then
        call detrsd('MATR_ASSE', matdev)
    else
!
        mat19=matdev
        nu14=nudev
        call jeveuo(mat19//'.REFA', 'L', jrefa)
        ASSERT(zk24(jrefa-1+2)(1:14).eq.nu14)
        call jedetr(mat19//'.LIME')
        call jedetr(mat19//'.REFA')
    endif
!
!
!
!     -- RECOPIE DE LA LISTE DES MATR_ELEM DANS 1 OBJET JEVEUX
    call wkvect(matdev//'.LIME', base1//' V K24 ', nbmat, ilimat)
    do i = 1, nbmat
        zk24(ilimat+i-1)=tlimat(i)
        if (dbg .and. tlimat(i) .ne. ' ') call cheksd(tlimat(i), 'SD_MATR_ELEM', iret)
    end do
!
!
!
!     -- CALCUL D UN REPERTOIRE,TEMPORAIRE, MATDEV.LILI A PARTIR
!     DE LA LISTE DE MATRICES ELEMENTAIRES MATDEV.LIME
    call crelil('F', nbmat, ilimat, matdev//'.LILI', 'V',&
                '&MAILLA', matdev, ibid, ma, ibid,&
                ibid, ilimo, nlili, nbelm)
    call jeveuo(matdev//'.ADLI', 'E', jadli)
    call jeveuo(matdev//'.ADNE', 'E', jadne)
!
!
!
    if (niv .ge. 2) then
        call uttcpu('CPU.ASSMAM', 'INIT ', ' ')
        call uttcpu('CPU.ASSMAM', 'DEBUT', ' ')
    endif
!
!         -- CALCUL DE MAT19 ET NU14 :
!         -------------------------------------
    mat19=matdev
    nu14=nudev
!
    call jeveuo(nu14//'.SMOS.SMHC', 'L', jsmhc)
    call jeveuo(nu14//'.SMOS.SMDI', 'L', jsmdi)
    if (imatd .ne. 0) then
        call jeveuo(nu14//'.NUML.PRNO', 'L', jprn1)
        call jeveuo(jexatr(nu14//'.NUML.PRNO', 'LONCUM'), 'L', jprn2)
    else
        call jeveuo(nu14//'.NUME.PRNO', 'L', jprn1)
        call jeveuo(jexatr(nu14//'.NUME.PRNO', 'LONCUM'), 'L', jprn2)
    endif
!
!
!         -- CREATION ET REMPLISSAGE DE .REFA
!         -------------------------------------
    call wkvect(mat19//'.REFA', base1//' V K24', 20, jrefa)
    zk24(jrefa-1+1)=ma
    zk24(jrefa-1+2)=nu14
    zk24(jrefa-1+8)='ASSE'
    if (lmasym) then
        zk24(jrefa-1+9)='MS'
    else
        zk24(jrefa-1+9)='MR'
    endif
    zk24(jrefa-1+10)='NOEU'
!
!
    call jeveuo(nu14//'.SMOS.SMDE', 'L', vi=smde)
    nequ=smde(1)
    itbloc=smde(2)
    ASSERT(smde(3).eq.1)
    if (lmasym) then
        nblc=1
    else
        nblc=2
    endif
!
!
!
!         -- ALLOCATION (OU NON) DE .VALM :
!         ---------------------------------
    if (acreer) then
        call jecrec(mat19//'.VALM', base1//' V '//tt(2:2), 'NU', 'DISPERSE', 'CONSTANT',&
                    nblc)
!
        call jeecra(mat19//'.VALM', 'LONMAX', itbloc)
        do i = 1, nblc
            call jecroc(jexnum(mat19//'.VALM', i))
        end do
    else
        if (.not.cumul) then
            do i = 1, nblc
                call jerazo(jexnum(mat19//'.VALM', i), itbloc, 1)
            end do
        endif
    endif
!
!         -- MISE EN MEMOIRE DES 1 (OU 2) BLOCS DE .VALM :
    call jeveuo(jexnum(mat19//'.VALM', 1), 'E', jvalm(1))
    call jelira(jexnum(mat19//'.VALM', 1), 'TYPE', cval=typsca)
    ASSERT(tt(2:2).eq.typsca)
    if (.not.lmasym) then
        call jeveuo(jexnum(mat19//'.VALM', 2), 'E', jvalm(2))
    else
        jvalm(2)=0
    endif
!
!
!
!
!
!         3. BOUCLE SUR LES MATR_ELEM
!         =============================
    do imat = 1, nbmat
        c1=licoef(imat)
        matel=zk24(ilimat+imat-1)(1:19)
        call dismoi('NOM_MODELE', matel, 'MATR_ELEM', repk=mo2)
        call dismoi('SUR_OPTION', matel, 'MATR_ELEM', repk=optio)
!
        if (imat .eq. 1) then
            optio2=optio
        else
            if (optio2 .ne. optio) optio2='&&MELANGE'
        endif
!
        if (mo2 .ne. mo) then
            call utmess('F', 'ASSEMBLA_5')
        endif
!
!
!           3.1 TRAITEMENT DES MACRO-ELEMENTS :
!           ----------------------------------
        call assma2(lmasym, tt, nu14, ncmp, matel,&
                    c1, jvalm, jtmp2, lgtmp2)
!
!
!           3.2 TRAITEMENT DES ELEMENTS FINIS CLASSIQUES
!           -------------------------------------------
        call jeexin(matel//'.RELR', iret)
        if (iret .eq. 0) goto 80
!
        call jelira(matel//'.RELR', 'LONUTI', nbresu)
        if (nbresu .gt. 0) call jeveuo(matel//'.RELR', 'L', vk24=relr)
!
!           BOUCLE SUR LES RESU_ELEM
!           ==========================
        do iresu = 1, nbresu
            resu=relr(iresu)(1:19)
            call jeexin(resu//'.DESC', ier)
            if (ier .eq. 0) goto 70
!
!
!                   -- CALCUL DE KAMPIC :
            call dismoi('MPI_COMPLET', resu, 'RESUELEM', repk=kempic)
            if (kempic .eq. 'NON') then
                ASSERT(ldist)
                kampic='NON'
            else
                ASSERT(.not.ldist)
            endif
!
!
!                   -- PARFOIS, CERTAINS RESUELEM SONT == 0.
            if (zerobj(resu//'.RESL')) goto 70
!
!
!                   -- NOM DU LIGREL
            call jeveuo(resu//'.NOLI', 'L', vk24=noli)
            ligre1=noli(1)(1:19)
!
            call dismoi('EXI_VF', ligre1, 'LIGREL', repk=exivf)
            if (exivf .eq. 'OUI') then
                ASSERT(.not.lmasym)
                call jeveuo(ligre1//'.REPE', 'L', jrepe)
                call jeveuo(ligre1//'.NVGE', 'L', vk16=nvge)
                vge=nvge(1)(1:12)
                call jeveuo(vge//'.PTVOIS', 'L', jptvoi)
                call jeveuo(vge//'.ELVOIS', 'L', jelvoi)
            endif
!
!
            call jenonu(jexnom(matdev//'.LILI', ligre1), ilima)
            call jenonu(jexnom(nu14//'.NUME.LILI', ligre1), ilinu)
!
!
            call dismoi('TYPE_SCA', resu, 'RESUELEM', repk=typsca)
            ASSERT(typsca.eq.'R' .or. typsca.eq.'C')
            tt(1:1)=typsca
            call dismoi('TYPE_MATRICE', resu, 'RESUELEM', repk=symel)
            ASSERT(symel(1:1).eq.'S' .or. symel(1:1) .eq.'N')
            lmesym=(symel(1:1).eq.'S')
            if (lmasym) then
                ASSERT(lmesym)
            endif
!
!                   -- BOUCLE SUR LES GRELS DU LIGREL
!                   ==================================
            do igr = 1, zzngel(ilima)
                if (ldgrel .and. mod(igr,nbproc) .ne. rang) goto 60
!
!                       -- IL SE PEUT QUE LE GREL IGR SOIT VIDE :
                call jaexin(jexnum(resu//'.RESL', igr), iexi)
                if (iexi .eq. 0) goto 60
!
                call jeveuo(resu//'.DESC', 'L', jdesc)
                mode=zi(jdesc+igr+1)
                if (mode .gt. 0) then
                    nnoe=nbno(mode)
                    ASSERT(nnoe.le.nbnomx)
                    nbvel=digdel(mode)
!                           -- nombre d'elements du grel igr du ligrel ligre1/ilima
                    nel=zznelg(ilima,igr)
                    call jeveuo(jexnum(resu//'.RESL', igr), 'L', jresl)
                    if (exivf .eq. 'OUI') then
                        call jeveuo(jexnum(resu//'.RSVI', igr), 'L', jrsvi)
                        itypel=zzliel(ilima,igr,nel+1)
                        call jenuno(jexnum('&CATA.TE.NOMTE', itypel), nomte)
                        call teattr('S', 'TYPE_VOISIN', codvoi, ibid, typel=nomte)
                    endif
!
!                           BOUCLE SUR LES ELEMENTS DU GREL
!                           ================================
                    do iel = 1, nel
                        call assma3(lmasym, lmesym, tt, igr, iel,&
                                    c1, rang, jnueq, jnumsd, jresl,&
                                    jrsvi, nbvel, nnoe, ldist, ldgrel,&
                                    ilima, jadli, jadne, jprn1, jprn2,&
                                    jnulo1, jnulo2, jposd1, jposd2, admodl,&
                                    lcmodl, mode, nec, nmxcmp, ncmp,&
                                    jsmhc, jsmdi, iconx1, iconx2, jtmp2,&
                                    lgtmp2, jvalm, ilinu, ellagr, exivf,&
                                    jdesc, jrepe, jptvoi, jelvoi, codvoi)
                    end do
                    call jelibe(jexnum(resu//'.RESL', igr))
                endif
 60             continue
            end do
 70         continue
        end do
 80     continue
    end do
!
!
!         -- MISE A JOUR DE REFA(4)
    call jeveuo(mat19//'.REFA', 'E', jrefa)
    if (acreer) then
        zk24(jrefa-1+4)=optio2
    else
        if (zk24(jrefa-1+4) .ne. optio2) zk24(jrefa-1+4)='&&MELANGE'
    endif
!
!
!
!
    if (niv .ge. 2) then
        call uttcpu('CPU.ASSMAM', 'FIN', ' ')
        call uttcpr('CPU.ASSMAM', 6, temps)
        if (niv .ge. 2) write (ifm, '(A44,D11.4,D11.4)'&
                        ) 'TEMPS CPU/SYS ASSEMBLAGE M                : ',&
                        temps(5), temps(6)
    endif
!
!
    if (.not.lmasym) then
!           -- ON AFFECTE AUX TERMES DIAGONAUX DU BLOC INFERIEUR
!              LES VALEURS DES TERMES DIAGONAUX DU BLOC SUPERIEUR
        do ieq = 1, nequ
            idia=zi(jsmdi+ieq-1)
            zr(jvalm(2)+idia-1)=zr(jvalm(1)+idia-1)
        end do
    endif
!
!         -- IL FAUT COMMUNIQUER ELLAGR ENTRE LES PROCS :
    if (ldist) then
        call asmpi_comm_vect('MPI_MAX', 'I', sci=ellagr)
    endif
!
!
!         -- MISE A L'ECHELLE DES COEF. DE LAGRANGE SI NECESSAIRE :
    if (ellagr .gt. 0) call assma1(mat19, ldist)
!
!
    if (kampic .eq. 'OUI') then
!         -- calcul std ou calcul distribue complete
!            (cmd eclatee asse_matrice)
        ASSERT(.not.ldist)
        zk24(jrefa-1+11)='MPI_COMPLET'
    else
!         -- calcul distribue avec ou sans mumps
        ASSERT(ldist)
        zk24(jrefa-1+11)='MPI_INCOMPLET'
    endif
!        -- DANGEREUX DE CUMULER DEUX TYPES D'INFORMATIONS EN REFA(11)
!           LE CARACTERE MPI_COMPLET/INCOMPLET ET MATR_DISTR
!           TOUT DEPEND PAR LA SUITE DU TYPE DE QUESTION QUE L'ON POSE
!           POUR RECUPERER CETTE INFO
    if (imatd .ne. 0) then
!        -- CALCUL DISTRIBUE AVEC MUMPS + OPTION MATR_DISTRIBUEE='OUI'
        ASSERT(ldist)
        zk24(jrefa-1+11)='MATR_DISTR'
    endif
!
!
!
    call jedetr(matdev//'.ADNE')
    call jedetr(matdev//'.ADLI')
    call jedetr('&&ASSMAM.NUMLO1')
    call jedetr('&&ASSMAM.NUMLO2')
    call jedetr('&&ASSMAM.POSDD1')
    call jedetr('&&ASSMAM.POSDD2')
    call jedetr('&&ASSMAM.TMP2')
    call jedbg2(ibid, idbgav)
    if (dbg) call cheksd(matdev, 'SD_MATR_ASSE', iret)
!
    call asmpi_barrier()
    call uttcpu('CPU.CALC.1', 'FIN', ' ')
    call uttcpu('CPU.ASSE.1', 'FIN', ' ')
    call uttcpu('CPU.ASSE.2', 'FIN', ' ')
    call jedema()
end subroutine
