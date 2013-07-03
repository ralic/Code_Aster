subroutine assmam(base, matas, nbmat, tlimat, licoef,&
                  nu, motcle, itysca)
!
!  ATTENTION : CETTE ROUTINE NE DOIT PAS ETRE APPELLEE DIRECTEMENT :
!              IL FAUT APPELER SON "CHAPEAU" : ASMATR.
!
! aslint: disable=W1501
    implicit none
!
#include "jeveux.h"
!
#include "asterc/cheksd.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/assma1.h"
#include "asterfort/assma2.h"
#include "asterfort/assma3.h"
#include "asterfort/crelil.h"
#include "asterfort/detrsd.h"
#include "asterfort/digdel.h"
#include "asterfort/dismoi.h"
#include "asterfort/fettsd.h"
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
#include "asterfort/mpicm0.h"
#include "asterfort/mpicm1.h"
#include "asterfort/nbec.h"
#include "asterfort/nbno.h"
#include "asterfort/parti0.h"
#include "asterfort/teattr.h"
#include "asterfort/typmat.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mess.h"
#include "asterfort/utimsd.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerobj.h"
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
    character(len=1) :: base1, typsca, kbid
    character(len=2) :: tt
    character(len=8) :: k8bid, nogdco, nogdsi, ma, ma2, mo, mo2, partit
    character(len=8) :: knumer, symel, kempic, kampic, exivf
    character(len=12) :: vge
    character(len=14) :: nudev, nu14
    character(len=16) :: k16bid, nomcmd
    character(len=19) :: matdev, mat19, resu, matel, ligre1, ligre2, sdfeti
    character(len=24) :: method, k24b, nomlog, infofe
    character(len=1) :: matsym
    real(kind=8) :: c1, temps(6), rbid
    complex(kind=8) :: cbid
!
    logical :: acreer, cumul, dbg, iddok, lbid, lfeti, lfetic
    logical :: lgoto, llich, llichd, llichp, llimo, ldist
    logical :: lmasym, lmesym, ldgrel
!
    integer :: admodl, i
    integer :: iad, jdesc
    integer :: jadli, jadne, jnueq, jnulo1, jnulo2
    integer :: jposd1, jposd2, jtmp2, lgtmp2
    integer :: ibid, iconx1, iconx2, idbgav, idd
    integer :: idime, jlres, jprn1, jprn2, jresl, jrsvi
    integer :: iel, ier, ierd, ifcpu, ifel1, ifel2, ifel3, ifel4
    integer :: ifel5, jfetm, jfetn, ifm, igr, igrel
    integer :: ili, jfnusd, ilima, ilimat, ilimo, ilimpi, ilinu
    integer :: imat, jnumsd, jrefn, iresu
    integer :: iret, iret1, iret2, iret3, itbloc
    integer :: jrefa, jsmde, jsmdi, jsmhc, jvalm(2)
    integer :: lcmodl, mode, n1, nbelm
    integer :: nblc, nbnomx, nbnoss, nbresu, nbsd, jnvge
    integer :: ncmp, nbvel, nec, nel, nequ, nbproc, vali(4)
    integer :: niv, nlili, nmxcmp, nnoe, jptvoi, jelvoi, jprti, jprtk
    integer :: nugd, rang, ieq, idia, ellagr, jrepe, itypel, imatd, iexi
!
!-----------------------------------------------------------------------
!     FONCTIONS FORMULES :
!-----------------------------------------------------------------------
    integer :: zzngel, zznelg, zzliel
!
    zzngel(ili)=zi(jadli+3*(ili-1))
    zznelg(ili,igrel)=zi(zi(jadli+3*(ili-1)+2)+igrel)-&
     &                  zi(zi(jadli+3*(ili-1)+2)+igrel-1)-1
    zzliel(ili,igrel,iel)=zi(zi(jadli+3*(ili-1)+1)-1+&
     &                    zi(zi(jadli+3*(ili-1)+2)+igrel-1)+iel-1)
!----------------------------------------------------------------------
!
!
!
    call jemarq()
    dbg=.false.
    call jedbg2(idbgav, 0)
    call infniv(ifm, niv)
    call mpicm1('BARRIER', ' ', ibid, ibid, ibid,&
                rbid, cbid)
    call uttcpu('CPU.CALC.1', 'DEBUT', ' ')
    call uttcpu('CPU.ASSE.1', 'DEBUT', ' ')
    call uttcpu('CPU.ASSE.2', 'DEBUT', ' ')
!
    base1=base
    matdev=matas
    nudev=nu
    if (dbg) call cheksd(nudev, 'SD_NUME_DDL', iret)
!
    call dismoi('F', 'NOM_MODELE', nudev, 'NUME_DDL', ibid,&
                mo, ierd)
    call dismoi('F', 'NOM_MAILLA', mo, 'MODELE', ibid,&
                ma, ierd)
    call dismoi('F', 'NOM_MAILLA', nudev, 'NUME_DDL', ibid,&
                ma2, ierd)
    call assert(ma.eq.ma2)
    call dismoi('F', 'NB_NO_SS_MAX', ma, 'MAILLAGE', nbnoss,&
                k8bid, ierd)
    call dismoi('F', 'NOM_GD', nudev, 'NUME_DDL', ibid,&
                nogdco, ierd)
    call dismoi('F', 'NOM_GD_SI', nogdco, 'GRANDEUR', ibid,&
                nogdsi, ierd)
    call dismoi('F', 'NB_CMP_MAX', nogdsi, 'GRANDEUR', nmxcmp,&
                k8bid, ierd)
    ncmp=nmxcmp
    call dismoi('F', 'NUM_GD_SI', nogdsi, 'GRANDEUR', nugd,&
                k8bid, ierd)
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
!     ELLAGR : 0 : PAS D'ELEMENT DE LAGRANGE
!              1 : IL EXISTE DES ELEMENTS DE LAGRANGE
    ellagr=0
!     KAMPIC : 'OUI' -> LA MATR_ASSE EST 'MPI_COMPLET'
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
    call assert(matsym.eq.'S' .or. matsym.eq.'N')
    lmasym=(matsym.eq.'S')
    if (motcle(1:4) .eq. 'ZERO') then
        cumul=.false.
        acreer=.true.
    else if (motcle(1:4).eq.'CUMU') then
        cumul=.true.
        acreer=.false.
        call jelira(matdev//'.VALM', 'NMAXOC', nblc, kbid)
        call assert(nblc.eq.1 .or. nblc.eq.2)
        if (nblc .eq. 2) lmasym=.false.
    else
        call assert(.false.)
    endif
    call assert((itysca.eq.1) .or. (itysca.eq.2))
    if (itysca .eq. 1) then
        tt='?R'
    else if (itysca.eq.2) then
        tt='?C'
    endif
!
!
! ------------------------------------------------------------------
!     -- SOLVEUR FETI :
!        CALCUL DE :
!           * LFETI  : .TRUE. : ON VA RESOUDRE AVEC FETI
!           * NBSD   : SI FETI : NOMBRE DE SOUS-DOMAINES (SINON:0)
!           * INFOFE : POUR LE MONITORING (SI FETI)
!           * LFETIC : POUR LE MONITORING (SI FETI)
!           * SDFETI : NOM DU PARTITIONNEMENT FETI (SI FETI)
!           * METHOD : METHODE DE RESOLUTION : FETI / AUTRE
! ------------------------------------------------------------------
    call jelira(nudev//'.NUME.REFN', 'LONMAX', n1, kbid)
    call assert(n1.eq.4)
    call jeveuo(nudev//'.NUME.REFN', 'L', jrefn)
    method=zk24(jrefn+2)
    sdfeti=zk24(jrefn+3)(1:19)
    lfeti=.false.
    lfetic=.false.
    nbsd=0
    infofe='FFFFFFFFFFFFFFFFFFFFFFFF'
    if (method(1:4) .eq. 'FETI') then
        lfeti=.true.
        call assert(lmasym)
        call jeveuo(sdfeti//'.FDIM', 'L', idime)
        nbsd=zi(idime)
        call jeveuo(nudev//'.FETN', 'L', jfetn)
        if (.not.acreer) call jeveuo(matdev//'.FETM', 'L', jfetm)
        call jeveuo('&FETI.FINF', 'L', iad)
        infofe=zk24(iad)
        if (infofe(11:11) .eq. 'T') lfetic=.true.
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
        call fettsd(infofe, ibid, ibid, ibid, sdfeti,&
                    k24b, ibid, ibid, ibid, ifm,&
                    lbid, ibid, ibid, ibid, mat19,&
                    2, lbid)
    endif
!
!
! ------------------------------------------------------------------
!
!     -- IL EXISTE TROIS FORMES DE CALCUL DISTRIBUE (DS LE SENS TOUS LES
!        PROCESSEURS NE CONNAISSENT QU'UNE PARTIE DE LA MATRICE) BASES
!        SUR UNE PARTITION:
!        * FETI SEQ ET //
!        * // DISTRIBUE
!        * // DISTRIBUE AVEC MUMPS + OPTION MATR_DISTIBUEE
!
!     -- IL EXISTE TROIS FORMES DE CALCUL CENTRALISE (TOUS LES PROC CON
!        NAISSENT LA MATRICE):
!        * SEQ HORS FETI
!        * // CENTRALISE
!        * // DISTRIBUE DANS ASSE_MATRICE
!
!        D'AUTRE PART ON TRACE DS LE REFA(11) DE LA MATRICE, SON CARAC
!        TERE COMPLET OU INCOMPLET AU SENS PARALLELISME:
!        * 'MPI_COMPLET': TOUS LES PROCS CONNAISSENT LA STRUCTURE DE
!           DONNEES, PAS BESOIN DE LA COMPLETER POUR FAIRE PAR EX. UN
!           PRODUIT MATRICE-VECTEUR. C'EST LE CAS D'UN CALCUL NON
!           DISTRIBUE OU D'UN CALCUL FETI (DISTRIBUE TYPE 1).
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
!         EN BREF ON A 4 CAS DE FIGURES DE CALCUL ASTER ET ILS SE DECLI
!         NENT COMME SUIT VIS-A-VIS DES VARIABLES DE ASSMAM:
!        1/ CALCUL STD SEQ PAS FETI OU CALCUL // CENTRALISE OU CALCUL //
!            DISTRIBUE AVEC LA CMDE ECLATE (ASSE_MATRICE) POUR LAQUELLE
!            ON A COMPLETE AU PREALABLE LES MATR_ELEMS(CALC_MATR_ELEM).
!            LFETI='F',LDIST='F',KAMPIC='OUI',REFA(11)='MPI_COMPLET',,
!            IMATD=0
!        2/ CALCUL FETI SEQ OU PARALLELE (MATRICES MERE ET FILLES)
!            LFETI='T',LDIST='F',KAMPIC='OUI',REFA(11)='MPI_COMPLET'
!            IMATD=0
!        3/ CALCUL PARALLELE (AVEC OU SANS MUMPS) DISTRIBUE STD:
!            LFETI='F',LDIST='T',KAMPIC='NON',REFA(11)='MPI_INCOMPLET',
!            IMATD=0
!        4/ CAS PARTICULIER DU PRECEDENT: SOLVEUR=MUMPS + OPTION MATR
!          DISTRIBUEE ACTIVEE
!            LFETI='F',LDIST='T',KAMPIC='NON',REFA(11)='MATR_DISTR',
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
!     -- FETI N'EST PAS UN CALCUL DISTRIBUE. SES DONNEES SONT COMPLETES
!        POUR CHAQUE PROC
    if ((partit.ne.' ') .and. (.not.lfeti)) then
        ldist=.true.
        call mpicm0(rang, nbproc)
        call jeveuo(partit//'.PRTI', 'L', jprti)
        if (zi(jprti) .ne. nbproc) then
            vali(1)=zi(jprti)
            vali(2)=nbproc
            call u2mesi('F', 'CALCULEL_13', 2, vali)
        endif
        call jeveuo(partit//'.PRTK', 'L', jprtk)
        ldgrel=zk24(jprtk-1+1).eq.'GROUP_ELEM'
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
        do 10 idd = 0, nbsd
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
! IDD=0 --> DOMAINE GLOBAL/ IDD=I --> IEME SOUS-DOMAINE
!
! TRAVAIL PREALABLE POUR DETERMINER SI ON EFFECTUE LA BOUCLE SUIVANT
! LE SOLVEUR (FETI OU NON), LE TYPE DE RESOLUTION (PARALLELE OU
! SEQUENTIELLE) ET L'ADEQUATION "RANG DU PROCESSEUR-NUMERO DU SD"
            if (.not.lfeti) then
                iddok=.true.
            else
                if (zi(ilimpi+idd) .eq. 1) then
                    iddok=.true.
                else
                    iddok=.false.
                endif
            endif
!
            if (iddok) then
                if (idd .eq. 0) then
!             -- MATR_ASSE MAITRE LIEE AU DOMAINE GLOBAL
                    mat19=matdev
                    nu14=nudev
                else
!             -- MATR_ASSE ESCLAVE LIEE AU SOUS-DOMAINE IDD
                    mat19=zk24(jfetm+idd-1)(1:19)
                    nu14=zk24(jfetn+idd-1)(1:14)
                endif
                call jeveuo(mat19//'.REFA', 'L', jrefa)
                call assert(zk24(jrefa-1+2)(1:14).eq.nu14)
                call jedetr(mat19//'.LIME')
                call jedetr(mat19//'.REFA')
            endif
10      continue
    endif
!
!
!
!     -- RECOPIE DE LA LISTE DES MATR_ELEM DANS 1 OBJET JEVEUX
    call wkvect(matdev//'.LIME', base1//' V K24 ', nbmat, ilimat)
    do 20 i = 1, nbmat
        zk24(ilimat+i-1)=tlimat(i)
        if (dbg .and. tlimat(i) .ne. ' ') call cheksd(tlimat(i), 'SD_MATR_ELEM', iret)
20  end do
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
    if (lfeti) then
!       STOCKE &&//NOMPRO(1:6)//'_M.' POUR COHERENCE AVEC L'EXISTANT
        nomlog='&FETI.MAILLE.NUMSD'
        call jeveuo(nomlog, 'L', jfnusd)
!       CONSTITUTION DE L'OBJET JEVEUX MATDEV.FETM COMPLEMENTAIRE
        if (acreer) call wkvect(matdev//'.FETM', base1//' V K24', nbsd, jfetm)
        call jeveuo('&FETI.INFO.CPU.ASSE', 'E', ifcpu)
    endif
!
!
!
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
! IDD=0 --> DOMAINE GLOBAL/ IDD=I --> IEME SOUS-DOMAINE
    do 130 idd = 0, nbsd
!
! TRAVAIL PREALABLE POUR DETERMINER SI ON EFFECTUE LA BOUCLE SUIVANT
! LE SOLVEUR (FETI OU NON), LE TYPE DE RESOLUTION (PARALLELE OU
! SEQUENTIELLE) ET L'ADEQUATION "RANG DU PROCESSEUR-NUMERO DU SD"
        if (.not.lfeti) then
            iddok=.true.
        else
            if (zi(ilimpi+idd) .eq. 1) then
                iddok=.true.
            else
                iddok=.false.
            endif
        endif
!
        if (iddok) then
            if (lfeti) call jemarq()
            if ((niv.ge.2) .or. lfetic) then
                call uttcpu('CPU.ASSMAM', 'INIT ', ' ')
                call uttcpu('CPU.ASSMAM', 'DEBUT', ' ')
            endif
!
!         -- CALCUL DE MAT19 ET NU14 :
!         -------------------------------------
            if (idd .eq. 0) then
                mat19=matdev
                nu14=nudev
            else
                mat19=zk24(jfetm+idd-1)(1:19)
                nu14=zk24(jfetn+idd-1)(1:14)
            endif
!
            if ((.not.lfeti) .or. (idd.gt.0)) then
                call jeveuo(nu14//'.SMOS.SMHC', 'L', jsmhc)
                call jeveuo(nu14//'.SMOS.SMDI', 'L', jsmdi)
                if (imatd .ne. 0) then
                    call jeveuo(nu14//'.NUML.PRNO', 'L', jprn1)
                    call jeveuo(jexatr(nu14//'.NUML.PRNO', 'LONCUM'), 'L', jprn2)
                else
                    call jeveuo(nu14//'.NUME.PRNO', 'L', jprn1)
                    call jeveuo(jexatr(nu14//'.NUME.PRNO', 'LONCUM'), 'L', jprn2)
                endif
            endif
!
!
            if (idd .gt. 0) then
                if (acreer) then
!             -- SI SOUS-DOMAINE FETI NOUVEAU
                    call gcncon('.', knumer)
                    knumer(1:1)='F'
                    zk24(jfetm+idd-1)=matdev(1:11)//knumer
                    mat19=zk24(jfetm+idd-1)(1:19)
                endif
                call jeveuo(nu14//'.NUME.REFN', 'L', jrefn)
                method=zk24(jrefn+2)
                sdfeti=zk24(jrefn+3)(1:19)
            endif
!
!         -- CREATION ET REMPLISSAGE DE .REFA
!         -------------------------------------
            call wkvect(mat19//'.REFA', base1//' V K24', 11, jrefa)
            zk24(jrefa-1+1)=ma
            zk24(jrefa-1+2)=nu14
            zk24(jrefa-1+8)='ASSE'
            if (lmasym) then
                zk24(jrefa-1+9)='MS'
            else
                zk24(jrefa-1+9)='MR'
            endif
            zk24(jrefa-1+10)='NOEU'
            if (method(1:4) .eq. 'FETI') then
                zk24(jrefa-1+5)='FETI'
                zk24(jrefa-1+6)=sdfeti
            endif
!
!         -- SI FETI ET DOMAINE GLOBAL ON N'A RIEN A ASSEMBLER :
            if (lfeti .and. (idd.eq.0)) goto 110
!
!
!
            call jeveuo(nu14//'.SMOS.SMDE', 'L', jsmde)
            nequ=zi(jsmde-1+1)
            itbloc=zi(jsmde-1+2)
            call assert(zi(jsmde-1+3).eq.1)
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
                call jeecra(mat19//'.VALM', 'LONMAX', itbloc, ' ')
                do 30 i = 1, nblc
                    call jecroc(jexnum(mat19//'.VALM', i))
30              continue
            else
                if (.not.cumul) then
                    do 40 i = 1, nblc
                        call jerazo(jexnum(mat19//'.VALM', i), itbloc, 1)
40                  continue
                endif
            endif
!
!         -- MISE EN MEMOIRE DES 1 (OU 2) BLOCS DE .VALM :
            call jeveuo(jexnum(mat19//'.VALM', 1), 'E', jvalm(1))
            call jelira(jexnum(mat19//'.VALM', 1), 'TYPE', ibid, typsca)
            call assert(tt(2:2).eq.typsca)
            if (.not.lmasym) then
                call jeveuo(jexnum(mat19//'.VALM', 2), 'E', jvalm(2))
            else
                jvalm(2)=0
            endif
!
!
!
            lgoto=.false.
            k24b(1:14)=nudev
            call fettsd(infofe, idd, ibid, ibid, sdfeti,&
                        k24b, jfetn, jvalm(1), ibid, ifm,&
                        lbid, ibid, ibid, ibid, mat19,&
                        3, lgoto)
            if (lgoto) goto 100
!
!
!
!         3. BOUCLE SUR LES MATR_ELEM
!         =============================
            do 90 imat = 1, nbmat
                c1=licoef(imat)
                matel=zk24(ilimat+imat-1)(1:19)
                call dismoi('F', 'NOM_MODELE', matel, 'MATR_ELEM', ibid,&
                            mo2, ierd)
                call dismoi('F', 'SUR_OPTION', matel, 'MATR_ELEM', ibid,&
                            optio, ierd)
!
                if (imat .eq. 1) then
                    optio2=optio
                else
                    if (optio2 .ne. optio) optio2='&&MELANGE'
                endif
!
                if (mo2 .ne. mo) call u2mess('F', 'ASSEMBLA_5')
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
                call jelira(matel//'.RELR', 'LONUTI ', nbresu, kbid)
                if (nbresu .gt. 0) call jeveuo(matel//'.RELR', 'L', jlres)
!
!           BOUCLE SUR LES RESU_ELEM
!           ==========================
                do 70 iresu = 1, nbresu
                    resu=zk24(jlres+iresu-1)(1:19)
                    call jeexin(resu//'.DESC', ier)
                    if (ier .eq. 0) goto 70
!
!
!             -- CALCUL DE KAMPIC :
                    call dismoi('F', 'MPI_COMPLET', resu, 'RESUELEM', ibid,&
                                kempic, ierd)
                    if (kempic .eq. 'NON') then
                        call assert(ldist.or.lfeti)
!             -- POUR FETI IL EST NORMAL QUE CHAQUE PROC NE CONNAISSE
!                QUE SA PARTIE DE MATRICE: LA MATRICE LOCALE AU SD IDD
                        if (.not.lfeti) kampic='NON'
                    else
                        call assert(.not.ldist)
                    endif
!
!
!             -- PARFOIS, CERTAINS RESUELEM SONT == 0.
                    if (zerobj(resu//'.RESL')) goto 70
!
!
!             -- NOM DU LIGREL
                    call jeveuo(resu//'.NOLI', 'L', iad)
                    ligre1=zk24(iad)(1:19)
!
                    call dismoi('F', 'EXI_VF', ligre1, 'LIGREL', ibid,&
                                exivf, ierd)
                    if (exivf .eq. 'OUI') then
                        call assert(.not.lmasym)
                        call jeveuo(ligre1//'.REPE', 'L', jrepe)
                        call jeveuo(ligre1//'.NVGE', 'L', jnvge)
                        vge=zk16(jnvge-1+1)(1:12)
                        call jeveuo(vge//'.PTVOIS', 'L', jptvoi)
                        call jeveuo(vge//'.ELVOIS', 'L', jelvoi)
                    endif
!
!
!             SI FETI & LIGREL TARDIF:
!             -------------------------
                    llimo=.true.
                    llich=.false.
                    llichd=.false.
                    llichp=.false.
                    if (lfeti .and. (idd.ne.0)) then
!               RECHERCHE D'OBJET TEMPORAIRE SI FETI
                        nomlog=ligre1//'.FEL1'
                        call jeexin(nomlog, iret1)
                        if (iret1 .ne. 0) then
!                 LIGREL DE CHARGE A MAILLES TARDIVES OU CONTACT
!                 CONTINUE 1ERE PASSE
                            call jeveuo(nomlog, 'L', ifel1)
                            llich=.true.
                            llimo=.false.
                            ligre2=zk24(ifel1-1+idd)(1:19)
                            if (ligre2 .eq. ' ') then
!                   LIGREL NE CONCERNANT PAS LE SOUS-DOMAINE IDD
                                goto 70
!
                            else
                                call jeexin(ligre1//'.FEL2', iret2)
                                if (iret2 .ne. 0) then
!                     LIGREL DE CHARGE A MAILLES TARDIVES DUPLIQUEES
!                     DE FILS LIGRE2 DDL_IMPO, FORCE_NODALE...
                                    llichd=.true.
!                     VRAI NOM DU LIGREL DUPLIQUE CONTENU DANS
!                     PROF_CHNO.LILI LOCAL
                                    call jeveuo(ligre1//'.FEL2', 'L', ifel2)
                                    call jeexin(ligre1//'.FEL3', iret3)
                                    if (iret3 .ne. 0) then
                                        call jeveuo(ligre1//'.FEL3', 'L', ifel3)
!                       LIGREL DE CHARGE A NOEUDS TARDIFS DUPLIQUES
!                       (DDL_IMPO...)
                                        llichp=.true.
                                    else
!                       PAS DE NOEUD TARDIF DUPLIQUE (FORCE_NODALE)
                                        llichp=.false.
                                    endif
                                    call jeexin(ligre1//'.FEL4', iret3)
                                    if (iret3 .ne. 0) call jeveuo(ligre1//'.FEL4', 'L', ifel4)
                                    call jeexin(ligre1//'.FEL5', iret3)
                                    if (iret3 .ne. 0) call jeveuo(ligre1//'.FEL5', 'L', ifel5)
                                else
!                     -- LIGREL DE CHARGE NON DUPLIQUE
                                    llichd=.false.
                                endif
                            endif
                        endif
                    endif
!
!
                    call jenonu(jexnom(matdev//'.LILI', ligre1), ilima)
                    if (llichd) then
                        call jenonu(jexnom(nu14//'.NUME.LILI', ligre2), ilinu)
                    else
                        call jenonu(jexnom(nu14//'.NUME.LILI', ligre1), ilinu)
                    endif
!
!
!               -- MONITORING:
                    if ((infofe(5:5).eq.'T') .and. lfeti) then
                        write (ifm,*)'**************** IDD ',idd
                        write (ifm,*)'<FETI/ASSMAM> ILIMO',ilimo,&
                        'ILIMA',ilima
                        write (ifm,*)'<FETI/ASSMAM> LIGRE1/2 ',ligre1,&
                        ligre2
                    endif
                    call dismoi('F', 'TYPE_SCA', resu, 'RESUELEM', ibid,&
                                typsca, ierd)
                    call assert(typsca.eq.'R' .or. typsca.eq.'C')
                    tt(1:1)=typsca
                    call dismoi('F', 'TYPE_MATRICE', resu, 'RESUELEM', ibid,&
                                symel, ierd)
                    call assert(symel(1:1).eq.'S' .or. symel(1:1) .eq.'N')
                    lmesym=(symel(1:1).eq.'S')
                    if (lmasym) call assert(lmesym)
!
!             BOUCLE SUR LES GRELS DU LIGREL
!             ==============================
                    do 60 igr = 1, zzngel(ilima)
                        if (ldgrel .and. mod(igr,nbproc) .ne. rang) goto 60
!
!               -- IL SE PEUT QUE LE GREL IGR SOIT VIDE :
                        call jaexin(jexnum(resu//'.RESL', igr), iexi)
                        if (iexi .eq. 0) goto 60
!
                        call jeveuo(resu//'.DESC', 'L', jdesc)
                        mode=zi(jdesc+igr+1)
                        if (mode .gt. 0) then
                            nnoe=nbno(mode)
                            call assert(nnoe.le.nbnomx)
                            nbvel=digdel(mode)
!                 NOMBRE D'ELEMENTS DU GREL IGR DU LIGREL LIGRE1/ILIMA
                            nel=zznelg(ilima,igr)
                            call jeveuo(jexnum(resu//'.RESL', igr), 'L', jresl)
                            if (exivf .eq. 'OUI') then
                                call jeveuo(jexnum(resu//'.RSVI', igr), 'L', jrsvi)
                                itypel=zzliel(ilima,igr,nel+1)
                                call jenuno(jexnum('&CATA.TE.NOMTE', itypel), nomte)
                                call teattr(nomte, 'S', 'TYPE_VOISIN', codvoi, ibid)
                            endif
!
!                 BOUCLE SUR LES ELEMENTS DU GREL
!                 ================================
                            do 50 iel = 1, nel
                                call assma3(lmasym, lmesym, tt, igr, iel,&
                                            c1, rang, ifel2, ifel3, ifel4,&
                                            ifel5, ifm, jfnusd, jnueq, jnumsd,&
                                            jresl, jrsvi, nbvel, nnoe, lfeti,&
                                            llich, llichd, llichp, llimo, ldist,&
                                            ldgrel, ilima, jadli, jadne, jprn1,&
                                            jprn2, jnulo1, jnulo2, jposd1, jposd2,&
                                            admodl, lcmodl, mode, nec, nmxcmp,&
                                            ncmp, jsmhc, jsmdi, iconx1, iconx2,&
                                            ligre1, ligre2, infofe, jtmp2, lgtmp2,&
                                            jvalm, ilinu, idd, ellagr, exivf,&
                                            jdesc, jrepe, jptvoi, jelvoi, codvoi)
50                          continue
                            call jelibe(jexnum(resu//'.RESL', igr))
                        endif
60                  continue
70              continue
80              continue
90          continue
100          continue
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
110          continue
!
!
!         -- MONITORING:
            if (lfeti .and. (infofe(1:1).eq.'T')) then
                if (idd .eq. 0) then
                    write (ifm,*)'<FETI/ASSMAM> DOMAINE GLOBAL',mat19
                else
                    write (ifm,*)'<FETI/ASSMAM> SD: ',idd,' ',mat19
                endif
            endif
            if ((infofe(3:3).eq.'T') .and. (idd.ne.0)) call utimsd(ifm, 2, .false., .true.,&
                                                                   matdev, 1, ' ')
            if ((infofe(3:3).eq.'T') .and. (idd.eq.nbsd)) call utimsd(ifm, 2, .false., .true.,&
                                                                      matdev, 1, ' ')
            if ((niv.ge.2) .or. lfetic) then
                call uttcpu('CPU.ASSMAM', 'FIN', ' ')
                call uttcpr('CPU.ASSMAM', 6, temps)
                if (niv .ge. 2) write (ifm, '(A44,D11.4,D11.4)'&
                                ) 'TEMPS CPU/SYS ASSEMBLAGE M                : ',&
                                temps(5), temps(6)
                if (lfetic) zr(ifcpu+idd)=zr(ifcpu+idd)+temps(5)+temps( 6)
            endif
!
!
!         -- ECRITURE DANS FICHIER SI FETI ET INFO_FETI(14:14)='T'
            k24b(1:14)=nudev
            call fettsd(infofe, idd, ibid, ibid, sdfeti,&
                        k24b, jfetn, jvalm(1), ibid, ifm,&
                        lbid, ibid, ibid, ibid, mat19,&
                        6, lbid)
!
!
            if (.not.lmasym) then
!           -- ON AFFECTE AUX TERMES DIAGONAUX DU BLOC INFERIEUR
!              LES VALEURS DES TERMES DIAGONAUX DU BLOC SUPERIEUR
                do 120 ieq = 1, nequ
                    idia=zi(jsmdi+ieq-1)
                    zr(jvalm(2)+idia-1)=zr(jvalm(1)+idia-1)
120              continue
            endif
!
!         -- IL FAUT COMMUNIQUER ELLAGR ENTRE LES PROCS :
            if (ldist) call mpicm1('MPI_MAX', 'I', 1, ibid, ellagr,&
                                   rbid, cbid)
!
!
!         -- MISE A L'ECHELLE DES COEF. DE LAGRANGE SI NECESSAIRE :
            if (lfeti) then
                if ((ellagr.gt.0) .and. (idd.gt.0)) call assma1(mat19, ldist)
                call jedema()
            else
                if (ellagr .gt. 0) call assma1(mat19, ldist)
            endif
        endif
!
!
        if (kampic .eq. 'OUI') then
!         -- CALCUL STD OU CALCUL FETI OU CALCUL DISTRIBUE COMPLETE
!            (CMD ECLATEE ASSE_MATRICE)
            call assert(.not.ldist)
            zk24(jrefa-1+11)='MPI_COMPLET'
        else
!         -- CALCUL DISTRIBUE AVEC OU SANS MUMPS
            call assert(ldist)
            zk24(jrefa-1+11)='MPI_INCOMPLET'
        endif
!        -- DANGEREUX DE CUMULER DEUX TYPES D'INFORMATIONS EN REFA(11)
!           LE CARACTERE MPI_COMPLET/INCOMPLET ET MATR_DISTR
!           TOUT DEPEND PAR LA SUITE DU TYPE DE QUESTION QUE L'ON POSE
!           POUR RECUPERER CETTE INFO
        if (imatd .ne. 0) then
!        -- CALCUL DISTRIBUE AVEC MUMPS + OPTION MATR_DISTRIBUEE='OUI'
            call assert(ldist)
            zk24(jrefa-1+11)='MATR_DISTR'
        endif
130  end do
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
    if (dbg .and. (.not.lfeti)) call cheksd(matdev, 'SD_MATR_ASSE', iret)
!
!     CALL UTIMSD(6,-1,.FALSE.,.TRUE.,MATAS,1,' ')
    call mpicm1('BARRIER', ' ', ibid, ibid, ibid,&
                rbid, cbid)
    call uttcpu('CPU.CALC.1', 'FIN', ' ')
    call uttcpu('CPU.ASSE.1', 'FIN', ' ')
    call uttcpu('CPU.ASSE.2', 'FIN', ' ')
    call jedema()
end subroutine
