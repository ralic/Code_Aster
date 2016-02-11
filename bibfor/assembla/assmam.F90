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
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! Assemblage Morse avec preconditionnement des matr_elem de mailles
! "Lagrange".
!-----------------------------------------------------------------------
! int k* base   : base sur laquelle on veut creer la matr_asse
! out k* matas  :l'objet matas de type matr_asse est cree et rempli
! in  k* matas  : nom de l'objet de type matr_asse a creer
! in  i  nbmat  : nombre de matr_elem  de la liste tlimat
! in  k* tlimat : liste des matr_elem
! in  i  licoef : liste des coefficients multiplicateurs des matr_elem
! in  k* nu     : nom du numero_ddl
! in  k4 motcle : 'ZERO' ou 'cumu'
!                 'ZERO':si un objet de nom matas et de type
!                        matr_asse existe on l'ecrase
!                 'CUMU':si un objet de nom matas et de type
!                        matr_asse existe on l'enrichi
! in  i   itysca  : type (r/c) de la matr_asse
!                          1 --> reelles
!                          2 --> complexes
!-----------------------------------------------------------------------
    character(len=16) :: optio, optio2, codvoi
    character(len=1) :: base1, typsca
    character(len=2) :: tt
    character(len=8) ::  nogdco, nogdsi, ma, ma2, mo, mo2, partit
    character(len=8) :: symel, kempic
    character(len=14) :: nudev, nu14
    character(len=19) :: matdev, mat19, resu, matel, ligre1
    character(len=1) :: matsym
    character(len=3) :: matd
    real(kind=8) :: c1, temps(6)

    aster_logical :: acreer, cumul, dbg, ldistme, lmatd
    aster_logical :: lmasym, lmesym, ldgrel

    integer :: admodl, i
    integer :: jdesc
    integer :: jadli, jadne, jnueq, jnulo1, jnulo2
    integer :: jposd1, jposd2, jtmp2, lgtmp2
    integer :: ibid, iconx1, iconx2, idbgav
    integer :: jprn1, jprn2, jresl
    integer :: iel, ier, ifm, igr
    integer :: ilima, ilimat, ilimo, ilinu
    integer :: imat, jnumsd, iresu
    integer :: iret, itbloc
    integer :: jrefa, jsmdi, jsmhc, jvalm(2)
    integer :: lcmodl, mode, n1, nbelm
    integer :: nblc, nbnomx, nbnoss, nbresu
    integer :: ncmp, nbvel, nec, nel, nequ, nbproc, vali(4)
    integer :: niv, nlili, nmxcmp, nnoe, jptvoi, jelvoi
    integer :: nugd, rang, ieq, idia, ellagr, jrepe, iexi
    character(len=24), pointer :: prtk(:) => null()
    integer, pointer :: smde(:) => null()
    character(len=24), pointer :: noli(:) => null()
    integer, pointer :: prti(:) => null()
    character(len=24), pointer :: relr(:) => null()

!-----------------------------------------------------------------------
!     FONCTIONS FORMULES :
!-----------------------------------------------------------------------
    mpi_int :: mrank, msize

#define zzngel(ili) zi(jadli+3*(ili-1))
#define zznelg(ili,igrel) zi(zi(jadli+3*(ili-1)+2)+igrel)- \
    zi(zi(jadli+3*(ili-1)+2)+igrel-1)-1
#define zzliel(ili,igrel,iel) zi(zi(jadli+3*(ili-1)+1)-1+ \
    zi(zi(jadli+3*(ili-1)+2)+igrel-1)+iel-1)

!----------------------------------------------------------------------
! Gestion du parallisme :
! -----------------------
! La routine assemble des matr_elem pour en faire une matr_asse.
! Si les matr_elem sont "distribues" (c'est a dire que chaque processeur
! ne connait qu'une partie des matrices elementaires), on va produire
! une matr_asse "distribuee" (de contenu different sur chaque processeur).
!
! Une matr_asse "distribuee" peut etre de petite taille (MATR_DISTRIBUEE='OUI')
! ou non (MATR_DISTRIBUEE='NON').
!
! 2 booleens pilotent le parallelisme de cette routine :
!  ldistme : il existe au moins un matr_elem distribue
!            => la matr_asse produite sera "distribuee"
!  lmatd : l'utilisateur a demande MATR_DISTRIBUEE='OUI'
!
! On verifie que :
!   lmatd=.T.    => ldistme=.T.
!
!
! Precisions :
! ------------
!  Le booleen lmatd sert essentiellement a determiner le stockage de la matr_asse
!  qu'il faut utiliser : un stockage global ou un stockage local.
!
!  La decision de produire une matr_asse distribuee est prise des que l'on
!  trouve un (ou plusieurs) resuelem distribue(s) dans les matr_elem a assembler.
!
!  Quand une matr_asse est distribuee, la matrice "totale" peut etre obtenue
!  en faisant (au moins par la pensee) une "simple" somme des matr_asse possedees
!  par les differents processeurs. Il est donc fondamental qu'une matrice elementaire
!  (ou la matrice d'un macro-element) ne soit assemblee que sur un seul processeur.
!
!  Si un resuelem est distribue, on peut recuperer la partition attachee a ce
!  resuelem. C'est cette parttion qui servira pour l'assemblage : chaque processeur
!  n'assemble que "ses" elements dans la partition.
!  Si plusieurs resuelem sont distribues, on verifie que leurs partitions sont
!  identiques. Sinon : erreur <F>.
!  Le nombre de processeurs lors de l'assemblage doit etre identique a celui de la
!  partition.
!
!  Les matrices liees aux macro-elements sont attachees aux matr_elem.
!  Elles sont toujours calculees (et identiques) sur TOUS les processeurs
!  (operateur MACR_ELEM_STAT).
!  Si la matr_asse est distribuee, c'est le processeur 0 (et lui seul) qui va assembler
!  les matrices des macro-elements.
!
!----------------------------------------------------------------------

    call jemarq()
    dbg=.false.
    call jedbg2(idbgav, 0)
    call infniv(ifm, niv)
    call asmpi_barrier()
    call uttcpu('CPU.CALC.1', 'DEBUT', ' ')
    call uttcpu('CPU.ASSE.1', 'DEBUT', ' ')
    call uttcpu('CPU.ASSE.2', 'DEBUT', ' ')

    base1=base
    matdev=matas
    nudev=nu
    if (dbg) call cheksd(nudev, 'SD_NUME_DDL', iret)

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
!   ellagr : 0 : il n'existe pas d'element de lagrange
!            1 : il existe des elements de lagrange
    ellagr=0


!   -- calcul de lmatd et jnueq :
!   -----------------------------
    call dismoi('MATR_DISTRIBUEE', nudev, 'NUME_DDL', repk=matd)
    lmatd = (matd.eq.'OUI')
    if (lmatd) then
        call jeveuo(nudev//'.NUML.NUEQ', 'L', jnueq)
    else
        call jeveuo(nudev//'.NUME.NUEQ', 'L', jnueq)
    endif


!   -- calcul de :
!   --------------
!     lmasym: .true   : matrice assemblee symetrique
!             .false. : matrice assemblee non-symetrique
!     acreer: .true.  : il faut creer la matr_asse
!             .false. : la matr_asse existe deja
!     cumul : .true.  : on accumule dans la matr_asse
!             .false. : on remet la matr_asse a zero
!                       (elle doit exister)
!     tt  : tt(1) : type (r/c) de ce que l'on assemble
!           tt(2) : type (r/c) de la sd_matr_asse
!------------------------------------------------------------
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

    call jelira(nudev//'.NUME.REFN', 'LONMAX', n1)
    ASSERT(n1.eq.4)


!   -- calcul de ldistme, partit, ldgrel, jnumsd :
!   -----------------------------------------------
    rang=0
    ldistme=.false.
    ldgrel=.false.
    call parti0(nbmat, tlimat, partit)

    if (partit .ne. ' ') then
        ldistme=.true.
        call asmpi_info(rank=mrank, size=msize)
        rang = to_aster_int(mrank)
        nbproc = to_aster_int(msize)
        call jeveuo(partit//'.PRTK', 'L', vk24=prtk)
        ldgrel=prtk(1).eq.'GROUP_ELEM'
        if (.not.ldgrel) then
            call jeveuo(partit//'.PRTI', 'L', vi=prti)
            if (prti(1) .gt. nbproc) then
                vali(1)=prti(1)
                vali(2)=nbproc
                call utmess('F', 'CALCUL_35', ni=2, vali=vali)
            endif
            call jeveuo(partit//'.NUPROC.MAILLE', 'L', jnumsd)
        endif
    endif


    if (lmatd) then
        ASSERT(ldistme)
    endif



!   -- allocation des objets .NUMLOX et .POSDDX:
!   ----------------------------------------------
!   50 est suppose etre le + gd nombre de noeuds d'une maille
!      standard (jusqu'a present : 27 (hexa27))
    nbnomx=max(nbnoss,50)
    call wkvect('&&ASSMAM.NUMLO1', 'V V I', 2*nbnomx, jnulo1)
    call wkvect('&&ASSMAM.NUMLO2', 'V V I', 2*nbnomx, jnulo2)
    call wkvect('&&ASSMAM.POSDD1', 'V V I', nbnomx*nmxcmp, jposd1)
    call wkvect('&&ASSMAM.POSDD2', 'V V I', nbnomx*nmxcmp, jposd2)

!   -- allocation d'un objet de travail utilise dans asretm :
!      ce vecteur est agrandi si necessaire dans asretm
    lgtmp2=400
    call wkvect('&&ASSMAM.TMP2', 'V V I', lgtmp2, jtmp2)

    if (acreer) then
        call detrsd('MATR_ASSE', matdev)
    else
        mat19=matdev
        nu14=nudev
        call jeveuo(mat19//'.REFA', 'L', jrefa)
        ASSERT(zk24(jrefa-1+2)(1:14).eq.nu14)
        call jedetr(mat19//'.LIME')
        call jedetr(mat19//'.REFA')
    endif


!   -- recopie de la liste des matr_elem dans 1 objet jeveux
    call wkvect(matdev//'.LIME', base1//' V K24 ', nbmat, ilimat)
    do i = 1, nbmat
        zk24(ilimat+i-1)=tlimat(i)
        if (dbg .and. tlimat(i) .ne. ' ') call cheksd(tlimat(i), 'SD_MATR_ELEM', iret)
    end do


!  -- calcul d un repertoire,temporaire, matdev.lili a partir
!     de la liste de matrices elementaires matdev.lime
    call crelil('F', nbmat, ilimat, matdev//'.LILI', 'V',&
                '&MAILLA', matdev, ibid, ma, ibid,&
                ibid, ilimo, nlili, nbelm)
    call jeveuo(matdev//'.ADLI', 'E', jadli)
    call jeveuo(matdev//'.ADNE', 'E', jadne)


    if (niv .ge. 2) then
        call uttcpu('CPU.ASSMAM', 'INIT ', ' ')
        call uttcpu('CPU.ASSMAM', 'DEBUT', ' ')
    endif


!   -- calcul de mat19, nu14, jsmhc, jsmdi, ... :
!   ----------------------------------------------
    mat19=matdev
    nu14=nudev

    call jeveuo(nu14//'.SMOS.SMHC', 'L', jsmhc)
    call jeveuo(nu14//'.SMOS.SMDI', 'L', jsmdi)
    if (lmatd) then
        call jeveuo(nu14//'.NUML.PRNO', 'L', jprn1)
        call jeveuo(jexatr(nu14//'.NUML.PRNO', 'LONCUM'), 'L', jprn2)
    else
        call jeveuo(nu14//'.NUME.PRNO', 'L', jprn1)
        call jeveuo(jexatr(nu14//'.NUME.PRNO', 'LONCUM'), 'L', jprn2)
    endif


!   -- creation et remplissage de .REFA
!   -------------------------------------
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

    call jeveuo(nu14//'.SMOS.SMDE', 'L', vi=smde)
    nequ=smde(1)
    itbloc=smde(2)
    ASSERT(smde(3).eq.1)
    if (lmasym) then
        nblc=1
    else
        nblc=2
    endif


!   -- allocation (ou non) de .VALM :
!   ---------------------------------
    if (acreer) then
        call jecrec(mat19//'.VALM', base1//' V '//tt(2:2), 'NU', 'DISPERSE', 'CONSTANT',&
                    nblc)
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


!   -- mise en memoire des 1 (ou 2) blocs de .VALM :
!   ------------------------------------------------
    call jeveuo(jexnum(mat19//'.VALM', 1), 'E', jvalm(1))
    call jelira(jexnum(mat19//'.VALM', 1), 'TYPE', cval=typsca)
    ASSERT(tt(2:2).eq.typsca)
    if (.not.lmasym) then
        call jeveuo(jexnum(mat19//'.VALM', 2), 'E', jvalm(2))
    else
        jvalm(2)=0
    endif


!   3. boucle sur les matr_elem
!   =============================
    do imat = 1, nbmat
        c1=licoef(imat)
        matel=zk24(ilimat+imat-1)(1:19)
        call dismoi('NOM_MODELE', matel, 'MATR_ELEM', repk=mo2)
        call dismoi('SUR_OPTION', matel, 'MATR_ELEM', repk=optio)

        if (imat .eq. 1) then
            optio2=optio
        else
            if (optio2 .ne. optio) optio2='&&MELANGE'
        endif

        if (mo2 .ne. mo) then
            call utmess('F', 'ASSEMBLA_5')
        endif


!       3.1 traitement des macro-elements :
!       ----------------------------------
        call assma2(ldistme, lmasym, tt, nu14, ncmp, matel,&
                    c1, jvalm, jtmp2, lgtmp2)


!       3.2 traitement des elements finis classiques
!       -------------------------------------------
        call jeexin(matel//'.RELR', iret)
        if (iret .eq. 0) goto 80

        call jelira(matel//'.RELR', 'LONUTI', nbresu)
        if (nbresu .gt. 0) call jeveuo(matel//'.RELR', 'L', vk24=relr)

!       -- boucle sur les resu_elem
!       ============================
        do iresu = 1, nbresu
            resu=relr(iresu)(1:19)
            call jeexin(resu//'.DESC', ier)
            if (ier .eq. 0) goto 70

            call dismoi('MPI_COMPLET', resu, 'RESUELEM', repk=kempic)
            if (kempic .eq. 'NON') then
                 ASSERT(ldistme)
            endif

!           -- parfois, certains resuelem sont == 0.
            if (zerobj(resu//'.RESL')) goto 70

!           -- nom du ligrel
            call jeveuo(resu//'.NOLI', 'L', vk24=noli)
            ligre1=noli(1)(1:19)

            call jenonu(jexnom(matdev//'.LILI', ligre1), ilima)
            call jenonu(jexnom(nu14//'.NUME.LILI', ligre1), ilinu)

            call dismoi('TYPE_SCA', resu, 'RESUELEM', repk=typsca)
            ASSERT(typsca.eq.'R' .or. typsca.eq.'C')
            tt(1:1)=typsca
            call dismoi('TYPE_MATRICE', resu, 'RESUELEM', repk=symel)
            ASSERT(symel(1:1).eq.'S' .or. symel(1:1) .eq.'N')
            lmesym=(symel(1:1).eq.'S')
            if (lmasym) then
                ASSERT(lmesym)
            endif

!           -- boucle sur les grels du ligrel
!           ==================================
            do igr = 1, zzngel(ilima)
                if (ldgrel .and. mod(igr,nbproc) .ne. rang) goto 60

!               -- il se peut que le grel igr soit vide :
                call jaexin(jexnum(resu//'.RESL', igr), iexi)
                if (iexi .eq. 0) goto 60

                call jeveuo(resu//'.DESC', 'L', jdesc)
                mode=zi(jdesc+igr+1)
                if (mode .gt. 0) then
                    nnoe=nbno(mode)
                    ASSERT(nnoe.le.nbnomx)
                    nbvel=digdel(mode)
!                           -- nombre d'elements du grel igr du ligrel ligre1/ilima
                    nel=zznelg(ilima,igr)
                    call jeveuo(jexnum(resu//'.RESL', igr), 'L', jresl)

!                   boucle sur les elements du grel
!                   -------------------------------
                    do iel = 1, nel
                        call assma3(lmasym, lmesym, tt, igr, iel,&
                                    c1, rang, jnueq, jnumsd, jresl,&
                                    nbvel, nnoe, ldistme, ldgrel,&
                                    ilima, jadli, jadne, jprn1, jprn2,&
                                    jnulo1, jnulo2, jposd1, jposd2, admodl,&
                                    lcmodl, mode, nec, nmxcmp, ncmp,&
                                    jsmhc, jsmdi, iconx1, iconx2, jtmp2,&
                                    lgtmp2, jvalm, ilinu, ellagr,&
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


!   -- mise a jour de REFA(4)
    call jeveuo(mat19//'.REFA', 'E', jrefa)
    if (acreer) then
        zk24(jrefa-1+4)=optio2
    else
        if (zk24(jrefa-1+4) .ne. optio2) zk24(jrefa-1+4)='&&MELANGE'
    endif

    if (niv .ge. 2) then
        call uttcpu('CPU.ASSMAM', 'FIN', ' ')
        call uttcpr('CPU.ASSMAM', 6, temps)
        if (niv .ge. 2) write (ifm, '(A44,D11.4,D11.4)'&
                        ) 'TEMPS CPU/SYS ASSEMBLAGE M                : ',&
                        temps(5), temps(6)
    endif


    if (.not.lmasym) then
!       -- par prudence, on affecte aux termes diagonaux du bloc inferieur
!          les valeurs des termes diagonaux du bloc superieur
        do ieq = 1, nequ
            idia=zi(jsmdi+ieq-1)
            zr(jvalm(2)+idia-1)=zr(jvalm(1)+idia-1)
        end do
    endif

!   -- il faut communiquer ellagr entre les procs :
    if (ldistme) then
        call asmpi_comm_vect('MPI_MAX', 'I', sci=ellagr)
    endif


!   -- mise a l'echelle des coef. de lagrange si necessaire :
    if (ellagr .gt. 0) call assma1(mat19, ldistme)


    if (.not.ldistme) then
        zk24(jrefa-1+11)='MPI_COMPLET'
    else
        if (lmatd) then
            zk24(jrefa-1+11)='MATR_DISTR'
        else
            zk24(jrefa-1+11)='MPI_INCOMPLET'
        endif
    endif


    call jedetr(matdev//'.ADNE')
    call jedetr(matdev//'.ADLI')
    call jedetr('&&ASSMAM.NUMLO1')
    call jedetr('&&ASSMAM.NUMLO2')
    call jedetr('&&ASSMAM.POSDD1')
    call jedetr('&&ASSMAM.POSDD2')
    call jedetr('&&ASSMAM.TMP2')
    call jedbg2(ibid, idbgav)
    if (dbg) call cheksd(matdev, 'SD_MATR_ASSE', iret)

    call asmpi_barrier()
    call uttcpu('CPU.CALC.1', 'FIN', ' ')
    call uttcpu('CPU.ASSE.1', 'FIN', ' ')
    call uttcpu('CPU.ASSE.2', 'FIN', ' ')
    call jedema()
end subroutine
