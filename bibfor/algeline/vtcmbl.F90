subroutine vtcmbl(nbcmb, typcst, const, typech, nomch,&
                  typres, chpres)
!     ------------------------------------------------------------------
!     COMBINAISON LINEAIRE DE CHAM_NO OU DE CHAM_ELEM
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     -----------------------------------------------------------------
!     *  LES CHAM_NOS OU CHAM_ELEMS SONT REELS OU COMPLEXES
!     *  LES SCALAIRES SONT REELS OU COMPLEXES
!     -----------------------------------------------------------------
! IN  : NBCOMB : IS  : NOMBRE DE CHAM_GDS A COMBINER
! IN  : TYPCST : K1  : TYPE DES CONSTANTES (R OU C, OU I )
! IN  : CONST  : R8  : TABLEAU DES COEFFICIENTS
! IN  : TYPECH : K1  : TYPE DES CHAM_GDS   (R OU C)
! IN  : NOMCH  : K19 : NOMS DES CHAM_GDS
! IN  : TYPRES : K1  : TYPE DU CHAMP RESULTAT (R OU C)
! IN  : CHPRES : K19 : NOM DU CHAMP RESULTAT
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/gcncon.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/sdchgd.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: nbcmb
    real(kind=8) :: const(*)
    character(len=*) :: typcst(*), typech(*), nomch(*), typres, chpres
!
!
! DECLARATION VARIABLES LOCALES
    integer :: ibid, irefe, idime, nbsd, ifetc, idd, kfetc, i, ivfetc, ifm, niv
    integer :: icmb, iret, ival, ilimpi, ifeti, lvale, iconst
    integer :: jdesc, jrefe, jvale
    integer :: kdesc, krefe, kvale
    integer :: nbdesc, nbrefe, nbvale
    integer :: nbdes1, nbref1, nbval1
    real(kind=8) :: dimag
    complex(kind=8) :: c8cst
    character(len=4) :: docu, type, kbid
    character(len=5) :: refe, desc, vale, fetc
    character(len=8) :: k8b
    character(len=19) :: ch19, ch19r
    character(len=24) :: method, sdfeti, metho1, sdfet1, k24b
    logical :: lfeti, iddok
!     ------------------------------------------------------------------
!
    call jemarq()
! RECUPERATION ET MAJ DU NIVEAU D'IMPRESSION
    call infniv(ifm, niv)
!
!-----------------------------------------------------------------------
! --- PHASE D'INITIALISATION
!-----------------------------------------------------------------------
    type=typres
    ch19=nomch(1)
!
! CHAM_NO OU CHAM_ELEM ?
    k24b=ch19//'.DESC'
    call jeexin(k24b, ibid)
    if (ibid .gt. 0) then
        k24b=ch19//'.DESC'
        call jelira(k24b, 'DOCU', ibid, docu)
    else
        k24b=ch19//'.CELD'
        call jelira(k24b, 'DOCU', ibid, docu)
    endif
!
! INIT. POUR FETI: NOMBRE DE SOUS-DOMAINES
    nbsd=0
    method=' '
    sdfeti=' '
    fetc='.FETC'
!
! INIT. DE BASE
    if (docu .eq. 'CHNO') then
        refe='.REFE'
        desc='.DESC'
        vale='.VALE'
    else if (docu.eq.'CHML') then
        refe='.CELK'
        desc='.CELD'
        vale='.CELV'
    else
        call u2mess('F', 'UTILITAI_21')
    endif
!
    if (docu .eq. 'CHNO') then
! FETI OR NOT ?
! ON VERIFIE QUE TOUTE LES CHAM_NO DE LA LISTE SONT HOMOGENES
        k24b=ch19//refe
        call jeexin(k24b, iret)
        if (iret .ne. 0) then
            call jelira(k24b, 'LONMAX', nbrefe, k8b)
        else
            if (niv .ge. 2) write (ifm, *)'<FETI/VTCMBL> CHAM_NO SANS REFE ', k24b(1:19)
            nbrefe=0
        endif
        if (nbrefe .ne. 4) then
            if (niv .ge. 2) write (ifm, * ) '<FETI/VTCMBL> CHAM_NO NON ETENDU POUR FETI ',&
                            k24b(1:19)
        else
            call jeveuo(k24b, 'L', irefe)
            method=zk24(irefe+2)
            sdfeti=zk24(irefe+3)
        endif
!
        do 10 icmb = 2, nbcmb
            ch19=nomch(icmb)
            k24b=ch19//refe
            call jeexin(k24b, iret)
            if (iret .ne. 0) then
                call jelira(k24b, 'LONMAX', nbrefe, k8b)
            else
                if (niv .ge. 2) write (ifm, * )'<FETI/VTCMBL> CHAM_NO SANS REFE ', k24b(1:19)
                nbrefe=0
            endif
            if (nbrefe .ne. 4) then
                metho1=' '
                sdfet1=' '
            else
                call jeveuo(k24b, 'L', irefe)
                metho1=zk24(irefe+2)
                sdfet1=zk24(irefe+3)
                if ((metho1.ne.method) .or. (sdfet1.ne. sdfeti)) call u2mess('F', 'ALGELINE3_92')
            endif
10      continue
    endif
!
! INIT. POUR METHODE FETI
    if (method(1:4) .eq. 'FETI') then
        call jeveuo(sdfeti(1:19)//'.FDIM', 'L', idime)
        nbsd=zi(idime)
! PREPARATION POUR LA BOUCLE SUR LES SOUS-DOMAINES. STOCKAGE
! DES ADRESSES DES .FETC DE CHACUN DES CHAM_NOS A CONCATENER
! POUR EVITER (NBCMB-1)*NBSD APPELS A JEVEUO !
        call wkvect('&&VECFETC', 'V V I', nbcmb, ivfetc)
        do 20 icmb = 1, nbcmb
            ch19=nomch(icmb)
            call jeveuo(ch19//fetc, 'L', ifetc)
            zi(ivfetc+icmb-1)=ifetc
20      continue
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
        lfeti=.true.
    else
        lfeti=.false.
    endif
!
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
! IDD=0 --> DOMAINE GLOBAL/ IDD=I --> IEME SOUS-DOMAINE
    do 150 idd = 0, nbsd
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
        if (iddok) then
!
            if (idd .eq. 0) then
! PREMIER CHAM_NO GLOBAL A CONCATENER
                ch19=nomch(1)
            else
! DETOURS PAR LE .FETC DU PREMIER CHAM_NO GLOBAL A CONCATENER POUR
! OBTENIR LE NOM DU CHAM_NO LOCAL CORRESPONDANT (CH19)
                ch19=zk24(zi(ivfetc)+idd-1)(1:19)
            endif
!
! OBTENTION DES ADRESSES ET DES TAILLES DES .DESC, .REFE ET .VALE
! DU PREMIER CHAM_NO (GLOBAL OU LOCAL) A CONCATENER. ON SUPPOSE QUE
! TOUS LES CHAM_NOS DE LA LISTE NOMCH SONT HOMOGENES SUR CE POINT.
            call jelira(ch19//desc, 'LONMAX', nbdesc, k8b)
            call jelira(ch19//vale, 'LONMAX', nbvale, k8b)
            call jelira(ch19//refe, 'LONMAX', nbrefe, k8b)
            call jeveuo(ch19//desc, 'L', jdesc)
            call jeveuo(ch19//refe, 'L', jrefe)
!
! CONSTRUCTION D'UN CHAM_GD RESULTAT SUR LE MODELE DE NOMCH(1)
            if (idd .eq. 0) then
! CHAM_NO GLOBAL RESULTAT
                ch19r=chpres
                call jeexin(ch19r(1:19)//fetc, ifeti)
            else
! CHAM_NO RESULTAT A CREER
                if (ifeti .eq. 0) then
                    call gcncon('.', k8b)
                    k8b(1:1)='F'
                    ch19r=chpres(1:11)//k8b
                    zk24(kfetc+idd-1)(1:19)=ch19r
                else
! CHAM_NO RESULTAT DEJA EXISTANT A ECRASER
                    ch19r=zk24(kfetc+idd-1)(1:19)
                endif
            endif
            call jeexin(ch19r//vale, iret)
            if (iret .eq. 0) then
                call wkvect(ch19r//desc, 'V V I', nbdesc, kdesc)
                call wkvect(ch19r//vale, 'V V '//type, nbvale, kvale)
                call wkvect(ch19r//refe, 'V V K24', nbrefe, krefe)
! SI FETI CONSTITUTION DE L'OBJET JEVEUX CHPRESS.FETC COMPLEMENTAIRE
                if ((nbsd.gt.0) .and. (idd.eq.0)) call wkvect(ch19r// fetc, 'V V K24', nbsd,&
                                                              kfetc)
            else
                call jeveuo(ch19r//desc, 'E', kdesc)
                call jelira(ch19r//desc, 'LONMAX', nbdes1, kbid)
                call jeveuo(ch19r//refe, 'E', krefe)
                call jelira(ch19r//refe, 'LONMAX', nbref1, kbid)
                call jelira(ch19r//vale, 'LONMAX', nbval1, kbid)
! VERIFICATION DE LA COHERENCE DES DIMENSIONS
                call assert(nbdes1.eq.nbdesc)
                call assert(nbref1.eq.nbrefe)
                call assert(nbval1.eq.nbvale)
! SI FETI CONNEXION A L'OBJET JEVEUX CHPRESS.FETC COMPLEMENTAIRE
                if ((nbsd.gt.0) .and. (idd.eq.0)) call jeveuo(ch19r// fetc, 'E', kfetc)
            endif
            call jeecra(ch19r//desc, 'DOCU', ibid, docu)
! RECOPIE DU .DESC ET DU .REFE DU PREMIER CHAM_NO DE LA LISTE
! DANS CEUX DU CHAM_NO SOLUTION
            do 30 i = 0, nbdesc-1
                zi(kdesc+i)=zi(jdesc+i)
30          continue
            do 40 i = 0, nbrefe-1
                zk24(krefe+i)=zk24(jrefe+i)
40          continue
!
! CHANGER LA GRANDEUR
            call sdchgd(ch19r, typres)
!
! VECTEUR RECEPTACLE TEMPORAIRE DE LA COMBINAISON LINEAIRE
            call wkvect('&&VTCMBL.VALE', 'V V '//type, nbvale, lvale)
!
!-----------------------------------------------------------------------
! --- BOUCLE SUR LES CHAM_GDS A COMBINER
!-----------------------------------------------------------------------
            iconst=1
            do 120 icmb = 1, nbcmb
!
! CHAM_NO A CONCATENER
                if (idd .eq. 0) then
! DOMAINE GLOBAL
                    ch19=nomch(icmb)
                else
! SOUS-DOMAINE N°IDD
                    ch19=zk24(zi(ivfetc+icmb-1)+idd-1)(1:19)
                endif
!
                call jeveuo(ch19//vale, 'L', jvale)
                if (typres(1:1) .eq. 'R') then
                    if (typech(icmb)(1:1) .eq. 'R') then
                        do 50 ival = 0, nbvale-1
                            zr(lvale+ival)=zr(lvale+ival)+ const(&
                            iconst)*zr(jvale+ival)
50                      continue
                    else
                        if (typcst(icmb)(1:1) .eq. 'R') then
                            do 60 ival = 0, nbvale-1
                                zr(lvale+ival)=zr(lvale+ival)+&
                                const(iconst)*dble(zc(jvale+ival))
60                          continue
                        else if (typcst(icmb)(1:1).eq.'I') then
                            do 70 ival = 0, nbvale-1
                                zr(lvale+ival)=zr(lvale+ival)+&
                                const(iconst)*dimag(zc(jvale+ival))
70                          continue
                        else
                            type=typcst(icmb)(1:1)
                            call u2mesk('F', 'PREPOST3_6', 1, type)
                        endif
                    endif
                else
                    if (typech(icmb)(1:1) .eq. 'R') then
                        if (typcst(icmb)(1:1) .eq. 'R') then
                            do 80 ival = 0, nbvale-1
                                zc(lvale+ival)=zc(lvale+ival)+&
                                const(iconst)*zr(jvale+ival)
80                          continue
                        else if (typcst(icmb)(1:1).eq.'C') then
                            c8cst=dcmplx(const(iconst),const(iconst+1)&
                            )
                            do 90 ival = 0, nbvale-1
                                zc(lvale+ival)=zc(lvale+ival)+c8cst*&
                                zr(jvale+ival)
90                          continue
                        endif
                    else
                        if (typcst(icmb)(1:1) .eq. 'R') then
                            do 100 ival = 0, nbvale-1
                                zc(lvale+ival)=zc(lvale+ival)+&
                                const(iconst)*zc(jvale+ival)
100                          continue
                        else if (typcst(icmb)(:1).eq.'C') then
                            c8cst=dcmplx(const(iconst),const(iconst+1)&
                            )
                            do 110 ival = 0, nbvale-1
                                zc(lvale+ival)=zc(lvale+ival)+c8cst*&
                                zc(jvale+ival)
110                          continue
                        endif
                    endif
                endif
                call jelibe(ch19//vale)
                iconst=iconst+1
                if (typcst(icmb)(1:1) .eq. 'C') iconst=iconst+1
!
!-----------------------------------------------------------------------
! --- FIN BOUCLE CHAM_GD
!-----------------------------------------------------------------------
120          continue
!
!   IL EST NECESSAIRE D'ACTUALISER KVALE SI LE RESULTAT EST DANS NOMCH()
            call jeveuo(ch19r//vale, 'E', kvale)
            if (type(1:1) .eq. 'R') then
                do 130 ival = 0, nbvale-1
                    zr(kvale+ival)=zr(lvale+ival)
130              continue
            else if (type(1:1).eq.'C') then
                do 140 ival = 0, nbvale-1
                    zc(kvale+ival)=zc(lvale+ival)
140              continue
            endif
!
! DESTRUCTION DU RECEPTACLE TEMPORAIRE, CAR SA TAILLE VA CHANGER A
! L'ITERATION SUIVANTE
            call jedetr('&&VTCMBL.VALE')
!
! LIBERATION DU CHAMP JEVEUX RESULTAT
            call jelibe(ch19r//vale)
!
        endif
!========================================
! FIN BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
150  end do
!
! DESTRUCTION OBJET JEVEUX TEMPORAIRE POUR FETI
    if (nbsd .gt. 0) call jedetr('&&VECFETC')
!
    call jedema()
end subroutine
