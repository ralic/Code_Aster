subroutine fetcrf(sdpart1)
!-----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: aimery.assire at edf.fr
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  CREATION DE LA STRUCTURE DE DONNEES SD_PARTIT.
!
! IN sdpart   : NOM DU CONCEPT PRODUIT
! OUT sdpart  : LE CONCEPT EST CREE ET INSTANCIE
!
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!       MESSAGE:INFNIV.
!       JEVEUX:JEMARQ,JEDEMA,JECROC,JEECRA,JEVEUO,WKVECT.
!
!     FONCTIONS INTRINSEQUES:
!       NONE.
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       02/11/03 (OB): CREATION.
!       NIV>2  JXVERI
!       NIV>3  UTIMSD + AFFICHAGES GLOBAUX
!       NIV>4  AFFICHAGES DETAILLES
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1501
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/gmgnre.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/jxveri.h"
#include "asterfort/lxcadr.h"
#include "asterfort/utimsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: sdpart1
!
!
! DECLARATION VARIABLES LOCALES
    integer :: nbno, ngma, nbrd, nbmabd, lstgma, lstbrd, nomcha, nomsd, jadr, i
    integer :: j, nbsd, intbuf, ifm, niv, nbchar, multc, nbid, itmp, i1, nec30
    integer :: nbnot2, j2, n31, iret, ier, nbnoto, ierd, ialino, jtrav, ianbno
    integer :: nbmail, ialima, jj, nbnosd, nb, ianbma, ialibd, is9, incrs, l, xt
    integer :: yt, zt, ialsno, ialsmu, k, nn, nbma, linoma, ialsma, jnoma, jprnm
    integer :: nec, n, ino, ialsk, numsd, ialspo, ipos, iajadr, jtmp, ialstr
    integer :: jadri, jadrj, ialsfg, nbmato, nbvm, nbfete, ialsml, ialsmd
    integer :: nbmama, iliddl, iaux1, iaux2, iaux3, iaux0, nd, iadr, n1, n2, n3
    integer :: nbobj1, lonli1, idlig1, nval, nbmata, ndtar, iaux, isdlch, isdmat
    integer :: inbmch, iflim, iflii, dec, ich, isd, ilscha, idflii, idflim
    integer :: idflm, idfln, ifnt, nb1, nb2, biflii, biflim, bifln, jadrh
    integer :: nbnota, nbmatr, noinch, iliais, inbno, icompt, ll, nbddli, jmeth
    integer :: vali(5), iafeta, nbmabo, ial, ials, jal, itma, nber, lil, lils
    integer :: iao, iaos, addr, k1, ktmp, nzoco, izone, ifcfl, jsuma, jsuno
    integer :: iform
    integer :: isurf, jdecma, jdecno, jmaco, idd, cfsd, ifcfm, numma, ifcfn
    integer :: ifcfb, nzocom, kadr, ladr, ima, jzone, isuco, jnoco, ifetb, lfetb
    integer :: ddlm, madr, iiaux1, jdim, ifcnm, nbsurf, inddz
    character(len=4) :: k4tmp
    character(len=8) :: k8bid, nom, ma, k8b, nomo, nomn1, nomn, noma, nocha1
    character(len=19) :: sdpart, ligrmo, ligrch
    character(len=24) :: nomsda, nomsdb, nomsdi, nomsdg, nomsdm, nomsdh, nomsdj
    character(len=24) :: nomsln, nomsli, nomslm, nomnoe, nomgma, nomref, grpma
    character(len=24) :: valk(3), methco, pzone, psurma, psurno, contma, nomfcl
    character(len=24) :: nomfcm, nomfcn, contno, k24bid, nomfci, ndimco, k24buf
    logical :: lbord, lcfc1, lpaire
!
!
! CORPS DU PROGRAMME
    call jemarq()
    call infniv(ifm, niv)
    if (niv .ge. 3) call jxveri()
!
!**********************************************************************
! INITIALISATIONS
!**********************************************************************
    sdpart=sdpart1
    nomref=sdpart//'.FREF'
    nomsdm=sdpart//'.FDIM'
    nomsda=sdpart//'.FETA'
    nomsdb=sdpart//'.FETB'
    nomsdg=sdpart//'.FETG'
    nomsdh=sdpart//'.FETH'
    nomsdi=sdpart//'.FETI'
    nomsdj=sdpart//'.FETJ'
    nomsln=sdpart//'.FLIN'
    nomsli=sdpart//'.FLII'
    nomslm=sdpart//'.FLIM'
    nomfcl=sdpart//'.FCFL'
    nomfci=sdpart//'.FCFI'
    nomfcm=sdpart//'.FCFM'
    nomfcn=sdpart//'.FCFN'
!     RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MODELE
    call getvid(' ', 'MODELE', scal=nomo, nbret=nbvm)
!
!     LIGREL DU MODELE
    ligrmo = nomo(1:8)//'.MODELE'
!     NBCHAR: NBRE DE CHARGE
    call getfac('EXCIT', nbchar)
!     VECTEURS TEMPORAIRES DES CHARGES
    if (nbchar .gt. 0) then
        call wkvect('&&FETCRF.NOMCHA   ', 'V V K8', nbchar, nomcha)
        do i = 1, nbchar
            call getvid('EXCIT', 'CHARGE', iocc=i, scal=zk8(nomcha-1+i), nbret=nd)
        end do
    endif
!     NBSD: NBRE D'OCCURENCE DU MOT-CLE DEFI
    call getfac('DEFI', nbsd)
    nbsd = abs(nbsd)
!     VECTEUR DES NBRE DE NOEUDS
    call wkvect('&&FETCRF.NBNO     ', 'V V I', nbsd, nbno)
!     FLAGS DE L'EXISTENCE DES GROUP_MA VOLUMIQUES
    call wkvect('&&FETCRF.NGMA     ', 'V V I', nbsd, ngma)
!     IDEM GROUP_MA DE BORDS
    call wkvect('&&FETCRF.NBRD     ', 'V V I', nbsd, nbrd)
!     VECTEUR DES NBRE DE MAILLES DE BORD
    call wkvect('&&FETCRF.NBMABD   ', 'V V I', nbsd, nbmabd)
!     LISTES DES GROUP_MA VOLUMIQUES
    call wkvect('&&FETCRF.LSTGMA   ', 'V V K24', nbsd, lstgma)
!     IDEM DE BORDS
    call wkvect('&&FETCRF.LSTBRD   ', 'V V K24', nbsd, lstbrd)
    call wkvect('&&FETCRF.NOMSD    ', 'V V K24', nbsd, nomsd)
!
!     CREATION .FREF
    intbuf=nbchar+1
    call wkvect(nomref, 'G V K8', intbuf, jadr)
    zk8(jadr)=nomo
    do i = 1, nbchar
        zk8(jadr+i)=zk8(nomcha-1+i)
    end do
    if (niv .ge. 4) call utimsd(ifm, 2, .false., .true., nomref,&
                                1, 'G')
!     MA: MAILLAGE ASSOCIE AU MODELE
    call getvid(' ', 'MAILLAGE', scal=noma, nbret=nbvm)
    if (nbvm .eq. 0) then
        call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
        ma = zk8(jnoma)
    else
        ma=noma
    endif
!     DETERMINATION DU NB TOTAL DE NOEUDS ET DE MAILLES DU MAILLAGE
    call dismoi('F', 'NB_NO_MAILLA', ma, 'MAILLAGE', nbnoto,&
                k8b, ierd)
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbmato,&
                k8b, ierd)
    nbmatr=nbmato
    grpma = ma//'.GROUPEMA       '
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
    call jelira(ligrmo//'.PRNM', 'LONMAX', n)
    nec = n/nbnoto
    nec30=30*nec
!
!     LECTURE DU CONTENU DES MOT-CLES DEFI
    do i = 1, nbsd
        call getvtx('DEFI', 'GROUP_MA', iocc=i, scal=zk24(lstgma-1+i), nbret=zi(ngma-1+i))
        if (zi(ngma-1+i) .ne. 1) then
            vali(1)=i
            call utmess('F', 'ELEMENTS5_25', si=vali(1))
        endif
        call getvtx('DEFI', 'GROUP_MA_BORD', iocc=i, scal=zk24(lstbrd-1+i), nbret=zi(nbrd-1+i))
    end do
!
!     NOMS DES SOUS-DOMAINES : NOMSD
!     ON PREND LE MOT-CLE NOM TRONQUE A 4  ET ON AJOUTE LE NUM
    call getvtx(' ', 'NOM', scal=nom, nbret=nbid)
    k4tmp=nom(1:4)
    itmp=0
    iaux=len(k4tmp)
    do i = 1, iaux
        if (k4tmp(i:i) .ne. ' ') itmp=itmp+1
    end do
    do i = 1, nbsd
        write(k8bid,'(I4)') i
        call lxcadr(k8bid)
        zk24(nomsd-1+i) = k4tmp(1:itmp)//k8bid
    end do
!
! TEST POUR VERFIFIER LA COHERENCE DES PARAMETRES DE LA MACRO ET DES
! GROUP_MA CREES EFFECTIVEMENT DANS LE MAILLAGE
    ier = 0
    do i = 1, nbsd
        nomgma=zk24(lstgma-1+i)
        call jeexin(jexnom(grpma, nomgma), iret)
        if (iret .eq. 0) then
            ier = ier + 1
            call utmess('E', 'ELEMENTS_62', sk=nomgma)
        endif
    end do
    ASSERT(ier.eq.0)
!
!     CREATION sdpart / .FETB
    call jecrec(nomsdb, 'G V I', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsd)
!     VECTEUR DES NBRE DE NOEUDS
    call wkvect('&&FETCRF.NB_NO    ', 'V V I', nbsd, ianbno)
!     VECTEUR DES NBRE DE MAILLES
    call wkvect('&&FETCRF.NB_MA    ', 'V V I', nbsd, ianbma)
!     STOCKAGE DES ADRESSES DES .FETB DE CHAQUE SD
    call wkvect('&&FETCRF.L_NO_JADR', 'V V I', nbsd, iajadr)
!     STOCKAGE DES ADRESSES DES .FETA DE CHAQUE SD
    call wkvect('&&FETCRF.ADR_FETA', 'V V I', nbsd, iafeta)
!     VECTEUR DES DDL CUMULES DE CHAQUE SD
    call wkvect('&&FETCRF.L_K      ', 'V V I', nbsd, ialsk)
    if (niv .ge. 3) call jxveri()
!
!***********************************************************************
! BOUCLE 1 SUR LES SOUS-DOMAINES POUR DETERMINER:
! PAR SD: NBRE DE MAILLES (NBMAIL), ADDRESSE GROUP_MA (IALIMA),
!         NB DE NOEUDS (IANBNO)
! NBRE TOTAL DE MAILLES VOLUMIQUES (NBMATO)/NOEUDS(NB) AVEC MULTIPLICITE
!***********************************************************************
!
    nbmato=0
    nb=0
    nbnot2=2*nbnoto
    call wkvect('&&FETCRF.TRAV ', 'V V I', nbnot2, jtrav)
    call wkvect('&&FETCRF.TMP  ', 'V V I', nbnot2, jtmp)
    do i = 1, nbsd
        nomgma=zk24(lstgma-1+i)
        call jelira(jexnom(grpma, nomgma), 'LONUTI', nbmail)
        call jeveuo(jexnom(grpma, nomgma), 'L', ialima)
        zi(iafeta+i-1)=ialima
        zi(ianbma-1+i)=nbmail
        nbmato=nbmato+nbmail
!
!       GMGNRE : donne la liste des noeuds ZI(JTMP) d'une liste de
!                mailles ZI(IALIMA)
!                sortie : liste des noeuds = ZI(JTMP)
!                         nombre de noeuds = ZI(IANBNO-1+I)
        call gmgnre(ma, nbnoto, zi(jtrav), zi(ialima), nbmail,&
                    zi(jtmp), nbnosd, 'TOUS')
        zi(ianbno-1+i)=nbnosd
        zi(nbno-1+i)=nbnosd
        nb=nb+nbnosd
    end do
!
! ****** ON PEUT MAINTENANT DIMENSIONNER EXACTEMENT:
!     LISTE DES NOEUDS AVEC MULTIPLICITES
    call wkvect('&&FETCRF.LISTE_NO ', 'V V I', nb, ialino)
!     NUMERO DE LA SD POUR UN NOEUD DONNE
    call wkvect('&&FETCRF.L_NO_GMA ', 'V V I', nb, linoma)
!     INDICE DANS CETTE SD POUR UN NOEUD DONNE
    call wkvect('&&FETCRF.L_NO_POS ', 'V V I', nb, ialspo)
    if (niv .ge. 3) call jxveri()
!
!**********************************************************************
! BOUCLE 2 SUR LES SOUS-DOMAINES POUR DETERMINER:
! PAR SD: NBRE DE MAILLES (NBMAIL), DE NOEUDS (NBNOSD), DE DDLS (K)
! REMPLIR .FETB
!**********************************************************************
    nb=0
    do i = 1, nbsd
!       ON REFAIT UN COUP POUR CETTE FOIS REMPLIR IALINO
        ialima= zi(iafeta+i-1)
        nbmail=zi(ianbma-1+i)
        call jerazo('&&FETCRF.TMP  ', nbnot2, 1)
        call gmgnre(ma, nbnoto, zi(jtrav), zi(ialima), nbmail,&
                    zi(jtmp), nbnosd, 'TOUS')
!       CREATION DE sdpart.FETB
        k24buf=zk24(nomsd-1+i)
        call jecroc(jexnom(nomsdb, k24buf))
        intbuf=2*nbnosd
        call jeecra(jexnom(nomsdb, k24buf), 'LONMAX', intbuf)
        call jeveuo(jexnom(nomsdb, k24buf), 'E', jadr)
        zi(iajadr-1+i)=jadr
! **** SOUS-BOUCLE 2.1 SUR LES NOEUDS POUR DETERMINER LE NBRE DE DDLS
!      ET REMPLIR .FETB (SANS LES SIGNES DES INTERFACES)
        k=0
        do j = 1, nbnosd
            ino = zi(jtmp-1+j)
            zi(jadr+2*(j-1))=ino
            do l = 1, nec30
                if (exisdg(zi(jprnm-1+nec*(ino-1)+1),l)) k=k+1
            end do
            zi(jadr+2*(j-1)+1)=k
        end do
!       NB DDL TOTAL DU SOUS-DOMAINE I
        zi(ialsk-1+i)=k
! **** SOUS-BOUCLE 2.2 SUR LES NOEUDS POUR REMPLIR
!       VECTEUR DES NOEUDS :  ZI(IALINO-1+...NB)
!       VECTEUR DE CORRESPONDANCE NOEUD -> SD: ZI(LINOMA-1+...NB)
!       VECTEUR DE CORRESPONDANCE NOEUD -> POSITION DANS SD: ZI(IALSPO)
        ipos=1
        do j = 1, nbnosd
            zi(ialino+nb-1+j)= zi(jtmp-1+j)
            zi(linoma+nb-1+j)= i
            zi(ialspo+nb-1+j)= ipos
            ipos=ipos+1
        end do
        nb=nb+nbnosd
    end do
    call jedetr('&&FETCRF.TMP')
    call jedetr('&&FETCRF.TRAV')
    if (niv .ge. 3) call jxveri()
!
!***********************************************************************
! BOUCLE 3 SUR LES SOUS-DOMAINES POUR REMPLIR .FETA
!          RAJOUT DES MAILLES SURFACIQUES A NBMATO
!***********************************************************************
!     PREPARATION DU TEST SUR LES MAILLES
    call wkvect('&&FETCRF.TESTMA ', 'V V I', nbmatr, itma)
    call jecrec(nomsda, 'G V I', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsd)
    do i = 1, nbsd
        k24buf=zk24(nomsd-1+i)
        call jecroc(jexnom(nomsda, k24buf))
!
!       NB MAILLES/ADDRESSE DE LEURS NUMEROS
        nbmail=zi(ianbma-1+i)
        ialima=zi(iafeta+i-1)
!
!       NB MAILLES DE BORDS SI BESOIN...
        zi(nbmabd-1+i)=0
        if (zi(nbrd-1+i) .eq. 1) then
            nomgma=zk24(lstbrd-1+i)
            call jelira(jexnom(grpma, nomgma), 'LONUTI', nbmabo)
            call jeveuo(jexnom(grpma, nomgma), 'L', ialibd)
            zi(nbmabd-1+i)=nbmabo
            lbord=.true.
        else
            nbmabo=0
            lbord=.false.
        endif
!       ON RAJOUTE LES MAILLES DE BORDS AU TOTAL DES MAILLES DES SD
        nbmato=nbmato+nbmabo
!       NB MAILLES TOTALES (BORDS+VOLUME) DU SD I
        nbid=nbmail+nbmabo
!       REMPLISSAGE EFFECTIF DE FETA
        call jeecra(jexnom(nomsda, k24buf), 'LONMAX', nbid)
        call jeveuo(jexnom(nomsda, k24buf), 'E', jadr)
!       MAILLES VOLUMIQUES
        do j = 1, nbmail
            ial=zi(ialima+j-1)
            zi(jadr+j-1)=ial
!         POUR TEST SUR LES MAILLES
            zi(itma+ial-1)=1
        end do
!       MAILLES DE BORD
        if (lbord) then
            do j = 1, nbmabo
                ial=zi(ialibd-1+j)
                zi(jadr+j-1+nbmail)=ial
                zi(itma+ial-1)=1
            end do
        endif
    end do
!
! **** TEST NBRE DE MAILLES MODELE = SOMME DES MAILLES DES GROUP_MA
    call jelira(nomo(1:8)//'.MAILLE', 'LONMAX', nbma)
    call jeveuo(nomo(1:8)//'.MAILLE', 'L', ial)
    nbmama=0
    nber=0
    do i = 1, nbma
!       MAILLE DU MODELE
        if (zi(ial+i-1) .ne. 0) then
            nbmama=nbmama+1
!         MAILLE DANS UN SD OU PAS ?
            if (zi(itma-1+i) .eq. 0) nber=nber+1
        endif
    end do
    if (nbmama .ne. nbmato) then
        vali(1)=nbmama
        vali(2)=nbmato
        call utmess('F', 'ELEMENTS5_24', ni=2, vali=vali)
    endif
    if (nber .gt. 0) then
        call utmess('F', 'ELEMENTS5_28', si=nber)
    endif
    nber=0
    do i = 1, nbmato
!       MAILLE DANS UN SD
        if (zi(itma-1+i) .ne. 0) then
!         MAILLE DU MODELE ?
            if (zi(ial+i-1) .eq. 0) nber=nber+1
        endif
    end do
    if (nber .gt. 0) then
        call utmess('F', 'ELEMENTS5_29', si=nber)
    endif
    call jedetr('&&FETCRF.TESTMA ')
    if (niv .ge. 3) call jxveri()
    if (niv .ge. 4) call utimsd(ifm, 2, .false., .true., nomsda,&
                                1, 'G')
!
!**********************************************************************
! FIN .FETA ET .FETB (SAUF SIGNE) ET AFFICHAGE
!**********************************************************************
    if (niv .ge. 4) then
        write(ifm,*) ' '
        write(ifm,*) '------------------------------------'
        write(ifm,*) '|           FETA ET FETB            |'
        write(ifm,*) '|  PAR SD :   - NB DDL              |'
        write(ifm,*) '|             - NB MAILLES          |'
        write(ifm,*) '|             - NB NOEUDS           |'
        write(ifm,*) ' '
        do i = 1, nbsd
            write(ifm,*) 'SD=',i
            write(ifm,*) 'DDL TOTAL   =',zi(ialsk-1+i)
            write(ifm,*) 'MA VOL/BORD =',zi(ianbma-1+i),zi(nbmabd-1+i)
            write(ifm,*) 'NO TOTAL    =',zi(ianbno-1+i)
        end do
    endif
!
    if (niv .ge. 5) then
        write(ifm,*) ' '
        write(ifm,*) '------------------------------------'
        write(ifm,*) '|    TOUS LES NOEUDS ET LEUR SD     |'
        write(ifm,*) ' '
        do j = 1, nb
            write(ifm,*) 'NOEUD=',zi(ialino-1+j), 'SD=',zi(linoma-1+j)
        end do
        write(ifm,*) 'TOTAL NOEUDS (AVEC MULTIPLICITE)=',nb
        write(ifm,*) ' '
        write(ifm,*) '------------------------------------'
    endif
!
!**********************************************************************
! ON REORDONNE LA LISTE DE TOUS LES NOEUDS DES SD (ET LEUR SD)
!   (INSPIRE DU TRI A BULLE UTTRIF.F)
!   PARTIE A OPTIMISER (VOIR TRI PAR INSERTION)
!**********************************************************************
!
!        --- TRI BULLE ---
    if (nb .gt. 1) then
!         --- CHOIX DE L'INCREMENT ---
        incrs = 1
        is9 = nb / 9
 11     continue
        if (incrs .lt. is9) then
            incrs = 3*incrs+1
            goto 11
        endif
!         --- REMONTEE DES BULLES ---
 12     continue
        do j = incrs+1, nb
            l = j-incrs
 13         continue
            if (l .gt. 0) then
                ial=ialino-1+l
                ials=ial+incrs
                lil=linoma-1+l
                lils=lil+incrs
                iao=ialspo-1+l
                iaos=iao+incrs
                if (zi(ial) .gt. zi(ials)) then
!            --- PERMUTATION DES VALEURS ---
                    xt = zi(ial)
                    zi(ial) = zi(ials)
                    zi(ials) = xt
                    yt = zi(lil)
                    zi(lil) = zi(lils)
                    zi(lils) = yt
                    zt = zi(iao)
                    zi(iao) = zi(iaos)
                    zi(iaos) = zt
                    l = l - incrs
                    goto 13
                endif
            endif
        end do
        incrs = incrs/3
        if (incrs .ge. 1) goto 12
    endif
!
!***********************************************************************
! UN NOEUD D'INTERFACE EST DEFINI COMME ETANT COMMUN A PLUSIEURS SD
! ----------------------------------------------------------------------
! CONSTRUCTION DES LISTES DES NOEUDS D'INTERFACE, DE LEUR MULTIPLICITE
!   (IE LES NOEUDS PRESENTS AU MOINS DEUX FOIS DANS LA LISTE PRECEDENTE)
!   ET DE LA LISTE DES SD AUXQUELS ILS APPARTIENNENT :
!    ZI(IALSNO-1+K)  , K=1,NB  : LISTE DES NOEUDS D'INTERFACE
!    ZI(IALSMU-1+K)  , K=1,NB  : LISTE DE LEUR MULTIPLICITE
!    ZI(IALSMA-1+KK) , KK=1,(SOMME DES PRECEDENTS) : LISTE DES SD
!***********************************************************************
!
!     VECTEUR DES NOEUDS D'INTERFACE
    call wkvect('&&FETCRF.LST_ORDO ', 'V V I', nb, ialsno)
!     LEUR MULTIPLICITE
    call wkvect('&&FETCRF.LST_MULT ', 'V V I', nb, ialsmu)
!     LES COUPLES (SD1,SD2)
    call wkvect('&&FETCRF.LST_GMA  ', 'V V I', 2*nb, ialsma)
!     VECTEURS DES TRIPLETS (NOEUDS INTERFACE,SD1,SD2)
    call wkvect('&&FETCRF.LST_TRI  ', 'V V I', 3*nb+3, ialstr)
!
    if (niv .ge. 3) call jxveri()
!
!***********************************************************************
! DOUBLE BOUCLE 4 SUR LES NOEUDS AVEC LEUR MULTIPLICITE POUR FINIR DE
! REMPLIR FETB (SIGNE) + IALSTR
!***********************************************************************
!     K1:  NBRE DE NOEUDS D'INTERFACE AVEC MULTIPLICITE
!     NB: NBRE DE NOEUDS TOTAL EN COMPTANT LES MULTIPLICITES
    k=1
    do i = 1, nb
        ial=zi(ialino-1+i)
        i1=i+1
        do j = i1, nb
            jal=zi(ialino-1+j)
!         SI ON A TROUVE UN NOEUD D'INTERFACE (CAD COMMUN A 2 SD)
            if (ial .eq. jal) then
!           ON COMPLETE LE TABLEAU DES NOEUDS D'INTERFACE ENTRE 2 SD
!           ON PREND ITMP < JTMP
                itmp=zi(linoma-1+i)
                jtmp=zi(linoma-1+j)
                if (jtmp .lt. itmp) then
                    itmp=zi(linoma-1+j)
                    jtmp=zi(linoma-1+i)
                endif
!
!           ON MODIFIE LE SIGNE DU NOEUD DANS .FETB POUR LES 2 SD
                jadr=zi(iajadr-1+zi(linoma-1+i))
                addr=jadr+2*(zi(ialspo-1+i)-1)
                zi(addr)=-1*abs(zi(addr))
                jadr=zi(iajadr-1+zi(linoma-1+j))
                addr=jadr+2*(zi(ialspo-1+j)-1)
                zi(addr)=-1*abs(zi(addr))
!
!           ON REMPLI LA LISTE DES TRIPLETS (NOEUD, SD1, SD2)
                zi(ialstr+3*(k-1)) =ial
                zi(ialstr+3*(k-1)+1)=itmp
                zi(ialstr+3*(k-1)+2)=jtmp
!           ON INCREMENTE LE NOMBRE DE NOEUDS TROUVES
                k=k+1
            endif
!
        end do
    end do
    if (niv .ge. 3) call jxveri()
    if (niv .ge. 4) call utimsd(ifm, 2, .false., .true., nomsdb,&
                                1, 'G')
!
!***********************************************************************
! DOUBLE BOUCLE 5 SUR LES NOEUDS D'INTERFACE AVEC LEUR MULTIPLICITE
! GEOMETRIQUE
!***********************************************************************
    k1=k-1
!     MULTIPLICITE (GEOMETRIQUE) DES NOEUDS D'INTERFACE
    call wkvect('&&FETCRF.LST_MLT  ', 'V V I', k1, ialsml)
!     VECTEUR AUXILIAIRE POUR LA BOUCLE CI DESSOUS
    call wkvect('&&FETCRF.LST_MSD  ', 'V V I', nbsd, ialsmd)
    do l = 1, k1
        ial=zi(ialstr+3*(l-1))
        call jerazo('&&FETCRF.LST_MSD  ', nbsd, 1)
        do i = 1, k1
            jal=ialstr+3*(i-1)
            if (zi(jal) .eq. ial) then
                jtmp=zi(jal+1)
                zi(ialsmd+jtmp-1)=1
                jtmp=zi(jal+2)
                zi(ialsmd+jtmp-1)=1
            endif
        end do
        nn=0
        do j = 1, nbsd
            if (zi(ialsmd+j-1) .eq. 1) nn=nn+1
        end do
        zi(ialsml+l-1)=nn
    end do
!
    if (niv .ge. 5) then
        write(ifm,*) ' '
        write(ifm,*) '------------------------------------'
        write(ifm,*) '  NOEUDS ET LAGRANGES D''INTERFACE  |'
        write(ifm,*) ' '
    endif
!
!***********************************************************************
! DOUBLE BOUCLE 6 SUR LES NOEUDS D'INTERFACE AVEC LEUR MULTIPLICITE
! TENANT COMPTE DES INTERFACES DE MESURE NULLE
! NBFETE : NOEUD D'INTERFACE AVEC MULTIPLICITE=K1
!***********************************************************************
    do i = 1, k1
        addr=ialstr+3*(i-1)
        itmp=zi(addr+1)
        jtmp=zi(addr+2)
        ktmp=zi(addr)
        if (niv .ge. 5) write(ifm, *)'NOEUD/SD1/SD2/NB: ', ktmp, itmp, jtmp, zi(ialsml+i-1)
        zi(ialsno-1+i)=ktmp
        zi(ialsmu-1+i)=zi(ialsml+i-1)
    end do
!     NB NOEUDS INTERFACE (EN COMPTANT LEUR MULTIPLICITE)
    nbfete=k1
!
    if (niv .ge. 5) then
        write(ifm,*) 'IL Y A :',nbfete,' NOEUDS INTERFACE'
        write(ifm,*) '-------------------------------------'
        write(ifm,*) ' '
    endif
    if (niv .ge. 3) call jxveri()
!
!***********************************************************************
! REMPLISSAGE EFFECTIF DE .FETI ET .FETJ
!***********************************************************************
    call wkvect(nomsdj, 'G V I', 2*nbfete, jadrj)
    call wkvect(nomsdi, 'G V I', 4*nbfete, jadri)
    multc = 1
    k=0
    do i = 1, nbfete
!
!       NUMERO DU NOEUD D'INTERFACE ET MULTIPLICITE
        addr=jadri+4*(i-1)
        ino=zi(ialsno-1+i)
        zi(addr)=ino
        zi(addr+1)=zi(ialsmu-1+i)
!       NB DDL CUMULES
        do j = 1, nec30
            if (exisdg(zi(jprnm-1+nec*(ino-1)+1),j)) k=k+1
        end do
        zi(addr+2)=k
!       LISTE DES SD D'APPARTENANCE
        zi(addr+3)=multc
!       VALEUR DES SDS
        zi(jadrj-1+multc) =zi(ialstr+3*(i-1)+1)
        zi(jadrj-1+multc+1)=zi(ialstr+3*(i-1)+2)
        multc=multc+2
    end do
!
!     ON STOCKE LE NB DE DDL TOTAL DES NOEUDS D'INTERFACE
    nbddli=k
!
    if (niv .ge. 3) call jxveri()
    if (niv .ge. 4) then
        call utimsd(ifm, 2, .false., .true., nomsdi,&
                    1, 'G')
        call utimsd(ifm, 2, .false., .true., nomsdj,&
                    1, 'G')
    endif
!
!***********************************************************************
! DOUBLE BOUCLE 7 POUR CONSTRUIRE .FETG
!***********************************************************************
    call jecrec(nomsdg, 'G V I', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsd)
    call wkvect('&&FETCRF.LST_FETG ', 'V V I', 2*nbfete, ialsfg)
    do i = 1, nbsd
!**** ON DETERMINE LES DONNEES A METTRE DANS .FETG ET SA TAILLE
!       NB D'OCCURENCE DANS FETG
        nn=0
!       ADRESSE DE DEBUT DE FETB POUR LE SD I
        jadr=zi(iajadr-1+i)
!       NBRE DE NOEUDS DU SD I
        nbnosd=zi(ianbno-1+i)
        do j = 1, nbnosd
!         SI LE NOEUD DANS FETB EST NEGATIF (IE NOEUD D'INTERFACE)
            iaux=zi(jadr+2*(j-1))
            if (iaux .lt. 0) then
                iaux=abs(iaux)
!           ON RECHERCHE DANS FETI LE NOEUD CORRESPONDANT
                do k = 1, nbfete
!
!             SI LES NOEUDS CORRESPONDENT (JADRI/J, ADDR DE .FETI/J)
                    if (zi(jadri+4*(k-1)) .eq. iaux) then
!               INDICE DANS .FETJ DU PREMIER SD
                        iaux0=zi(jadri+4*(k-1)+3)
!               SD1 ET SD2 DE FETJ
                        iaux1=zi(jadrj-1+iaux0)
                        iaux2=zi(jadrj+iaux0)
!               SI LE SD I EST LE 1ER SD DU NOEUD COURANT
                        nbid=0
                        if (i .eq. iaux1) then
!                   SI LE SD COURANT EST PLUS FAIBLE QUE LE 1ER SD
                            if (i .lt. iaux2) then
                                nbid=-k
                            else
                                nbid=k
                            endif
!               OU SI LE SD I EST LE DEUXIEME SD DU NOEUD COURANT
                        else if (i.eq.iaux2) then
                            if (i .lt. iaux1) then
                                nbid=-k
                            else
                                nbid=k
                            endif
                        endif
!
                        if (nbid .ne. 0) then
                            nn=nn+1
                            iaux3=ialsfg+2*(nn-1)
                            zi(iaux3) = nbid
                            zi(iaux3+1)= j
                        endif
                    endif
                end do
            endif
        end do
!
!***** ON CONSTRUIT EFFECTIVEMENT .FETG
        k24buf=zk24(nomsd-1+i)
        call jecroc(jexnom(nomsdg, k24buf))
        intbuf=2*nn
        call jeecra(jexnom(nomsdg, k24buf), 'LONMAX', intbuf)
        call jeveuo(jexnom(nomsdg, k24buf), 'E', jadr)
        do j = 1, nn
            j2=2*(j-1)
            iaux1=jadr+j2
            iaux2=ialsfg+j2
            zi(iaux1) =zi(iaux2)
            zi(iaux1+1)=zi(iaux2+1)
        end do
!
! FIN BOUCLE SUR LES SD
    end do
    call jedetr('&&FETCRF.LST_FETG ')
    if (niv .ge. 3) call jxveri()
    if (niv .ge. 4) call utimsd(ifm, 2, .false., .true., nomsdg,&
                                1, 'G')
!
!***********************************************************************
!***********************************************************************
!
!       T R A I T E M E N T   D E S   C H A R G E S  A MAILLES ET/OU
!       POINTS TARDIFS
!
!***********************************************************************
!***********************************************************************
!
!     NB NOMBRE DE MAILLES TARDIVES ET DE NOEUDS TARDIFS
    nbmata=0
    nbnota=0
    nomnoe = ma//'.NOMNOE'
!
!***********************************************************************
! BOUCLE 8 CALCUL MAILLES TARDIVES DE TOUTES LES CHARGES
!***********************************************************************
!
    if (nbchar .ge. 1) then
!       TABLE TEMPORAIRE : 2xNB CHARGE PAR SD
        call wkvect('&&FETCRF.L_2CHA_SD', 'V V I', nbsd*2*nbchar, isdlch)
!       LISTE TEMPORAIRE : NB MAILLES TARDIVES PAR CHARGE
        call wkvect('&&FETCRF.L_NBMT_CH', 'V V I', nbsd*2*nbchar, inbmch)
!       LISTE TEMPORAIRE : NOMS DES LIGREL DE CHARGE
        call wkvect('&&FETCRF.LST_CHA  ', 'V V K24', nbchar, ilscha)
!       PREMIERE PASSE : CALCUL DU NB TOTAL DE MAILLES TARDIVES
        do i = 1, nbchar
            ligrch = zk8(nomcha-1+i)//'.CHME.LIGRE'
            zk24(ilscha-1+i)= ligrch
            call jeexin(ligrch//'.NEMA', n2)
            if (n2 .ne. 0) then
                call jelira(ligrch//'.NEMA', 'NUTIOC', nbobj1)
                nbmata=nbmata+nbobj1
                zi(inbmch-1+i)=nbobj1
            endif
        end do
        if (niv .ge. 4) write(ifm,*) 'NBRE MAILLES TARDIVES:', nbmata
    endif
!
    if (niv .ge. 3) call jxveri()
!
! **** IF SUR LES CHARGES A MAILLES TARDIVES
    if ((nbchar.ge.1) .and. (nbmata.gt.0)) then
!
!       CREATION DES OBJETS TEMPORAIRES
! MAILLES TARDIVES TROUVEES PAR SD
! IFLIM  :  MT  1          2          3          4          5 (4+1) ...
!           SD  1 2 3 4    1 2 3 4    1 2 3 4    1 2 3 4    1 2 3 4 ...
!      oui/non  1 0 0 0    1 0 0 0    0 0 1 0    0 0 1 0    1 0 0 0 ...
! NOMBRE DE MAILLES TARDIVES PAR SD
! IDFLIM :  SD  1 2 3 4
!        nb MT  2 0 2 0
        call wkvect('&&FETCRF.FLIM     ', 'V V I', nbsd*nbmata, iflim)
        call wkvect('&&FETCRF.IDFLIM   ', 'V V I', nbsd, idflim)
! NOMBRE DE DDLS SUPPLEMENTAIRES ASSOCIES A UNE MAILLE TARDIVE
!       IFNT   :  MT  1 2 3 4
!             nb NoT  2 2 2 2
!
        call wkvect('&&FETCRF.IFNT     ', 'V V I', nbmata, ifnt)
! NBRE DE MAILLES TARDIVES PAR SD POUR UN CHARGEMENT DONNE
!       IFLII  :  CH  1          2          3       ...
!                 SD  1 2 3 4    1 2 3 4    1 2 3 4 ...
!              nb MT  0 0 0 0    2 0 2 0            ...
        call wkvect('&&FETCRF.FLII     ', 'V V I', nbsd*nbchar, iflii)
!
!       DECALAGE POUR PRENDRE EN COMPTE PLUSIEURS CHARGES
        nb2=0
!       NB DE NOEUDS DE L'INTERFACE QUI SONT SUR DES CHARGEMENTS
        noinch=0
!
!***********************************************************************
! MAXI BOUCLE 9 SUR LES CHARGES
!***********************************************************************
        do ich = 1, nbchar
            ligrch = zk8(nomcha-1+ich)//'.CHME.LIGRE'
            if (niv .ge. 4) then
                write(ifm,*) '--------------------------------------------'
                write(ifm,*) '        CHARGE : ', ich
                write(ifm,*) '--------------------------------------------'
                write(ifm,*) 'LIGRCH:', ligrch
            endif
!
!***** 9.0 LIGREL DE CONTACT OU PAS (DEDUIT DE SURFCL.F)
            lcfc1=.false.
            methco = zk8(nomcha-1+ich)//'.CONTACT.METHCO'
            call jeexin(methco, iret)
            if (iret .ne. 0) then
                call jeveuo(methco, 'L', jmeth)
                nzoco = cfdisi(zk8(nomcha-1+ich)//'.CONTACT','NZOCO')
                iform = cfdisi(zk8(nomcha-1+ich)//'.CONTACT', 'FORMULATION')
!
                do izone = 1, nzoco
                    if (iform .eq. 2) lcfc1=.true.
                end do
            endif
!
! **** 9.1 SI ON TROUVE DES NOEUDS TARDIFS DANS LA CHARGE
            inbno=0
            call jeexin(ligrch//'.NBNO', n1)
            if (n1 .ne. 0) then
                call jeveuo(ligrch//'.NBNO', 'L', iadr)
!               OBJET TEMP POUR STOCKER LES NOEUDS TARDIFS DEJA TRAITES ET
!               AINSI EVITER LES NOEUDS COMPTES 2 FOIS AVEC LIAISON_DDL....
                inbno=zi(iadr)
                if (inbno .ne. 0) then
!                   VECTEUR DES NOEUDS TARDIFS DEJA COMPTES
                    call wkvect('&&FETCRF.LIAISON', 'V V I', inbno, iliais)
                    iaux1=3*inbno
!                   ILIDDL(1,J)=NOEUD PHYSIQUE ASSOCIE AU NOEUD TARDIF
!                   ILIDDL(2,J)= SON NUMERO DE SD;
!                   ILIDDL(3,J)= COMPTEUR (SI > 1 LIAISON_***)
                    call wkvect('&&FETCRF.LIAISONDDL', 'V V I', iaux1, iliddl)
                endif
                icompt=0
            endif
!
! **** 9.2 SI ON TROUVE DES MAILLES TARDIVES DANS LA CHARGE
            call jeexin(ligrch//'.NEMA', n2)
            if (n2 .ne. 0) then
!           NB MAILLES TARDIVES
                call jelira(ligrch//'.NEMA', 'NUTIOC', nbobj1)
!           LONGUEUR TOTALE DE LA COLLECTION
                call jelira(ligrch//'.NEMA', 'LONT', lonli1)
!           ADRESSE DE DEBUT DE LA COLLECTION
                call jeveuo(ligrch//'.NEMA', 'L', idlig1)
!           DIMENSIONNEMENT DES OBJETS TEMPORAIRES
!           LISTE DES MAILLES TARDIVES DE CHAQUE SD
!           ISDMAT(I,J)=1 SI LA MAILLE TARDIVE J APPARTIENT AU SD I
                call wkvect('&&FETCRF.L_MAT_SD ', 'V V I', nbsd*nbobj1, isdmat)
!
! **** 9.3 BOUCLE SUR LES OBJETS DE LA COLLECTION .NEMA
!           POINTEUR VERS LA POSITION DANS LA COLLECTION
                iadr=idlig1
                do j = 1, nbobj1
!             NB NOEUDS DE CHAQUE MAILLE
                    call jelira(jexnum(ligrch//'.NEMA', j), 'LONMAX', n3)
                    n31=n3-1
!             ON PARCOURS LES NOEUDS DE LA MAILLE TARDIVE
                    do k = 1, n31
                        ndtar=zi(iadr-1+k)
!
!               SI C'EST UN NOEUD PHYSIQUE, ON CHERCHE SON(SES) SD
                        if (ndtar .gt. 0) then
                            nb1=0
                            do l = 1, nb
! **** 9.3.1 ON A TROUVE LE NOEUDS PHYSIQUE CORRESPONDANT
                                if (ndtar .eq. zi(ialino-1+l)) then
                                    numsd=zi(linoma-1+l)
                                    dec=(j-1)*nbsd+numsd
                                    nb1=nb1+1
! POUR REMPLISSAGE OBJETS FLIM, FLIN, FLII
! ON RAJOUTE CE TEST SUR NB1 POUR NE PAS DUPLIQUER LES NOEUDS OU MAILLES
! TARDIVES SITUEES SUR L'INTERFACE.
                                    if (nb1 .eq. 1) then
                                        if (zi(isdmat-1+dec) .eq. 0) zi( isdmat-1+dec)=1
                                    endif
!
! REMPLISSAGE OBJET TEMPORAIRE POUR DETECTER LA PRESENCE DE LIAISONS
! TRAVERSANT LES INTERFACES. ON NE S'INTERESSE QU'AUX TRIA3 DANS LA
! CONFIGURATION: NOEUD PHYS NOEUD TARDIF1 NOEUD TARDIF2
! ON NE STOCKE QUE LES INFOS RELATIVES AU NOEUD TARDIF1
                                    iaux1=zi(iadr+1)
                                    iaux3=zi(iadr+2)
! **** 9.3.2 CONFIGURATION TRIA3 QUI NOUS INTERESSE
                                    if ((n3.eq.4) .and. (k.eq.1) .and. ( iaux1.lt.0) .and.&
                                        (iaux3.lt.0)) then
                                        iaux2=iliddl+3*(-iaux1-1)
!                       CAS DU LIAISON INTER-SOUS-DOMAINES
                                        if ((zi(iaux2+1).ne.0) .and.&
                                            ( zi(iaux2+1).ne. numsd) .and. ( nb1.eq.1)) then
                                            call jenuno(jexnum(nomnoe, ndtar), nomn)
                                            call jenuno(jexnum( nomnoe, zi( iaux2)), nomn1)
                                            valk(1)=zk8(nomcha-1+ich)
                                            valk(2)=nomn
                                            valk(3)=nomn1
                                            call utmess('F', 'ELEMENTS_64', nk=3, valk=valk)
                                        endif
                                        if (zi(iaux2+2) .lt. 2) then
                                            zi(iaux2)=ndtar
                                            zi(iaux2+1)=numsd
                                            zi(iaux2+2)=nb1
                                        endif
                                    endif
                                endif
                            end do
!
!                 .AJOUT DU NOEUD CHARGE SUR L'INTERFACE DANS LA LISTE
                            if (nb1 .gt. 1) then
                                noinch=noinch+1
                                if (niv .ge. 2) then
                                    call jenuno(jexnum(nomnoe, ndtar), nomn)
                                    write(ifm,*)'FETCRF, LE NOEUD '//&
                                    nomn//' DU '// 'CHARGEMENT '//zk8(&
                                    nomcha-1+ich)//' EST SUR '//&
                                    'L''INTERFACE.'
                                endif
                            endif
!
!               SINON C'EST UN NOEUD TARDIF (NDTAR)
                        else
! **** 9.3.3 BOUCLE SUR LES NOEUDS TARDIFS DEJA COMPTES
                            do ll = 1, icompt
                                if (ndtar .eq. zi(iliais-1+ll)) then
! TEST SUPPLEMENTAIRE POUR SAVOIR SI IL EST CONCERNE PAR UN LIGREL DE
! CHARGE TOUCHANT L'INTERFACE
                                    iaux0=iliddl+3*(-ndtar-1)
                                    if (zi(iaux0+2) .gt. 1) then
                                        call jenuno(jexnum(nomnoe, zi( iaux0)), nomn)
                                        valk(1)=zk8(nomcha-1+ich)
                                        valk(2)=nomn
                                        call utmess('F', 'ELEMENTS_65', nk=2, valk=valk)
                                    endif
                                    goto 206
                                endif
                            end do
                            zi(ifnt-1+j+nb2)=zi(ifnt-1+j+nb2)+1
                            nbnota=nbnota+1
                            icompt=icompt+1
                            zi(iliais-1+icompt)=ndtar
206                         continue
                        endif
!             FIN BOUCLE SUR LES MAILLES TARDIVES
                    end do
!             POSITION DE LA MAILLE TARDIVE SUIVANTE DANS LA COLLECTION
                    iadr=iadr+n3
                end do
!
! **** 9.4 MONITORING
                if (niv .ge. 5) then
                    write(ifm,*) '----------'
                    do j = 1, nbobj1
                        do numsd = 1, nbsd
                            dec=(j-1)*nbsd+numsd
                            write(ifm,*) 'SD:',numsd,' MT:',j,' VAL:',&
                            zi(isdmat-1+dec)
                        end do
                    end do
                    write(ifm,*) '----------'
                    do numsd = 1, nbsd
                        do j = 1, nbobj1
                            dec=(j-1)*nbsd+numsd
                            write(ifm,*) 'MT:',j,' SD:',numsd,' VAL:',&
                            zi(isdmat-1+dec)
                        end do
                    end do
                    write(ifm,*) '----------', nb2
                endif
!
! **** 9.5 MISE A JOUR DE L'OBJET TEMPORAIRE POUR FLIM
                do j = 1, nbobj1
                    do numsd = 1, nbsd
                        dec=(j-1)*nbsd+numsd
                        nval=zi(isdmat-1+dec)
!               LOGICAL=1 SI LA MAILLE TARDIVE J A UN POINT TARDIF DANS
!               LE SD NUMSD. EN CAS DE MULTIPLICITE, C'EST LE SD DE
!               PLUS PETIT NUMERO QUI L'EMPORTE
                        if (nval .gt. 0) then
!                 MAJ de IDFLIM (NB DE MAILLES TARDIVES TROUVEES PAR SD)
                            zi(idflim-1+numsd)=zi(idflim-1+numsd)+1
!                 MAJ DE IFLIM (NUM DE MAILLES TARDIVES TROUVEES PAR SD)
                            dec=(j-1 + nb2)*nbsd+ numsd
                            zi(iflim-1+dec) = j
!                 MAJ DE IFLII (NB DE MAILLES TARDIVES TROUVEES PAR SD)
                            dec=(ich-1)*nbsd+numsd
                            zi(iflii-1+ dec) = zi(iflii-1+ dec) + 1
                        endif
                    end do
                end do
!           DECALAGE POUR IFLIM (LES NOUVELLES MaT COMMENCENT A LA FIN)
                nb2=nb2+nbobj1
                call jedetr('&&FETCRF.L_MAT_SD')
!         FIN SI ON A TROUVE DES MAILLES TARDIVES DANS CETTE CHARGE...
            endif
!
!         FIN BOUCLE DE 1 A NBCHAR
            if (inbno .ne. 0) then
                call jedetr('&&FETCRF.LIAISON')
                call jedetr('&&FETCRF.LIAISONDDL')
            endif
        end do
!
!***********************************************************************
! FIN MAXI BOUCLE 9 SUR LES CHARGES
!***********************************************************************
!
!***********************************************************************
! ETAPE 10 MONITORING
!***********************************************************************
        if (niv .ge. 4) then
            write(ifm,*) ' '
            write(ifm,*) '----------------------------------------------'
            write(ifm,*) '         FIN DES CHARGES'
            write(ifm,*) '----------------------------------------------'
            write(ifm,*) ' '
            write(ifm,*) 'NB1=',nb1
            write(ifm,*) 'NB2=',nb2
            write(ifm,*) 'NBMATA=',nbmata
            write(ifm,*) 'NBSD=',nbsd
            write(ifm,*) 'NBCHAR=',nbchar
!
            write(ifm,*) '--------------------------'
            do j = 1, nbmata
                write(ifm,*) ' '
                do numsd = 1, nbsd
                    dec=(j-1)*nbsd+numsd
                    write(ifm,*) 'SD:',numsd,' MT:',j,' IFLIM:',zi(&
                    iflim-1+dec)
                end do
            end do
            write(ifm,*) '--------------------------'
            do k = 1, nbsd*nbmata
                write(ifm,*) 'IFLIM:',zi(iflim-1+k)
            end do
            write(ifm,*) '--------------------------'
            do ich = 1, nbchar
                do isd = 1, nbsd
                    dec=(ich-1)*nbsd+isd
                    write(ifm,*) 'CH:',ich,' SD:',isd,' IFLII:',zi(&
                    iflii-1+dec)
                end do
            end do
            write(ifm,*) '--------------------------'
            do k = 1, nbsd
                write(ifm,*) 'SD:',k,' IDFLIM:',zi(idflim-1+ k)
            end do
            write(ifm,*) '--------------------------'
            do k = 1, nbchar
                write(ifm,*) 'CHA:',zk24(ilscha-1+ k)
            end do
            write(ifm,*) '--------------------------'
            do k = 1, nbmata
                write(ifm,*) 'MAT',k,' NBNOT:',zi(ifnt-1+k)
            end do
            write(ifm,*) '--------------------------'
        endif
! **** FIN IF SUR LES CHARGES A MAILLES TARDIVES
    endif
    if (niv .ge. 3) call jxveri()
!***********************************************************************
! ETAPE 11 CREATION .FDIM
!***********************************************************************
!
    call wkvect(nomsdm, 'G V I', 5, jadr)
    zi(jadr)=nbsd
    zi(jadr+1)=nbfete
    zi(jadr+2)=nbmatr
    zi(jadr+3)=nbddli
    zi(jadr+4)=nbnoto
!
    if (niv .ge. 3) call jxveri()
    if (niv .ge. 4) call utimsd(ifm, 2, .false., .true., nomsdm,&
                                1, 'G')
!***********************************************************************
! ETAPE 12 CREATION .FETH
!***********************************************************************
!     PRISE EN COMPTE DES NOEUDS PHYSIQUES
    call wkvect(nomsdh, 'G V I', nbsd, jadrh)
    do isd = 1, nbsd
        zi(jadrh+isd-1)=zi(ialsk-1+isd)
    end do
!
!     DES NOEUDS TARDIFS
    if (nbnota .gt. 0) then
        do j = 1, nbmata
            do numsd = 1, nbsd
                dec=(j-1)*nbsd+numsd
                if (zi(iflim-1+dec) .gt. 0) then
                    zi(jadrh-1+numsd)=zi(jadrh-1+numsd) + zi(ifnt-1+j)
                endif
            end do
        end do
    endif
!
    if (niv .ge. 3) call jxveri()
    if (niv .ge. 4) call utimsd(ifm, 2, .false., .true., nomsdh,&
                                1, 'G')
!***********************************************************************
! ETAPE 13 CREATION .FLII/.FLIM/.FLIN
!***********************************************************************
!
!     CREATION DES TROIS COLLECTIONS
    call jecrec(nomsln, 'G V K24', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsd)
    call jecrec(nomsli, 'G V I', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsd)
    call jecrec(nomslm, 'G V I', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsd)
!
!     ON NE REMPLIT CES COLLECTIONS QUE SI ON A DES MAILLES TARDIVES
    if (nbmata .gt. 0) then
!
!       VECTEURS TEMPORAIRES CONTENANT LES TAILLES DES COLLECTIONS FLIx
        call wkvect('&&FETCRF.DIM_FLII ', 'V V I', nbsd, idflii)
        call wkvect('&&FETCRF.DIM_FLIM ', 'V V I', nbsd, idflm)
        call wkvect('&&FETCRF.DIM_FLIN ', 'V V I', nbsd, idfln)
!       NBRE DE MAILLES TARDIVES POUR UN CHARGEMENT DONNE
        call wkvect('&&FETCRF.TEMP     ', 'V V I', nbchar, itmp)
!
! ****  13.1 PREMIER PASSAGE : RECUPERATION DE LA TAILLE DES COLLECTIONS
        do ich = 1, nbchar
!         NBRE DE MAILLES TARDIVES DU CHARGEMENT ICH
            nn = 0
            do isd = 1, nbsd
                dec=(ich-1)*nbsd+isd
                nn=nn+zi(iflii-1+dec)
            end do
            zi(itmp+ich-1)=nn
!         SI LA CHARGE COURANTE A DES MAILLES TARDIVES
            if (nn .gt. 0) then
                do isd = 1, nbsd
                    dec=(ich-1)*nbsd+isd
!             SI LA CHARGE COURANTE A DES MAILLES TARDIVES DANS LE SD
!             ON INCREMENTE LES TAILLES DES 3 COLLECTIONS POUR LE SD
                    if (zi(iflii-1+ dec) .gt. 0) then
                        zi(idfln-1+isd)=zi(idfln-1+isd)+1
                        zi(idflii-1+isd)=zi(idflii-1+isd)+2
                        zi(idflm-1+isd)=zi(idflm-1+isd)+zi(iflii-1+&
                        dec)
                    endif
                end do
            endif
        end do
!
!       VECTEURS DE POINTEUR VERS L'ADDRESSE JEVEUX DES OBJETS DE COLL.:
!       CA VAUT 0 SI L'OBJET N'A PAS ENCORE ETE CREE
!       SINON CA VAUT L'ADRESSE MEMOIRE COURANTE POUR CHAQUE SD
        call wkvect('&&FETCRF.BIM_FLII ', 'V V I', nbsd, biflii)
        call wkvect('&&FETCRF.BIM_FLIM ', 'V V I', nbsd, biflim)
        call wkvect('&&FETCRF.BIM_FLIN ', 'V V I', nbsd, bifln)
!
! **** 13.2 DEUXIEME PASSAGE : REMPLISSAGE DES COLLECTIONS FLII ET FLIN
        do ich = 1, nbchar
!         NN = NB DE MAILLES TARDIVE DE LA CHARGE
            nn = zi(itmp+ich-1)
!         SI LA CHARGE COURANTE A DES MAILLES TARDIVES
            if (nn .gt. 0) then
                do isd = 1, nbsd
                    dec=(ich-1)*nbsd+isd
!             SI LA CHARGE COURANTE A DES MAILLES TARDIVES DANS LE SD
                    if (zi(iflii-1+dec) .gt. 0) then
!
! **** 13.2.1   CREATION DE .FLIN
                        n1 = zi(idfln-1+isd)
                        if (n1 .gt. 0) then
                            if (zi(bifln-1+isd) .eq. 0) then
                                k24buf=zk24(nomsd-1+isd)
                                call jecroc(jexnom(nomsln, k24buf))
                                call jeecra(jexnom(nomsln, k24buf), 'LONMAX', n1)
                                call jeveuo(jexnom(nomsln, k24buf), 'E', jadr)
!                   ADDR DE DEPART POUR FLIN/ISD
                                zi(bifln-1+isd)=jadr
                            endif
!                 ADDR COURANTE POUR FLIN/ISD
                            jadr=zi(bifln-1+isd)
!                 REMPLISSAGE DE FLIN/ISD
                            zk24(jadr)=zk24(ilscha-1+ich)
!                 ADDR COURANTE POUR FLIN/ISD INCREMENTEE DE 1
                            zi(bifln-1+isd)=jadr+1
                        endif
!
! **** 13.2.2   CREATION DE .FLII
                        n1 = zi(idflii-1+isd)
                        if (n1 .gt. 0) then
                            if (zi(biflii-1+isd) .eq. 0) then
                                k24buf=zk24(nomsd-1+isd)
                                call jecroc(jexnom(nomsli, k24buf))
                                call jeecra(jexnom(nomsli, k24buf), 'LONMAX', n1)
                                call jeveuo(jexnom(nomsli, k24buf), 'E', jadr)
!                   ADDR DE DEPART POUR FLII/ISD
                                zi(biflii-1+isd)=jadr
                            endif
!                 ADDR COURANTE POUR FLII/ISD
                            jadr=zi(biflii-1+isd)
!                 REMPLISSAGE DE FLII/ISD
                            zi(jadr)=nn
                            zi(jadr+1)=zi(iflii-1+dec)
!                 ADDR COURANTE POUR FLII/ISD INCREMENTEE DE 2
                            zi(biflii-1+isd)=jadr+2
                        endif
!
! **** 13.2.3   CREATION DE .FLIM
                        n1 = zi(idflm-1+isd)
                        if (n1 .gt. 0) then
                            if (zi(biflim-1+isd) .eq. 0) then
                                k24buf=zk24(nomsd-1+isd)
                                call jecroc(jexnom(nomslm, k24buf))
                                call jeecra(jexnom(nomslm, k24buf), 'LONMAX', n1)
                                call jeveuo(jexnom(nomslm, k24buf), 'E', jadr)
!                   ADDR DE DEPART POUR FLIM/ISD
                                zi(biflim-1+isd)=jadr
                            endif
                        endif
!             FIN SI LA CHARGE EST CONCERNEE PAR LE SD
                    endif
!           FIN BOUCLE SUR LES SD
                end do
!         FIN SI LA CHARGE COURANTE A DES MAILLES TARDIVES
            endif
!       FIN BOUCLE SUR LES CHARGES
        end do
!
! **** 13.3 TROISIEME PASSAGE : REMPLISSAGE DE LA COLLECTION FLIM
        do j = 1, nbmata
            do numsd = 1, nbsd
                dec=(j-1)*nbsd+numsd
                if (zi(iflim-1+dec) .gt. 0) then
!             ADDR COURANTE POUR FLIM/ISD
                    jadr=zi(biflim-1+numsd)
!             REMPLISSAGE DE FLIM/ISD
                    zi(jadr)=zi(iflim-1+dec)
!             ADDR COURANTE POUR FLIM/ISD INCREMENTEE DE 1
                    jadr=jadr+1
                    zi(biflim-1+numsd)=jadr
                endif
            end do
        end do
!
!     FIN DE CREATION DE FLII, FLIM ET FLIN, IF (NBNOTA.GT.0)
    endif
    if (niv .ge. 3) call jxveri()
    if (niv .ge. 4) then
        call utimsd(ifm, 2, .false., .true., nomsln,&
                    1, 'G')
        call utimsd(ifm, 2, .false., .true., nomsli,&
                    1, 'G')
        call utimsd(ifm, 2, .false., .true., nomslm,&
                    1, 'G')
    endif
!
!***********************************************************************
! MAXI BOUCLE 14 SUR LES CHARGES POUR LE CONTACT CONTINUE (CF. SURFCL.F)
!***********************************************************************
!     ON NE RETESTE PAS L'HOMOGENIEITE DES ZONES DE CONTACT EN CONTACT
!     CONTINUE ET LEUR NON JUXTAPOSITION AVEC UNE ZONE DE DIRICHLET OU
!     DE FORCE NODALE DANS UN MEME AFFE_CHAR_MECA. CELA A ETE DEJA FAIT
!     PRECEDEMMENT.
!
! VECTEURS DE NBRE DE MAILLES/NOEUDS ESCLAVES PAR SD
    call wkvect('&&FETCRF.FCFM     ', 'V V I', nbsd, ifcfm)
    call jerazo('&&FETCRF.FCFM     ', nbsd, 1)
    call wkvect('&&FETCRF.FCFN     ', 'V V I', nbsd, ifcfn)
    call jerazo('&&FETCRF.FCFN     ', nbsd, 1)
! VECTEURS AUXILIAIRES DE POINTEURS DE CONTACT
    if (nbchar .ne. 0) call wkvect('&&FETCRF.FCFB     ', 'V V I', 9*nbchar, ifcfb)
!
! **** 14.1 BOUCLE POUR DETERMINER LE NOMBRE DE ZONES MAXI DE CONTACT
!  ET LES POINTEURS DE CONTACT ADHOC
    nzocom=0
    do ich = 1, nbchar
        nocha1=zk8(nomcha-1+ich)
        methco = nocha1//'.CONTACT.METHCO'
        pzone = nocha1//'.CONTACT.PZONECO'
        psurma = nocha1//'.CONTACT.PSUMACO'
        psurno = nocha1//'.CONTACT.PSUNOCO'
        contma = nocha1//'.CONTACT.MAILCO'
        contno = nocha1//'.CONTACT.NOEUCO'
        ndimco = nocha1//'.CONTACT.NDIMCO'
!
        call jeexin(methco, iret)
        zi(ifcfb+9*(ich-1))=0
        if (iret .ne. 0) then
            zi(ifcfb+9*(ich-1))=1
            call jeveuo(methco, 'L', jmeth)
            zi(ifcfb+9*(ich-1)+1)=jmeth
            call jeveuo(pzone, 'L', jzone)
            zi(ifcfb+9*(ich-1)+2)=jzone
            call jeveuo(psurma, 'L', jsuma)
            zi(ifcfb+9*(ich-1)+3)=jsuma
            call jeveuo(psurno, 'L', jsuno)
            zi(ifcfb+9*(ich-1)+4)=jsuno
            call jeveuo(contma, 'L', jmaco)
            zi(ifcfb+9*(ich-1)+5)=jmaco
            nzoco = cfdisi(k8bid//'.CONTACT','NZOCO')
!
            zi(ifcfb+9*(ich-1)+6)=nzoco
            if (nzoco .gt. nzocom) nzocom=nzoco
            call jeveuo(contno, 'L', jnoco)
            zi(ifcfb+9*(ich-1)+7)=jnoco
            call jeveuo(ndimco, 'L', jdim)
            zi(ifcfb+9*(ich-1)+8)=jdim
        endif
    end do
! VECTEUR INDIQUANT LE NUMERO DE SD CONCERNE PAR LA JIEME ZONE
! DU IEME CHARGEMENT
!     CHAR     1        2        3...
!     ZONE     1 2 0 0  0 1 0 0
    iaux0=nzocom*nbchar
    if (iaux0 .ne. 0) call wkvect('&&FETCRF.FCFL', 'V V I', iaux0, ifcfl)
! NOMBRE TOTAL DE MAILLES DE CONTACT ESCLAVES PAR CHARGEMENT
    if (nbchar .gt. 0) call wkvect('&&FETCRF.FCNM', 'V V I', nbchar, ifcnm)
!
! **** 14.2 BOUCLE SUR LES CHARGEMENTS POUR DETERMINER LES COUPLES
! (SD,CHAR), LES NOMBRES DE MAILLES ET NOEUDS MAITRES PAR SD
    do ich = 1, nbchar
        if (zi(ifcfb+9*(ich-1)) .eq. 1) then
            jmeth=zi(ifcfb+9*(ich-1)+1)
            jzone=zi(ifcfb+9*(ich-1)+2)
            jsuma=zi(ifcfb+9*(ich-1)+3)
            jsuno=zi(ifcfb+9*(ich-1)+4)
            jmaco=zi(ifcfb+9*(ich-1)+5)
            nzoco=zi(ifcfb+9*(ich-1)+6)
            isuco = 0
! BOUCLE SUR LES ZONES DE CONTACT
            do izone = 1, nzoco
                nbsurf = zi(jzone+izone) - zi(jzone+izone-1)
                do isurf = 1, nbsurf
! CETTE INITIALISATION PERMET DE TESTER SI TOUTES LES MAILLES D'UNE ZONE
! APPARTIENNENT A UN SEUL SOUS-DOMAINE
                    cfsd=-1
! ISUCO PILOTE LE CHANGEMENT DE SURFACE: 1=ESCLAVE, 2=MAITRE
                    isuco = isuco + 1
                    nbma = zi(jsuma+isuco) - zi(jsuma+isuco-1)
                    nbno = zi(jsuno+isuco) - zi(jsuno+isuco-1)
                    jdecma = zi(jsuma+isuco-1)
                    jdecno = zi(jsuno+isuco-1)
!
                    zi(ifcnm+ich-1)=zi(ifcnm+ich-1)+nbma
! BOUCLE SUR SES MAILLES
                    do ima = 1, nbma
                        numma = zi(jmaco+jdecma+ima-1)
! BOUCLE SUR LES SOUS-DOMAINES
                        do idd = 1, nbsd
! ILS ONT DES MAILLES DE BORD OU PAS ?
                            if (zi(nbrd-1+idd) .eq. 1) then
                                nomgma=zk24(lstbrd-1+idd)
                                call jelira(jexnom(grpma, nomgma), 'LONUTI', nbmabo)
                                call jeveuo(jexnom(grpma, nomgma), 'L', ialibd)
                                nbmabo=nbmabo-1
! BOUCLE SUR LES MAILLES DE BORD
                                do j = 0, nbmabo
                                    if (zi(ialibd+j) .eq. numma) then
                                        cfsd=idd
                                        goto 952
                                    endif
                                end do
                            endif
! SORTIE DU IF; ON A TROUVE QUE LA MAILLE DE CONTACT NUMMA EST CONCERNEE
! PAR LE SD IDD. ON VA VERIFIER QU'ELLE NE L'EST PAS AUCUN AUTRE SD
952                         continue
                        end do
                    end do
!
! SI UNE SURFACE DE CONTACT N'A TROUVE AUCUN SD, UTMESS_F
                    if (cfsd .gt. 0) then
                        zi(ifcfl+(ich-1)*nzocom+izone)=cfsd
                        zi(ifcfm+cfsd-1)=zi(ifcfm+cfsd-1)+nbma
                        zi(ifcfn+cfsd-1)=zi(ifcfn+cfsd-1)+nbno
                    endif
! FIN BOUCLE SUR LES SURFACES
                end do
! FIN BOUCLE SUR LES ZONES DE CONTACT
            end do
        endif
! FIN BOUCLE SUR LES CHARGES
    end do
!
! CREATION OBJETS .FCFL, .FCFI, .FCFM ET .FCFN POUR FETI+CONTACT (inutiles => a supprimer)
    call jecrec(nomfcl, 'G V K24', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsd)
    call jecrec(nomfci, 'G V I', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsd)
    call jecrec(nomfcm, 'G V I', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsd)
    call jecrec(nomfcn, 'G V I', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsd)
!
! **** 14.3 ON REPARCOURT LES CHARGES PAR SD CETTE FOIS POUR REMPLIR
! LES OBJETS PRECEDENTS
    do idd = 1, nbsd
! ON CHERCHE A DIMENSIONNER L'OBJET IDD DE NOMFCL
! IAUX: NOMBRE DE CHARGEMENT DE CONTACT CONCERNANT IDD
        iaux=0
        do ich = 1, nbchar
            do izone = 1, nzocom
                if (zi(ifcfl+(ich-1)*nzocom+izone) .eq. idd) then
                    iaux=iaux+1
                    goto 966
                endif
            end do
966         continue
        end do
        k24buf=zk24(nomsd-1+idd)
        if (iaux .ne. 0) then
! TRAVAIL PREPARATOIRE 1 POUR CALCULER DDLS DE CONTACT SUPPLEMENTAIRES
            call jelira(jexnom(nomsdb, k24buf), 'LONMAX', lfetb)
            lfetb=lfetb/2
            call jeveuo(jexnom(nomsdb, k24buf), 'L', ifetb)
! CREATION NOMFCL
            call jecroc(jexnom(nomfcl, k24buf))
            call jeecra(jexnom(nomfcl, k24buf), 'LONMAX', iaux)
            call jeveuo(jexnom(nomfcl, k24buf), 'E', jadr)
            iiaux1=0
            iaux1=0
! CREATION NOMFCI
            call jecroc(jexnom(nomfci, k24buf))
            call jeecra(jexnom(nomfci, k24buf), 'LONMAX', 2*iaux)
            call jeveuo(jexnom(nomfci, k24buf), 'E', madr)
! CREATION NOMFCM
            call jecroc(jexnom(nomfcm, k24buf))
            call jeecra(jexnom(nomfcm, k24buf), 'LONMAX', zi(ifcfm+idd-1))
            call jeveuo(jexnom(nomfcm, k24buf), 'E', kadr)
            iaux2=0
! CREATION NOMFCN
            call jecroc(jexnom(nomfcn, k24buf))
            call jeecra(jexnom(nomfcn, k24buf), 'LONMAX', zi(ifcfn+idd-1))
            call jeveuo(jexnom(nomfcn, k24buf), 'E', ladr)
            iaux3=0
            do ich = 1, nbchar
                if (zi(ifcfb+9*(ich-1)) .eq. 1) then
!
                    nzoco=zi(ifcfb+9*(ich-1)+6)
                    jzone=zi(ifcfb+9*(ich-1)+2)
! BOUCLE SUR LES ZONES DE CONTACT
! INDDZ: PERMET DE DECIDER SI UN NOEUD COMPTE PLUSIEURS FOIS EST AU SEIN
! D'UNE MEME ZONE OU NON
                    inddz=0
                    isuco= 0
                    lpaire=.true.
                    do izone = 1, nzoco
                        nbsurf = zi(jzone+izone) - zi(jzone+izone-1)
                        do isurf = 1, nbsurf
! ISUCO PILOTE LE CHANGEMENT DE SURFACE: IMPAIRE=ESCLAVE, PAIRE=MAITRE
                            isuco = isuco + 1
                            lpaire=.not.lpaire
                            if (zi(ifcfl+(ich-1)*nzocom+izone) .eq. idd) then
! ON STOCKE LE NOM DE LA CHARGE
                                k8bid=zk8(nomcha-1+ich)
                                do i = 1, iaux1
                                    if (zk24(jadr+i-1)(1:8) .eq. k8bid) then
                                        iiaux1=i
                                        goto 964
                                    endif
                                end do
                                zk24(jadr+iaux1)=k8bid//'.CHME.LIGRE'
                                iiaux1=iaux1+1
                                iaux1=iaux1+1
! LABEL POUR NE PAS ENREGISTRER PLUSIEURS FOIS LE NOM DE LA CHARGE
964                             continue
! TRAVAIL PREPARATOIRE 2 POUR CALCULER DDLS DE CONTACT SUPPLEMENTAIRES
                                k24bid=k8bid//'.CHME.LIGRE.PRNM'
                                call jeveuo(k24bid, 'L', jprnm)
!
                                jmeth=zi(ifcfb+9*(ich-1)+1)
                                jsuma=zi(ifcfb+9*(ich-1)+3)
                                jsuno=zi(ifcfb+9*(ich-1)+4)
                                jmaco=zi(ifcfb+9*(ich-1)+5)
                                jnoco=zi(ifcfb+9*(ich-1)+7)
                                jdim =zi(ifcfb+9*(ich-1)+8)
                                nbma = zi(jsuma+isuco) - zi(jsuma+ isuco-1)
                                nbno = zi(jsuno+isuco) - zi(jsuno+ isuco-1)
                                jdecma = zi(jsuma+isuco-1)
                                jdecno = zi(jsuno+isuco-1)
! ON REMPLI .FCFM POUR IDD
                                do ima = 1, nbma
                                    zi(kadr+iaux2+ima-1)=zi(jmaco+&
                                    jdecma+ima-1)
                                end do
                                iaux2=iaux2+nbma
! ON REMPLI .FCFI POUR IDD
                                zi(madr+2*(iiaux1-1))=zi(ifcnm+ich-1)
                                zi(madr+2*(iiaux1-1)+1)=zi(madr+2*(&
                                iiaux1-1)+1)+nbma
! ON REMPLI .FCFN POUR IDD (EN EVITANT LES DOUBLONS POUR LES DDLS)
! ON NE COMPTE QU'UNE FOIS UN NOEUD COMMUN AUX DEUX PARTIES D'UNE
! MEME ZONE OU A PLUSIEURS ZONES
                                do ino = 1, nbno
                                    j2=iaux3+ino-2
                                    jj=zi(jnoco+jdecno+ino-1)
                                    zi(ladr+j2+1)=jj
                                    do j = 1, nbfete
                                    end do
                                    do j = 0, j2
                                        if (zi(ladr+j) .eq. jj) then
! NOEUD COMPTE DEUX FOIS
                                            vali(1)=i
                                            vali(2)=izone
                                            if (j .ge. inddz) then
                                                goto 869
                                            endif
                                        endif
                                    end do
! POUR LES NOEUDS ESCLAVES (ISUCO IMPAIR)
! ON DETERMINE LE NOMBRE DE DDL DUS AU CONTACT SANS LES DDLS
! PHYSIQUES DEJA COMPTES
                                    ddlm=0
                                    k=0
                                    if (lpaire) then
                                        do l = 1, lfetb
                                            if (abs(zi(ifetb+2*(l-1))) .eq. jj) then
                                                if (l .eq. 1) then
                                                    ddlm=zi(ifetb+1)
                                                else
                                                    ddlm=zi(ifetb+2*(l-1)+1)-&
                                        zi(ifetb+2*(l-2)+1)
                                                endif
                                                goto 669
                                            endif
                                        end do
669                                     continue
                                        k=-ddlm
                                        do l = 1, nec30
                                            if (exisdg( zi(jprnm-1+nec*(jj- 1)+1),l)) k=k+1
                                        end do
! MAJ DE .FETH EN NE TENANT COMPTE QUE DES DDLS DE CONTACT
                                        zi(jadrh-1+idd)=zi(jadrh-1+&
                                        idd)+k
                                    endif
!
! MONITORING
!                    WRITE(IFM,*)'NOEUD/DDL CONTACT',JJ,DDLM,K
869                                 continue
! FIN BOUCLE SUR LES NOEUDS
                                end do
                                iaux3=iaux3+nbno
! FIN SI ZONE CONCERNANT LE SD
                            endif
! FIN BOUCLE SUR LES SURFACES
                        end do
! FIN BOUCLE SUR LES ZONES
                        inddz=inddz+iaux3
                    end do
! FIN SI CHARGEMENT DE CONTACT
                endif
! FIN BOUCLE SUR LES CHARGES
            end do
! SI SD CONCERNE PAR CONTACT
        endif
! FIN BOUCLE SUR LES SD
    end do
!
!***********************************************************************
! ETAPE 15 DESTRUCTION OBJETS TEMPORAIRES
!***********************************************************************
    call jedetc('V', '&&FETCRF', 1)
    call jedema()
end subroutine
