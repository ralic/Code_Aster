subroutine crsvfe(motfac, solveu, istop, nprec, syme,&
                  epsmat, mixpre, kmd)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/creso1.h"
#include "asterfort/fetmpi.h"
#include "asterfort/gcncon.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jevtbl.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
    integer :: istop, nprec
    real(kind=8) :: epsmat
    character(len=3) :: syme, mixpre, kmd
    character(len=16) :: motfac
    character(len=19) :: solveu
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
! ----------------------------------------------------------
!  BUT : REMPLISSAGE SD_SOLVEUR FETI (NIVEAU 1)
!
! IN K19 SOLVEU  : NOM DU SOLVEUR DONNE EN ENTREE
! OUT    SOLVEU  : LE SOLVEUR EST CREE ET INSTANCIE
! IN  IN ISTOP   : PARAMETRE LIE AUX MOT-CLE STOP_SINGULIER
! IN  IN NPREC   :                           NPREC
! IN  K3 SYME    :                           SYME
! IN  R8 EPSMAT  :                           FILTRAGE_MATRICE
! IN  K3 MIXPRE  :                           MIXER_PRECISION
! IN  K3 KMD     :                           MATR_DISTRIBUEE
! ----------------------------------------------------------
!
!
!
!
    integer :: nmaxit, ibid, niremp, ilimpi, ifets, ifm, inumsd, niv, nbsd, i
    integer :: nbma, idime, nbreor, nmaxi1, nbreo1, ifref, imail, iinf, nbsd1
    integer :: nbproc, nivmpi, rang, nbreoi, nbreo2, ibid1, ibid2, ibid3, ibid4
    integer :: ibid5, ibid6, ifetpt, imsmi, reacre, reacr1, imsmk
    real(kind=8) :: resire, tbloc, eps, testco, resir1, testc1, rbid
    character(len=8) :: renum, preco, k8bid, verif
    character(len=8) :: tyreor, scalin, preco1, verif1, tyreo1, scali1, modele
    character(len=8) :: stogi, acsm, acsm1, acma, acma1
    character(len=16) :: metho1, method
    character(len=19) :: solfeb
    character(len=24) :: sdfeti, masd, infofe, k24bid
    logical :: testok
    integer :: iarg
!------------------------------------------------------------------
    call jemarq()
!
! --- INITS.
    preco='SANS'
    preco1='????'
    resire=1.d-6
    resir1=0.d0
    eps=0.d0
    nmaxit=0
    nmaxi1=0
    niremp=0
    tbloc=jevtbl('TAILLE_BLOC')
    sdfeti='????'
    verif='????'
    verif1='????'
    testco=0.d0
    testc1=0.d0
    nbreor=0
    nbreo1=0
    nbreoi=0
    nbreo2=0
    tyreor='SANS'
    tyreo1='????'
    scalin='SANS'
    scali1='????'
    stogi='????'
    acsm='NON'
    acsm1='????'
    acma='NON'
    acma1='????'
    reacre=0
    reacr1=0
    call infniv(ifm, niv)
!
!
! --- PARAMETRE FETI IDENTIQUE A CELUI DE MULT_FRONT ET HOMOGENE POUR
!     CHAQUE SOUS-DOMAINE
    call getvtx(motfac, 'RENUM', iocc=1, scal=renum, nbret=ibid)
    ASSERT(ibid.eq.1)
!
! --- LECTURES PARAMETRES DEDIES AU SOLVEUR
    sdfeti=' '
    call getvid(motfac, 'PARTITION', iocc=1, scal=sdfeti(1:8), nbret=ibid)
    ASSERT(ibid.eq.1)
    call getvis(motfac, 'NMAX_ITER', iocc=1, scal=nmaxit, nbret=ibid)
    ASSERT(ibid.eq.1)
    call getvis(motfac, 'REAC_RESI', iocc=1, scal=reacre, nbret=ibid)
    ASSERT(ibid.eq.1)
    call getvr8(motfac, 'RESI_RELA', iocc=1, scal=resire, nbret=ibid)
    ASSERT(ibid.eq.1)
    call getvtx(motfac, 'VERIF_SDFETI', iocc=1, scal=verif, nbret=ibid)
    ASSERT(ibid.eq.1)
    call getvr8(motfac, 'TEST_CONTINU', iocc=1, scal=testco, nbret=ibid)
    ASSERT(ibid.eq.1)
    call getvtx(motfac, 'TYPE_REORTHO_DD', iocc=1, scal=tyreor, nbret=ibid)
    ASSERT(ibid.eq.1)
    if (tyreor(1:4) .ne. 'SANS') then
        call getvis(motfac, 'NB_REORTHO_DD', iocc=1, scal=nbreor, nbret=ibid)
        ASSERT(ibid.eq.1)
        call getvtx(motfac, 'ACCELERATION_SM', iocc=1, scal=acsm, nbret=ibid)
        ASSERT(ibid.eq.1)
        if (acsm(1:3) .eq. 'OUI') then
            call getvis(motfac, 'NB_REORTHO_INST', iocc=1, scal=nbreoi, nbret=ibid)
            ASSERT(ibid.eq.1)
        else
            nbreoi=0
        endif
    else
        nbreor=0
        acsm(1:3)='NON'
        nbreoi=0
    endif
    call getvtx(motfac, 'PRE_COND', iocc=1, scal=preco, nbret=ibid)
    ASSERT(ibid.eq.1)
    if (preco(1:4) .ne. 'SANS') then
        call getvtx(motfac, 'SCALING', iocc=1, scal=scalin, nbret=ibid)
        ASSERT(ibid.eq.1)
    else
        scalin(1:4)='SANS'
    endif
    call getvtx(motfac, 'STOCKAGE_GI', iocc=1, scal=stogi, nbret=ibid)
    ASSERT(ibid.eq.1)
!
! --- OBJET TEMPORAIRE POUR MONITORING FETI
    infofe='FFFFFFFFFFFFFFFFFFFFFFFF'
    call getvtx(motfac, 'INFO_FETI', iocc=1, scal=infofe, nbret=ibid)
    ASSERT(ibid.eq.1)
    call wkvect('&FETI.FINF', 'V V K24', 1, iinf)
    zk24(iinf)=infofe
!
!     LECTURE NOMBRE DE SOUS-DOMAINES:NBSD
    call jelira(sdfeti(1:19)//'.FETA', 'NUTIOC', nbsd)
    if (nbsd .le. 1) call u2mess('F', 'ALGORITH2_40')
!     CONSTITUTION DE L'OBJET SOLVEUR.FETS
    call wkvect(solveu(1:19)//'.FETS', 'V V K24', nbsd, ifets)
!     LECTURE NOMBRE TOTAL DE MAILLE:NBMA
    call jeveuo(sdfeti(1:19)//'.FDIM', 'L', idime)
    nbma=zi(idime+2)
    call jeveuo(sdfeti(1:19)//'.FREF', 'L', ifref)
    modele=zk8(ifref)
    call jeveuo(modele//'.MAILLE', 'L', imail)
!     POUR RESOUDRE LES PBS AVEC MULTIPLES SECONDS MEMBRES OU
!     MULTI-MATRICES A STRUCTURE IDENTIQUE
    call wkvect('&FETI.PAS.TEMPS', 'V V I', 3, ifetpt)
    zi(ifetpt)=nbreoi
    zi(ifetpt+1)=0
    zi(ifetpt+2)=0
    ibid1=nbreoi+1
    call wkvect('&FETI.MULTIPLE.SM.K24', 'V V K24', ibid1, imsmk)
    call wkvect('&FETI.MULTIPLE.SM.IN', 'V V I', ibid1, imsmi)
    do 10 i = 1, ibid1
        zk24(imsmk+i-1)='                       '
        zi(imsmi+i-1)=0
10  end do
!
! --- OBJET TEMPORAIRE POUR PERFORMANCE STOCKAGE FETI
    call wkvect('&FETI.INFO.STOCKAGE.FIDD', 'V V I', 2, ibid)
    zi(ibid)=0
    zi(ibid+1)=nbsd
    nbsd1=nbsd+1
    call wkvect('&FETI.INFO.STOCKAGE.FVAL', 'V V I', nbsd1, ibid)
    call wkvect('&FETI.INFO.STOCKAGE.FVAF', 'V V I', nbsd1, ibid1)
    call wkvect('&FETI.INFO.STOCKAGE.FNBN', 'V V I', nbsd1, ibid2)
    call wkvect('&FETI.INFO.CPU.FACN', 'V V R', nbsd1, ibid3)
    call wkvect('&FETI.INFO.CPU.FACS', 'V V R', nbsd1, ibid4)
    call wkvect('&FETI.INFO.CPU.ASSE', 'V V R', nbsd1, ibid5)
    do 20 i = 0, nbsd
        zi(ibid+i)=0
        zi(ibid1+i)=0
        zi(ibid2+i)=0
        zr(ibid3+i)=0.d0
        zr(ibid4+i)=0.d0
        zr(ibid5+i)=0.d0
20  end do
!
!     CONSTITUTION DES OBJETS '&FETI.MAILLE.NUMSD'
    masd='&FETI.MAILLE.NUMSD'
    call wkvect(masd, 'V V I', nbma, inumsd)
    do 30 i = 1, nbma
        zi(inumsd+i-1)=-999
30  end do
!
!     APPEL MPI POUR DETERMINER LES SOUS-DOMAINES CONCERNES PAR LE
!     PROCESSEUR COURANT. INFORMATION STOCKEE DANS OBJET JEVEUX
!     '&FETI.LISTE.SD.MPI'
    if (infofe(10:10) .eq. 'T') then
        nivmpi=2
    else
        nivmpi=1
    endif
    k24bid(1:16)=motfac
    call fetmpi(1, nbsd, ifm, nivmpi, rang,&
                nbproc, k24bid, k24bid, k24bid, rbid)
!     OBJET TEMPORAIRE POUR PROFILING CALCULS ELEMENTAIRES
    call wkvect('&FETI.INFO.CPU.ELEM', 'V V R', nbproc, ibid6)
    do 40 i = 1, nbproc
        zr(ibid6+i-1)=0.d0
40  end do
!
    if (nbproc .gt. nbsd) call u2mess('F', 'ALGORITH2_41')
!     VOIR FETGGT.F POUR EXPLICATION DE CETTE CONTRAINTE
    if ((nbproc.gt.1) .and. (stogi(1:3).ne.'OUI')) call u2mess('F', 'ALGORITH2_42')
    if (nbproc .eq. 1) then
        testok=.true.
    else
        testok=.false.
    endif
    call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
!
!     ========================================
!     BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!     ========================================
    do 50 i = 1, nbsd
        if (zi(ilimpi+i) .eq. 1) then
!
!         REMPLISSAGE .FETS : NOMS DES SD SOLVEUR DES SOUS-DOMAINES
            call gcncon('.', k8bid)
            k8bid(1:1)='F'
            solfeb=solveu(1:11)//k8bid
            zk24(ifets+i-1)=solfeb
!
!         -----------------------------------------------------------
!         CREATION ET REMPLISSAGE DE LA SD SOLVEUR "ESCLAVE" ET DU
!         VECTEUR TEMPORAIRE LOGIQUE LIEE A CHAQUE SOUS-DOMAINE
!         -----------------------------------------------------------
            metho1='MULT_FRONT'
            call creso1(solfeb, metho1, preco1, renum, syme,&
                        sdfeti, eps, resir1, tbloc, nprec,&
                        nmaxi1, istop, niremp, ifm, i,&
                        nbma, verif1, testc1, nbreo1, tyreo1,&
                        scali1, inumsd, imail, infofe, stogi,&
                        testok, nbreo2, acma1, acsm1, reacr1)
!
        endif
50  end do
!
    method='FETI'
    call creso1(solveu, method, preco, renum, syme,&
                sdfeti, eps, resire, tbloc, nprec,&
                nmaxit, istop, niremp, ifm, 0,&
                nbma, verif, testco, nbreor, tyreor,&
                scalin, inumsd, imail, infofe, stogi,&
                testok, nbreoi, acma, acsm, reacre)
!
    call jedema()
end subroutine
