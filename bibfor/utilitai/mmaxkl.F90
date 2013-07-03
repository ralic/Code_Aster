subroutine mmaxkl(latabl, modele, thetai, mate, compor,&
                 symech, chfond, nnoff, basloc,courb,&
                   ndeg, thlagr, glagr, thlag2,pair,&
                   ndimte, nbprup, noprup, fiss,lonvec,&
                   ivec, resuco, lmelas,lncas,lord,&
                    milieu, connex, lischa)
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterfort/cakg3d.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/medom1.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/tbexve.h"
#include "asterfort/u2mesk.h"
#include "asterfort/vtcmbl.h"
#include "asterfort/wkvect.h"
    integer :: nbprup, lonvec, ivec, nnoff, ndeg, ndimte
    character(len=8) :: modele, thetai, fiss, latabl
    character(len=8) :: symech, resuco
    character(len=16) :: noprup(*)
    character(len=19) :: lischa
    character(len=24) :: chfond, mate, compor, basloc, courb
    logical :: thlagr, glagr, pair, thlag2, lmelas, lncas, lord(lonvec)
    logical :: milieu, connex
!
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
!      OPERATEUR :     CALC_G
!
!     ----------------------------------------------------------------
!
!     BUT: MAXIMISATION DE K LOCAL SOUS CONTRAINTES BORNES
!
!     ----------------------------------------------------------------
!
!  IN    LATABL --> TABLE RESULTAT
!  IN    MODELE --> NOM DU MODELE
!  IN    THETAI --> BASE DE I CHAMPS THETA
!  IN    MATE   --> CHAMP DE MATERIAUX
!  IN    COMPOR --> COMPORTEMENT
!  IN    SYMECH --> SYMETRIE DU CHARGEMENT
!  IN    CHFOND --> ABSCISSES CURVILIGNES DU FOND DE FISSURE
!  IN    NNOFF  --> NOMBRE DE POINTS DU FOND DE FISSURE
!  IN    BASLOC --> BASE LOCALE
!  IN    COURB  --> NOM DU TENSEUR DE COURBURE
!  IN    NDEG   --> DEGRE DU POLYNOME DE LEGENDRE
!  IN    THLAGR --> VRAI SI LISSAGE THETA_LAGRANGE (SINON LEGENDRE)
!  IN    GLAGR  --> VRAI SI LISSAGE G_LAGRANGE (SINON LEGENDRE)
!  IN    FISS   --> NOM DE LA SD FISS_XFEM OU SD FOND_FISS
!  IN    LONVEC --> NOMBRE DE CHAMPS DE DEPLACEMENTS
!  IN    LMELAS --> TRUE SI LE TYPE DE LA SD RESULTAT EST MULT_ELAS
!  IN    MILIEU --> .TRUE.  : ELEMENT QUADRATIQUE
!                   .FALSE. : ELEMENT LINEAIRE
!  IN    CONNEX --> .TRUE.  : SI FOND FERME
!                   .FALSE. : SI FOND OUVERT
!
! ----------------------------------------------------------------------
!
!
    integer :: i, j, k, icoef, ikm1, ikm2, ikm3, nbcol, inopr, itypr
    integer :: iad, init
    integer :: ipr, ipi, iret, iord, jinst, nborn, itypc, nbval
    integer :: ik1, ik2, ik3, labscu, igl, ibid, iglm
    integer :: inom, ityp, mxval, nbv, itmp
    real(kind=8) :: kmoy, time, puls
    complex(kind=8) :: cbid
!
    character(len=3) :: chnu
    character(len=8) :: k8b
    character(len=16) :: k16bid, optio2, nomcas
    character(len=24) :: depla, depmax, chsigi
    character(len=24) :: valk
    logical :: exitim, lmoda
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!- RECUPERATION DE L'ETAT INITIAL (NON TRAITE DANS CETTE OPTION)
!-INUTILE ???
    call getvid('COMP_INCR', 'SIGM_INIT', 1, iarg, 1,&
                chsigi, init)
    if (init .ne. 0) then
        valk='CALC_K_MAX'
        call u2mesk('F', 'RUPTURE1_13', 1, valk)
    endif
!
! CALCUL DE K POUR CHACUNE DES SITUATIONS
!
    do 1 i = 1, lonvec
!
        iord = zi(ivec-1+i)
!
        if (lmelas) then
            if (lncas) then
                if (.not.lord(i)) goto 1
            endif
            exitim = .false.
            time=0.d0
            call rsadpa(resuco, 'L', 1, 'NOM_CAS', iord,&
                        0, iad, k8b)
            nomcas=zk16(iad)
        else
            call rsadpa(resuco, 'L', 1, 'INST', iord,&
                        0, jinst, k8b)
            time = zr(jinst)
            exitim = .true.
        endif
!
        call medomg(resuco, iord, modele, mate, lischa)
        call rsexch('F', resuco, 'DEPL', iord, depla,&
                    iret)
!
        optio2 = 'CALC_K_G'
        lmoda = .false.
        puls = 0.d0
        call cakg3d(optio2, latabl, modele, depla, thetai,&
                    mate, compor, lischa, symech,chfond,&
                    nnoff, basloc, courb, iord, ndeg,&
                    thlagr, glagr, thlag2, pair,ndimte,&
                    exitim, time, nbprup, noprup,fiss,&
                    lmelas, nomcas, lmoda, puls,milieu,&
                    connex)
 1  continue
!
! EXTRACTION DES RESULTATS DE LA TABLE
!
    call tbexve(latabl, 'K1', '&&MMAXKL.K1LOC', 'V', nbval,&
                k8b)
    call jeveuo('&&MMAXKL.K1LOC', 'L', ik1)
    call tbexve(latabl, 'K2', '&&MMAXKL.K2LOC', 'V', nbval,&
                k8b)
    call jeveuo('&&MMAXKL.K2LOC', 'L', ik2)
    call tbexve(latabl, 'K3', '&&MMAXKL.K3LOC', 'V', nbval,&
                k8b)
    call jeveuo('&&MMAXKL.K3LOC', 'L', ik3)
    call tbexve(latabl, 'ABSC_CURV', '&&MMAXKL.ABSCUR', 'V', nbval,&
                k8b)
    call jeveuo('&&MMAXKL.ABSCUR', 'L', labscu)
    call tbexve(latabl, 'G', '&&MMAXKL.GLOC', 'V', nbval,&
                k8b)
    call jeveuo('&&MMAXKL.GLOC', 'L', igl)
!
!
! RECUPERATION DU TYPE DE CHARGE ET MAXIMISATION DE K
!
    call getfac('SIGNES', nborn)
    if (nborn .ne. 0) then
        call wkvect('&&MMAXKL.TYPE_CHAR', 'V V K8', lonvec, itypc)
        mxval = 0
        call getvis('SIGNES', 'CHARGE_NS', 1, iarg, mxval,&
                    ibid, nbv)
        mxval = -nbv
        call wkvect('&&MMAXKL.TMP', 'V V I', mxval, itmp)
        call getvis('SIGNES', 'CHARGE_NS', 1, iarg, mxval,&
                    zi(itmp), nbv)
        do 2 i = 1, mxval
            zk8(itypc+zi(itmp+i-1)-1) = 'NON_SIGN'
 2      continue
        call jedetr('&&MMAXKL.TMP')
        mxval = 0
        call getvis('SIGNES', 'CHARGE_S', 1, iarg, mxval,&
                    ibid, nbv)
        mxval = -nbv
        call wkvect('&&MMAXKL.TMP', 'V V I', mxval, itmp)
        call getvis('SIGNES', 'CHARGE_S', 1, iarg, mxval,&
                    zi(itmp), nbv)
        do 3 i = 1, mxval
            zk8(itypc+zi(itmp+i-1)-1) = 'SIGNE'
 3      continue
        call jedetr('&&MMAXKL.TMP')
    endif
!
    call wkvect('&&MMAXKL.COEF', 'V V R8', lonvec, icoef)
    do 10 i = 1, lonvec
        kmoy = 0.d0
        if (zk8(itypc+i-1) .ne. 'SIGNE') then
            do 11 j = 1, nnoff
                kmoy = kmoy + zr(ik1+j-1+(i-1)*nnoff)
11          continue
            if (kmoy .le. 0.d0) then
                zr(icoef+i-1) = -1
            else
                zr(icoef+i-1) = 1
            endif
        else
            zr(icoef+i-1) = 1
        endif
10  continue
!
    call wkvect('&&MMAXKL.KMAX1', 'V V R8', nnoff, ikm1)
    call wkvect('&&MMAXKL.KMAX2', 'V V R8', nnoff, ikm2)
    call wkvect('&&MMAXKL.KMAX3', 'V V R8', nnoff, ikm3)
    do 20 j = 1, nnoff
        zr(ikm1+j-1) = 0.d0
        zr(ikm2+j-1) = 0.d0
        zr(ikm3+j-1) = 0.d0
        do 21 i = 1, lonvec
            zr(ikm1+j-1)=zr(ikm1+j-1)+zr(icoef+i-1)*zr(ik1+j-1+(i-1)*&
            nnoff)
            zr(ikm2+j-1)=zr(ikm2+j-1)+zr(icoef+i-1)*zr(ik2+j-1+(i-1)*&
            nnoff)
            zr(ikm3+j-1)=zr(ikm3+j-1)+zr(icoef+i-1)*zr(ik3+j-1+(i-1)*&
            nnoff)
21      continue
20  continue
!
!
!
! CALCUL DE G LOCAL MAX
!
    call wkvect('&&MMAXKL.TYP', 'V V K8', lonvec, ityp)
    call wkvect('&&MMAXKL.DEP', 'V V K24', lonvec, inom)
    do 60 i = 1, lonvec
        iord = zi(ivec-1+i)
        call rsexch(' ', resuco, 'DEPL', iord, depla,&
                    iret)
        zk24(inom+i-1) = depla
        zk8(ityp+i-1) = 'R'
60  continue
!
    depmax = 'MMAXKL.DEPMAX'
    call vtcmbl(lonvec, zk8(ityp), zr(icoef), zk8(ityp), zk24(inom),&
                zk8(ityp), depmax)
    optio2 = 'CALC_K_G'
    lmoda = .false.
    puls = 0.d0
    call cakg3d(optio2, latabl, modele, depmax, thetai,&
                mate, compor, lischa, symech,chfond,&
                 nnoff, basloc, courb, 1,ndeg,&
                 thlagr, glagr, thlag2, pair,ndimte,&
                 exitim, time, nbprup, noprup,fiss,&
                 lmelas, k16bid, lmoda, puls,milieu,&
                 connex)
!
    call tbexve(latabl, 'G', '&&MMAXKL.GMAX', 'V', nbval,&
                k8b)
    call jeveuo('&&MMAXKL.GMAX', 'L', iglm)
    call detrsd('TABLE', latabl)
!
!
! CREATION DU TABLEAU RESULTAT
!
    nbcol = lonvec + 6
    call wkvect('&&MMAXKL.NORU', 'V V K16', nbcol, inopr)
    call wkvect('&&MMAXKL.TYPR', 'V V K8', nbcol, itypr)
    do 30 i = 1, lonvec
        call codent(i, 'G', chnu)
        zk16(inopr+i-1) = 'Q_'//chnu
        zk8 (itypr+i-1) = 'I'
30  continue
    zk16(inopr+lonvec) = 'NUM_PT'
    zk8 (itypr+lonvec) = 'I'
    zk16(inopr+lonvec+1) = 'ABSC_CURV'
    zk8 (itypr+lonvec+1) = 'R'
    zk16(inopr+lonvec+2) = 'K1'
    zk8 (itypr+lonvec+2) = 'R'
    zk16(inopr+lonvec+3) = 'K2'
    zk8 (itypr+lonvec+3) = 'R'
    zk16(inopr+lonvec+4) = 'K3'
    zk8 (itypr+lonvec+4) = 'R'
    zk16(inopr+lonvec+5) = 'G'
    zk8 (itypr+lonvec+5) = 'R'
!
!
    call tbcrsd('T4', 'V')
    call tbajpa('T4', nbcol, zk16(inopr), zk8(itypr))
    call wkvect('&&MMAXKL.TABR', 'V V R', 5, ipr)
    call wkvect('&&MMAXKL.TABI', 'V V I', lonvec+1, ipi)
!
    do 40 i = 1, lonvec
        do 41 k = 1, lonvec
            zi(ipi+k-1) = 0
41      continue
        zi(ipi+i-1) = 1
!
        do 42 j = 1, nnoff
            zi(ipi+lonvec) = j
            zr(ipr) = zr(labscu+j-1)
            zr(ipr+1) = zr(ik1+j+(i-1)*nnoff-1)
            zr(ipr+2) = zr(ik2+j+(i-1)*nnoff-1)
            zr(ipr+3) = zr(ik3+j+(i-1)*nnoff-1)
            zr(ipr+4) = zr(igl+j+(i-1)*nnoff-1)
            call tbajli('T4', nbcol, zk16(inopr), zi(ipi), zr(ipr),&
                        cbid, k8b, 0)
42      continue
40  continue
!
    do 50 j = 1, nnoff
        do 51 k = 1, lonvec
            zi(ipi+k-1) = nint(zr(icoef+k-1))
51      continue
        zi(ipi+lonvec) = j
        zr(ipr) = zr(labscu+j-1)
        zr(ipr+1) = zr(ikm1+j-1)
        zr(ipr+2) = zr(ikm2+j-1)
        zr(ipr+3) = zr(ikm3+j-1)
        zr(ipr+4) = zr(iglm+j-1+nnoff*lonvec)
        call tbajli('T4', nbcol, zk16(inopr), zi(ipi), zr(ipr),&
                    cbid, k8b, 0)
50  continue
!
    call copisd('TABLE', 'G', 'T4', latabl)
!
    call detrsd('TABLE', 'T4')
    call jedetr('&&MMAXKL.TABR')
    call jedetr('&&MMAXKL.TABI')
    call jedetr('&&MMAXKL.TYP')
    call jedetr('&&MMAXKL.NORU')
    call jedetr('&&MMAXKL.TYPR')
    call jedetr('&&MMAXKL.DEP')
    call jedetr('&&MMAXKL.TYPE_CHAR')
    call jedetr('&&MMAXKL.COEF')
    call jedetr('&&MMAXKL.KMAX1')
    call jedetr('&&MMAXKL.KMAX2')
    call jedetr('&&MMAXKL.KMAX3')
!
    call jedema()
end subroutine
