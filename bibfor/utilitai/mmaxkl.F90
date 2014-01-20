subroutine mmaxkl(latabl, modele, thetai, mate, compor,&
                  symech, chfond, nnoff, basloc, courb,&
                  ndeg, thlagr, glagr, thlag2, pair,&
                  ndimte, nbprup, noprup, fiss, lonvec,&
                  ivec, resuco, lmelas, lncas, lord,&
                  milieu, connex, lischa)
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/cakg3d.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/medom1.h"
#include "asterfort/medomg.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/tbexve.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcmbl.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
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
    integer :: i, j, k,     nbcol
    integer :: iad, init
    integer ::   iret, iord, jinst, nborn,  nbval
    integer :: ik1, ik2, ik3, labscu, igl, iglm
    integer ::  ityp, mxval, nbv, itmp
    real(kind=8) :: kmoy, time, puls
    complex(kind=8) :: cbid
!
    character(len=3) :: chnu
    character(len=8) :: k8b
    character(len=16) :: k16bid, optio2, nomcas
    character(len=24) :: depla, depmax, chsigi
    character(len=24) :: valk
    logical :: exitim, lmoda
    real(kind=8), pointer :: coef(:) => null()
    character(len=24), pointer :: dep(:) => null()
    real(kind=8), pointer :: kmax1(:) => null()
    real(kind=8), pointer :: kmax2(:) => null()
    real(kind=8), pointer :: kmax3(:) => null()
    character(len=16), pointer :: noru(:) => null()
    integer, pointer :: tabi(:) => null()
    real(kind=8), pointer :: tabr(:) => null()
    character(len=8), pointer :: type_char(:) => null()
    character(len=8), pointer :: typr(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!    
    cbid=(0.d0,0.d0)
!
!- RECUPERATION DE L'ETAT INITIAL (NON TRAITE DANS CETTE OPTION)
!-INUTILE ???
    call getvid('ETAT_INIT', 'SIGM', iocc=1, scal=chsigi, nbret=init)
    if (init .ne. 0) then
        valk='CALC_K_MAX'
        call utmess('F', 'RUPTURE1_13', sk=valk)
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
                        0, sjv=iad, styp=k8b)
            nomcas=zk16(iad)
        else
            call rsadpa(resuco, 'L', 1, 'INST', iord,&
                        0, sjv=jinst, styp=k8b)
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
                    mate, compor, lischa, symech, chfond,&
                    nnoff, basloc, courb, iord, ndeg,&
                    thlagr, glagr, thlag2, pair, ndimte,&
                    exitim, time, nbprup, noprup, fiss,&
                    lmelas, nomcas, lmoda, puls, milieu,&
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
        AS_ALLOCATE(vk8=type_char, size=lonvec)
        call getvis('SIGNES', 'CHARGE_NS', iocc=1, nbval=0, nbret=nbv)
        mxval = -nbv
        call wkvect('&&MMAXKL.TMP', 'V V I', mxval, itmp)
        call getvis('SIGNES', 'CHARGE_NS', iocc=1, nbval=mxval, vect=zi(itmp),&
                    nbret=nbv)
        do 2 i = 1, mxval
            type_char(1+zi(itmp+i-1)-1) = 'NON_SIGN'
 2      continue
        call jedetr('&&MMAXKL.TMP')
        call getvis('SIGNES', 'CHARGE_S', iocc=1, nbval=0, nbret=nbv)
        mxval = -nbv
        call wkvect('&&MMAXKL.TMP', 'V V I', mxval, itmp)
        call getvis('SIGNES', 'CHARGE_S', iocc=1, nbval=mxval, vect=zi(itmp),&
                    nbret=nbv)
        do 3 i = 1, mxval
            type_char(1+zi(itmp+i-1)-1) = 'SIGNE'
 3      continue
        call jedetr('&&MMAXKL.TMP')
    endif
!
    AS_ALLOCATE(vr=coef, size=lonvec)
    do 10 i = 1, lonvec
        kmoy = 0.d0
        if (type_char(i) .ne. 'SIGNE') then
            do 11 j = 1, nnoff
                kmoy = kmoy + zr(ik1+j-1+(i-1)*nnoff)
11          continue
            if (kmoy .le. 0.d0) then
                coef(i) = -1
            else
                coef(i) = 1
            endif
        else
            coef(i) = 1
        endif
10  continue
!
    AS_ALLOCATE(vr=kmax1, size=nnoff)
    AS_ALLOCATE(vr=kmax2, size=nnoff)
    AS_ALLOCATE(vr=kmax3, size=nnoff)
    do 20 j = 1, nnoff
        kmax1(j) = 0.d0
        kmax2(j) = 0.d0
        kmax3(j) = 0.d0
        do 21 i = 1, lonvec
            kmax1(j)=kmax1(j)+coef(i)*zr(ik1+j-1+(i-1)*&
            nnoff)
            kmax2(j)=kmax2(j)+coef(i)*zr(ik2+j-1+(i-1)*&
            nnoff)
            kmax3(j)=kmax3(j)+coef(i)*zr(ik3+j-1+(i-1)*&
            nnoff)
21      continue
20  continue
!
!
!
! CALCUL DE G LOCAL MAX
!
    call wkvect('&&MMAXKL.TYP', 'V V K8', lonvec, ityp)
    AS_ALLOCATE(vk24=dep, size=lonvec)
    do 60 i = 1, lonvec
        iord = zi(ivec-1+i)
        call rsexch(' ', resuco, 'DEPL', iord, depla,&
                    iret)
        dep(i) = depla
        zk8(ityp+i-1) = 'R'
60  continue
!
    depmax = 'MMAXKL.DEPMAX'
    call vtcmbl(lonvec, zk8(ityp), coef, zk8(ityp), dep,&
                zk8(ityp), depmax)
    optio2 = 'CALC_K_G'
    lmoda = .false.
    puls = 0.d0
    call cakg3d(optio2, latabl, modele, depmax, thetai,&
                mate, compor, lischa, symech, chfond,&
                nnoff, basloc, courb, 1, ndeg,&
                thlagr, glagr, thlag2, pair, ndimte,&
                exitim, time, nbprup, noprup, fiss,&
                lmelas, k16bid, lmoda, puls, milieu,&
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
    AS_ALLOCATE(vk16=noru, size=nbcol)
    AS_ALLOCATE(vk8=typr, size=nbcol)
    do 30 i = 1, lonvec
        call codent(i, 'G', chnu)
        noru(i) = 'Q_'//chnu
        typr(i) = 'I'
30  continue
    noru(lonvec+1) = 'NUM_PT'
    typr(lonvec+1) = 'I'
    noru(1+lonvec+1) = 'ABSC_CURV'
    typr(1+lonvec+1) = 'R'
    noru(1+lonvec+2) = 'K1'
    typr(1+lonvec+2) = 'R'
    noru(1+lonvec+3) = 'K2'
    typr(1+lonvec+3) = 'R'
    noru(1+lonvec+4) = 'K3'
    typr(1+lonvec+4) = 'R'
    noru(1+lonvec+5) = 'G'
    typr(1+lonvec+5) = 'R'
!
!
    call tbcrsd('T4', 'V')
    call tbajpa('T4', nbcol, noru,typr)
    AS_ALLOCATE(vr=tabr, size=5)
    AS_ALLOCATE(vi=tabi, size=lonvec+1)
!
    do 40 i = 1, lonvec
        do 41 k = 1, lonvec
            tabi(k) = 0
41      continue
        tabi(i) = 1
!
        do 42 j = 1, nnoff
            tabi(lonvec+1) = j
            tabr(1) = zr(labscu+j-1)
            tabr(1+1) = zr(ik1+j+(i-1)*nnoff-1)
            tabr(1+2) = zr(ik2+j+(i-1)*nnoff-1)
            tabr(1+3) = zr(ik3+j+(i-1)*nnoff-1)
            tabr(1+4) = zr(igl+j+(i-1)*nnoff-1)
            call tbajli('T4', nbcol, noru, tabi, tabr,&
                        [cbid], k8b, 0)
42      continue
40  continue
!
    do 50 j = 1, nnoff
        do 51 k = 1, lonvec
            tabi(k) = nint(coef(k))
51      continue
        tabi(lonvec+1) = j
        tabr(1) = zr(labscu+j-1)
        tabr(1+1) = kmax1(j)
        tabr(1+2) = kmax2(j)
        tabr(1+3) = kmax3(j)
        tabr(1+4) = zr(iglm+j-1+nnoff*lonvec)
        call tbajli('T4', nbcol, noru, tabi, tabr,&
                    [cbid], k8b, 0)
50  continue
!
    call copisd('TABLE', 'G', 'T4', latabl)
!
    call detrsd('TABLE', 'T4')
    AS_DEALLOCATE(vr=tabr)
    AS_DEALLOCATE(vi=tabi)
    call jedetr('&&MMAXKL.TYP')
    AS_DEALLOCATE(vk16=noru)
    AS_DEALLOCATE(vk8=typr)
    AS_DEALLOCATE(vk24=dep)
    AS_DEALLOCATE(vk8=type_char)
    AS_DEALLOCATE(vr=coef)
    AS_DEALLOCATE(vr=kmax1)
    AS_DEALLOCATE(vr=kmax2)
    AS_DEALLOCATE(vr=kmax3)
!
    call jedema()
end subroutine
