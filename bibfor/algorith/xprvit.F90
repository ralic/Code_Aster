subroutine xprvit(noma, fiss, ndim, nvit, nbeta,&
                  lcmin, cnsvt, cnsvn, vpoint, cnsbl,&
                  cnsdis, disfr, cnsbet, listp, damax,&
                  locdom, rdimp, rdtor, delta, ucnslt,&
                  ucnsln)
!
! aslint: disable=W1501,W1504
    implicit none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/cnscre.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/xprfon.h"
#include "asterfort/xprvir.h"
    character(len=8) :: noma, fiss
!
    character(len=19) :: cnsvt, cnsvn, vpoint, disfr, cnsbl, cnsdis, cnsbet
    character(len=19) :: listp, delta, ucnslt, ucnsln
    character(len=24) :: nvit, nbeta
    integer :: ndim
    real(kind=8) :: lcmin, damax, rdimp, rdtor
    logical :: locdom
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
! person_in_charge: patrick.massin at edf.fr
!     ------------------------------------------------------------------
!
!       XPRVIT   : X-FEM PROPAGATION : EXTENSION DU CHAMP DE VITESSES
!       ------     -     --                                  ---
!    CALCUL DE LA VITESSE DE PROPAGATION DE FISSURE SUR LE FOND
!    ET EXTENSION DU CHAMP DE VITESSE A TOUS LES NOEUDS DU MAILLAGE
!
!    ENTREE
!        NOMA    : NOM DU CONCEPT MAILLAGE
!        FISS    : NOM DU CONCEPT FISSURE X-FEM
!                  (FISSURE INITIALE DONT ON EXTRAIT LE FOND DE FISSURE)
!        NDIM    : DIMENSION DU MODELE (2=2D OU 3=3D)
!        NVIT    : VECTEUR DES VITESSES DE PROPAGATION POUR CHAQUE POINT
!                  DU FOND DE LA FISSURE (NOM DU CONCEPT)
!        NBETA   : VECTEUR DES ANGLES DE PROPAGATION POUR CHAQUE POINT
!                  DU FOND DE LA FISSURE (NOM DU CONCEPT)
!        LCMIN   : LONGUEUR DE LA PLUS PETITE ARETE DU MAILLAGE
!        DAMAX   : AVANCEMEMT MAXIMAL DE LA FISSURE
!        RADIMP  : RAYON DE LOCALISATION DES LEVEL SETS
!        LOCDOM  : LOCALISATION DES LEVELS SETS ACTIVEe
!
!    SORTIE
!        CNSVT   : CHAM_NO_S VITESSE TANGENTIELLE DE PROPAGATION
!        CNSVN   : CHAM_NO_S VITESSE NORMALE DE PROPAGATION
!        VPOINT  : VECTEUR DES VITESSES DE PROPAGATION EN CHAQUE POINT
!                  DU DOMAINE DE CALCUL (MODULE DE LA VITESSE DU POINT
!                  PROJETE SUR LE FOND DE LA FISSURE)
!        CNSBL   : CHAM_NO_S BASE LOCALE POUR CHAQUE NODE DU MAILLAGE
!                  (AXE NORMALE ET AXE TANGENTE AU PLANE DE LA FISSURE)
!        CNSDIS  : CHAM_NO_S VECTEUR DISTANCE ENTRE CHAQUE NODE DU
!                  MAILLAGE ET SON PROJECTION SUR LE FOND DE FISSURE
!        DISFR   : VECTEUR INDIQUANT LA DISTANCE^2 ENTRE CHAQUE NODE DU
!                  MAILLAGE NOMA ET LE FOND DU FISSURE
!        CNSBET  : VECTEUR DES ANGLES DE BIFURCATION DE LA FISSURE
!                  EN CHAQUE POINT DU DOMAINE DE CALCUL (ANGLE AU POINT
!                  PROJETE SUR LE FOND DE LA FISSURE)
!        LISTP   : VECTEUR (A 3 COMPOSANTES) OU LES CORDONNEES DU
!                  PROJETE DE CHAQUE POINT DU DOMAINE DE CALCUL SUR LE
!                  FOND DE LA FISSURE SONT STOCKEES
!       RADIMP   : RAYON DE LA ZONE DE REACTUALISATION DES LEVELS SETS
!       RADTOR   : RAYON DE LA ZONE DE REPROJECTION DES LEVELS SETS
!       DAMAX    : AVANCEMENT MAXIMUM DU FRONT DE FISSURE
!       DELTA    : VECTEUR CONTENANT LES CORRECTION DES LEVELS SETS
!                 TANGENTES ET NORMALES
!       UCNLSN   : CHAM_NO_S  LEVEL SET NORMALE AU NOEUDS
!       UCNLSN   : CHAM_NO_S  LEVEL SET TANGENTE AU NOEUDS
!
!     ------------------------------------------------------------------
!
!
    integer :: i, j, jcoor, nbno, jmin, nbptff, ibid, jdelta, lsn, lst
    integer :: jfonf, jvtff, jvnff, jvtl, jvtv, jvnl, jvnv, ifm, niv, jvit
    integer :: jbeta, jdisfr, cfv, bfv, vfv, afv, nfv
    real(kind=8) :: eps, xm, ym, zm, dmin, smin, xi1, yi1, zi1, xj1, yj1, zj1
    real(kind=8) :: xij, yij, zij, xim, yim, zim, s, norm2, xn, yn, zn, d
    real(kind=8) :: radimp, radtor
    character(len=8) :: typcmp(6), method
    integer :: jvff, jbasef, jbl, jdis, k
!
    real(kind=8) :: bast(3), tast(3), n(3), t(3), b(3), mtast, pi(3), normij
    real(kind=8) :: lsnth(2), lstth(2), normkl, modnor, modtan
    logical :: grille, fonvir, fvirtu
!
    character(len=19) :: covir, bavir, vitvir, angvir, numvir
!
!     EULER AXIS AND EULER ANGLE CALCULATIONS
    integer :: jeuler, jcnsb, jlistp, jvp
    real(kind=8) :: ni(3), ti(3), bi(3), nj(3), tj(3), bj(3), rij(3, 3), tpl(3)
    real(kind=8) :: npl(3), bpl(3), axeul(3), calfa, salfa, modvec
    real(kind=8) :: t0, t180, alfa
    parameter      (t0 = 0.5d0/180.d0*3.1415d0)
    parameter      (t180 = 179.5d0/180.d0*3.1415d0)
    logical :: endpnt
!
!     MULTIPLE CRACK FRONTS
    integer :: jfmult, numfon, fon
!
!     BISECTION METHOD AND VELOCITY INTERPOLATION
    real(kind=8) :: tolld, dprec, ds, vp, betap
    integer :: maxite, jlimsx, jlimdx
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
!
!     Recuperation des points des caracteristique du maillage et du
!     fond de fissure
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
    radtor = rdtor
    radimp = rdimp
!
!     RECUPERATION DE LA METHODE DE REINITIALISATION A EMPLOYER
    call getvtx(' ', 'METHODE', scal=method, nbret=ibid)
!
!     RECUPERATION DES CARACTERISTIQUES DU MAILLAGE
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
!     RECUPERATION DU FOND DE FISSURE
    call jeveuo(fiss//'.FONDFISS', 'L', jfonf)
    call dismoi('NB_POINT_FOND', fiss, 'FISS_XFEM', repi=nbptff)
!
!     RETRIEVE THE DIFFERENT PIECES OF THE CRACK FRONT
    call jeveuo(fiss//'.FONDMULT', 'L', jfmult)
    call dismoi('NB_FOND', fiss, 'FISS_XFEM', repi=numfon)
!
!     RETRIEVE THE LOCAL REFERENCE SYSTEM FOR EACH NODE ON THE FRONT
    call jeveuo(fiss//'.BASEFOND', 'E', jbasef)
!
!     RETRIEVE THE CRACK'S SPEED AND PROPAGATION ANGLE FOR EACH NODE ON
!     THE FRONT
    call jeveuo(nvit, 'E', jvit)
    call jeveuo(nbeta, 'E', jbeta)
!
    fvirtu = .false.
!
!     CHECK if an auxilliary grid is used
!
    call jeexin(fiss//'.GRI.MODELE', ibid)
    if (ibid .eq. 0) then
!        NO AUXILIARY GRID USED
        grille=.false.
    else
        grille=.true.
    endif
!
!      On utilise un fond virtuel si une grille auxilliaire
!      est utilisee et que l on travaille sur un modele 3D
!
    if ((grille) .and. (ndim.eq.3) .and. (method.ne.'GEOMETRI')) then
!
        fvirtu = .true.
!
!     EXTRACTION des valeurs des levels sets au noeud
        call jeveuo(ucnsln//'.CNSV', 'L', lsn)
        call jeveuo(ucnslt//'.CNSV', 'L', lst)
!
!     Creation de vecteur ou sont stocke les coordonnes et les
!     bases associees du font de fissure virtuel
        covir='&&XPRVIT.COVIR'
        bavir='&&XPRVIT.BAVIR'
        vitvir='&&XPRVIT.VITVIR'
        angvir='&&XPRVIT.ANGVIR'
        numvir='&&XPRVIT.NUMVIR'
!
        call wkvect(covir, 'V V R8', 4*(nbptff+2*numfon), cfv)
        call wkvect(bavir, 'V V R8', 6*(nbptff+2*numfon), bfv)
        call wkvect(vitvir, 'V V R8', (nbptff+2*numfon), vfv)
        call wkvect(angvir, 'V V R8', (nbptff+2*numfon), afv)
        call wkvect(numvir, 'V V I', (2*numfon), nfv)
!
!      OPERATION SUR RADIMP ET RADTOR
        if (radimp .gt. 0.d0) then
            radimp=sqrt(radimp)
        endif
        if (radtor .gt. 0.d0) then
            radtor=sqrt(radtor)
        endif
!
!
        if (numfon .gt. 1) then
!        ON VERIFIE QUE LE FOND DE FISSURE EST BIEN ORIENTE
!        SINON ON MODIFIE LA NUMEROTATION DU FOND DE FISSURE
            call xprfon(noma, fiss, numfon, nvit, nbeta)
        endif
!
!       DEFINITION DU FOND VIRTUEL
        call xprvir(fiss, covir, bavir, vitvir, angvir,&
                    numvir, numfon, nvit, nbeta, nbptff,&
                    radimp, radtor, damax, noma, locdom)
!
!       ON POINTE LES ROUTINES DESTINEES AU FRONT PHYSIQUE
!       SUR LE FRONT VIRTUEL
        jfonf=cfv
        jbasef=bfv
        jvit=vfv
        jbeta=afv
        jfmult=nfv
    endif
!
!     CREATE THE CHAMP_NO_S WHERE THE LOCAL REFERENCE SYSTEM IS
!     STORED FOR EACH NODE IN THE MESH.
!     CREATE ALSO THE CHAMP_NO_S WHERE THE DISTANCE BETWEEN EACH
!     NODE AND ITS PROJECTION ON THE CRACK FRONT IS STORED.
    typcmp(1)='X1'
    typcmp(2)='X2'
    typcmp(3)='X3'
    typcmp(4)='X4'
    typcmp(5)='X5'
    typcmp(6)='X6'
    if (ndim .eq. 2) then
!        2D CASE: EACH VECTOR IN THE FIELD HAS 2 COMPONENTS ONLY
        call cnscre(noma, 'NEUT_R', 4, typcmp, 'V',&
                    cnsbl)
        call cnscre(noma, 'NEUT_R', 4, typcmp, 'V',&
                    cnsdis)
    else
!        3D CASE: EACH VECTOR IN THE FIELD HAS 3 COMPONENTS
        call cnscre(noma, 'NEUT_R', 6, typcmp, 'V',&
                    cnsbl)
        call cnscre(noma, 'NEUT_R', 6, typcmp, 'V',&
                    cnsdis)
    endif
!
    call jeveuo(cnsbl//'.CNSV', 'E', jbl)
    call jeveuo(cnsdis//'.CNSV', 'E', jdis)
!
!     CREATION DES CHAM_NO_S CONTENANT LES COMPOSANTES DU VECTEUR V
!     (VT & VN)
    call cnscre(noma, 'NEUT_R', 1, 'X1', 'V',&
                cnsvt)
    call cnscre(noma, 'NEUT_R', 1, 'X1', 'V',&
                cnsvn)
!
!     CREATE THE VECTOR WHERE THE MODULE OF THE PROPAGATION SPEED IS
!     STORED FOR EACH POINT (=SQRT(CNSVT**2+CNSVN**2))
    call wkvect(vpoint, 'V V R8', nbno, jvp)
!
!     CREATION DES VECTEURS DE VITESSE DE PROPAGATION EN FOND
!     DE FISSURE
!
    call wkvect('&&XPRVIT.V_PROPA_FF', 'V V R8', ndim*nbptff, jvff)
    call wkvect('&&XPRVIT.VT_PROPA_FF', 'V V R8', nbptff, jvtff)
    call wkvect('&&XPRVIT.VN_PROPA_FF', 'V V R8', nbptff, jvnff)
!
!     CREATE THE VECTOR WHERE THE DISTANCE BETWEEN EACH NODE AND THE
!     CRACK FRONT IS STORED
    call wkvect(disfr, 'V V R8', nbno, jdisfr)
!
!     CREATE THE VECTOR WHERE THE PROPAGATION ANGLE IS STORED FOR EACH
!     POINT
    call wkvect(cnsbet, 'V V R8', nbno, jcnsb)
!
!     CREATE THE VECTOR WHERE THE COORDINATES OF THE PROJECTED POINT
!     ARE STORED FOR EACH POINT
    call wkvect(listp, 'V V R8', 3*nbno, jlistp)
!
    call jeveuo(cnsvt//'.CNSV', 'E', jvtv)
    call jeveuo(cnsvt//'.CNSL', 'E', jvtl)
    call jeveuo(cnsvn//'.CNSV', 'E', jvnv)
    call jeveuo(cnsvn//'.CNSL', 'E', jvnl)
!
!     CREATE THE VECTOR WHERE THE EULER AXIS AND THE EULER ANGLE ARE
!     STORED. THESE OBJECTS ARE STORED IN THE FOLLOWING ORDER:
!     ELEMENT 1: EULER ANGLE FOR THE FIRST POINT ON THE CRACK FRONT
!     ELEMENT 2: X COMPONENT OF THE EULER AXIS FOR THE FIRST POINT ON
!                THE CRACK FRONT
!     ELEMENT 3: Y COMPONENT OF THE EULER AXIS FOR THE FIRST POINT ON
!                THE CRACK FRONT
!     ELEMENT 4: Z COMPONENT OF THE EULER AXIS FOR THE FIRST POINT ON
!                THE CRACK FRONT
!     ELEMENT 5: X COMPONENT OF THE LOCAL BASE FOR THE FIRST POINT ON
!                THE CRACK FRONT
!     ELEMENT 6: Y COMPONENT OF THE LOCAL BASE FOR THE FIRST POINT ON
!                THE CRACK FRONT
!     ELEMENT 7: Z COMPONENT OF THE LOCAL BASE FOR THE FIRST POINT ON
!                THE CRACK FRONT
!     THIS BLOCK IS REPEATED FOR ALL THE POINTS ON THE CRACK FRONT WITH
!     THE EXCEPTION OF THE LAST ONE.
    call wkvect('&&XPRVIT.EULER', 'V V R8', 7*nbptff, jeuler)
!
!     creation du vecteur contenant les modifications a apporter au
!     level set avant application du fond virtuel
    call wkvect(delta, 'V V R8', 2*nbno, jdelta)
!
!    Initialisation du vecteur delta
    do i = 1, 2*nbno
        zr(jdelta+i-1)=0.d0
    end do
!
! ***************************************************************
! ELABORATE EACH POINT ON THE CRACK FRONT IN ORDER TO CALCULATE
! THE FOLLOWING:
! - PROPAGATION SPEED VECTOR V AND ITS NORMAL AND TANGENTIAL COMPONENTS
! - PROPAGATION ANGLE
! - EULER AXIS AND ANGLE BETWEEN TWO CONSECUTIVE REFERENCE SYSTEMS ON
!   THE CRACK FRONT
! ***************************************************************
!
    do i = 1, nbptff
!
!        ***************************************************************
!        CALCULATE THE NORMAL AND THE TANGENTIAL PROPAGATION SPEED AT
!        THE ACTUAL POINT ON THE CRACK FRONT
!        ***************************************************************
!
        zr(jvtff-1+i)=zr(jvit-1+i)*cos(zr(jbeta-1+i))
        zr(jvnff-1+i)=zr(jvit-1+i)*sin(zr(jbeta-1+i))
!
!        ***************************************************************
!        RECALCULATE THE LOCAL REFERENCE SYSTEM IN THE ACTUAL CRACK
!        FRONT POINT IN ORDER TO BE SURE THAT THE THREE AXES ARE
!        ORTHOGONAL EACH OTHER
!        ***************************************************************
!
        if (ndim .eq. 2) then
!          NORMAL AXIS (NORMAL TO THE CRACK PLANE)
            n(1) = zr(jbasef-1+2*ndim*(i-1)+1)
            n(2) = zr(jbasef-1+2*ndim*(i-1)+2)
            n(3) = 0
!          TANGENTIAL AXIS (TANGENT TO THE CRACK PLANE)
            t(1) = zr(jbasef-1+2*ndim*(i-1)+3)
            t(2) = zr(jbasef-1+2*ndim*(i-1)+4)
            t(3) = 0
        else
!          NORMAL AXIS
            n(1) = zr(jbasef-1+2*ndim*(i-1)+1)
            n(2) = zr(jbasef-1+2*ndim*(i-1)+2)
            n(3) = zr(jbasef-1+2*ndim*(i-1)+3)
!          TANGENTIAL AXIS
            t(1) = zr(jbasef-1+2*ndim*(i-1)+4)
            t(2) = zr(jbasef-1+2*ndim*(i-1)+5)
            t(3) = zr(jbasef-1+2*ndim*(i-1)+6)
        endif
!
!        CALCULATE THE BINORMAL AXIS AS THE VECTORIAL PRODUCT BETWEEN
!        THE TANGENTIAL AND NORMAL AXES. THIS AXIS IS TANGENT TO THE
!        CRACK FRONT.
        bast(1) = n(2)*t(3)-n(3)*t(2)
        bast(2) = n(3)*t(1)-n(1)*t(3)
        bast(3) = n(1)*t(2)-n(2)*t(1)
!
!        RECALCULATE THE TANGENTIAL AXIS AS THE VECTORIAL PRODUCT
!        BETWEEN THE BINORMAL AND THE NORMAL AXES.
        tast(1) = bast(2)*n(3)-bast(3)*n(2)
        tast(2) = bast(3)*n(1)-bast(1)*n(3)
        tast(3) = bast(1)*n(2)-bast(2)*n(1)
!
!        CALCULATE THE MODULE OF THE NEW TANGENTIAL AXIS
        mtast = (tast(1)**2.d0+tast(2)**2.d0+tast(3)**2.d0)**0.5d0
!
!        THE MODULE OF THE NEW TANGENTIAL AXIS SHOULD ALWAYS BE GREATER
!        THAN ZERO... EXCEPT IN THE CASE THE NORMAL AND TANGENTIAL AXIS
!        ARE PARALLEL! IN THIS CASE SOMETHING NASTY HAS HAPPENED TO THE
!        LEVEL SETS (OUTSIDE THIS SUBROUTINE)!
        ASSERT(mtast.gt.0.d0)
!
!        CALCULATE THE UNIT VECTOR FOR THE NEW TANGENTIAL AXIS AND STORE
!        IT
        if (ndim .eq. 2) then
            zr(jbasef-1+2*ndim*(i-1)+3) = tast(1)/mtast
            zr(jbasef-1+2*ndim*(i-1)+4) = tast(2)/mtast
        else
            zr(jbasef-1+2*ndim*(i-1)+4) = tast(1)/mtast
            zr(jbasef-1+2*ndim*(i-1)+5) = tast(2)/mtast
            zr(jbasef-1+2*ndim*(i-1)+6) = tast(3)/mtast
        endif
!
!        CALCULATE THE UNIT VECTOR FOR THE NORMAL AXIS AND STORE IT
        mtast = (n(1)**2.d0+n(2)**2.d0+n(3)**2.d0)**0.5d0
        if (ndim .eq. 2) then
            zr(jbasef-1+2*ndim*(i-1)+1) = n(1)/mtast
            zr(jbasef-1+2*ndim*(i-1)+2) = n(2)/mtast
        else
            zr(jbasef-1+2*ndim*(i-1)+1) = n(1)/mtast
            zr(jbasef-1+2*ndim*(i-1)+2) = n(2)/mtast
            zr(jbasef-1+2*ndim*(i-1)+3) = n(3)/mtast
        endif
!
!        ***************************************************************
!        EVALUATE THE PROPAGATION SPEED VECTOR V AT THE POINT
!        ***************************************************************
!
!        CALCULATE THE MODULE OF THE NORMAL AND TANGENTIAL AXES FOR THE
!        LOCAL REFERENCE SYSTEM AT THE POINT
        if (ndim .eq. 2) then
            modnor = sqrt(zr( jbasef-1+2*ndim*(i-1)+1)**2.d0+ zr( jbasef-1+2*ndim*(i-1)+2 )**2.d0&
                     )
            modtan = sqrt(zr( jbasef-1+2*ndim*(i-1)+3)**2.d0+ zr( jbasef-1+2*ndim*(i-1)+4 )**2.d0&
                     )
        else
            modnor = sqrt(&
                     zr(&
                     jbasef-1+2*ndim*(i-1)+1)**2.d0+ zr( jbasef-1+2*ndim*(i-1)+2)**2.d0+ zr(jbase&
                     &f-1+2*ndim*(i-1)+ 3&
                     )**2.d0&
                     )
            modtan = sqrt(&
                     zr(&
                     jbasef-1+2*ndim*(i-1)+4)**2.d0+ zr( jbasef-1+2*ndim*(i-1)+5)**2.d0+ zr(jbase&
                     &f-1+2*ndim*(i-1)+ 6&
                     )**2.d0&
                     )
        endif
!
        ASSERT(modnor.gt.0.d0)
        ASSERT(modtan.gt.0.d0)
!
!        CALCULATE THE PROPAGATION SPEED VECTOR V IN THE GLOBAL
!        REFERENCE SYSTEM USED FOR THE MESH
        do j = 1, ndim
            zr(jvff-1+ndim*(i-1)+j) = zr(jvnff-1+i)*zr(jbasef-1+2* ndim*(i-1)+j)/modnor+ zr(jvtff&
                                      &-1+i)*zr(jbasef-1+2*ndim*(i- 1)+j+ndim)/modtan
        end do
!
!        ***************************************************************
!        EVALUATE THE EULER AXIS AND ANGLE FOR THE ROTATION BETWEEN
!        THE LOCAL BASE IN THE PREVIOUS POINT AND THE LOCAL BASE IN THE
!        ACTUAL POINT
!        ***************************************************************
!
        if ((ndim.eq.3) .and. (i.gt.1)) then
!
!           RETRIEVE THE LOCAL BASE FOR THE PREVIOUS POINT
            ni(1) = zr(jbasef-1+2*ndim*(i-1-1)+1)
            ni(2) = zr(jbasef-1+2*ndim*(i-1-1)+2)
            ni(3) = zr(jbasef-1+2*ndim*(i-1-1)+3)
            ti(1) = zr(jbasef-1+2*ndim*(i-1-1)+4)
            ti(2) = zr(jbasef-1+2*ndim*(i-1-1)+5)
            ti(3) = zr(jbasef-1+2*ndim*(i-1-1)+6)
!           THE BINORMAL AXIS MUST BE CALCULATED AS THE VECTORIAL
!           PRODUCT BETWEEN TANGENTIAL AND NORMAL AXES (T,N,B)
            bi(1) = ti(2)*ni(3)-ti(3)*ni(2)
            bi(2) = ti(3)*ni(1)-ti(1)*ni(3)
            bi(3) = ti(1)*ni(2)-ti(2)*ni(1)
!
!           STORE THE BINORMAL AXIS
            zr(jeuler-1+7*(i-1-1)+5) = bi(1)
            zr(jeuler-1+7*(i-1-1)+6) = bi(2)
            zr(jeuler-1+7*(i-1-1)+7) = bi(3)
!
!           RETRIEVE THE LOCAL BASE FOR THE ACTUAL POINT
            nj(1) = zr(jbasef-1+2*ndim*(i-1)+1)
            nj(2) = zr(jbasef-1+2*ndim*(i-1)+2)
            nj(3) = zr(jbasef-1+2*ndim*(i-1)+3)
            tj(1) = zr(jbasef-1+2*ndim*(i-1)+4)
            tj(2) = zr(jbasef-1+2*ndim*(i-1)+5)
            tj(3) = zr(jbasef-1+2*ndim*(i-1)+6)
!           THE BINORMAL AXIS MUST BE CALCULATED AS THE VECTORIAL
!           PRODUCT BETWEEN TANGENTIAL AND NORMAL AXES (T,N,B)
            bj(1) = tj(2)*nj(3)-tj(3)*nj(2)
            bj(2) = tj(3)*nj(1)-tj(1)*nj(3)
            bj(3) = tj(1)*nj(2)-tj(2)*nj(1)
!
!           CALCULATE THE ROTATION MATRIX
            rij(1,1) = tj(1)*ti(1)+tj(2)*ti(2)+tj(3)*ti(3)
            rij(2,1) = tj(1)*ni(1)+tj(2)*ni(2)+tj(3)*ni(3)
            rij(3,1) = tj(1)*bi(1)+tj(2)*bi(2)+tj(3)*bi(3)
!
            rij(1,2) = nj(1)*ti(1)+nj(2)*ti(2)+nj(3)*ti(3)
            rij(2,2) = nj(1)*ni(1)+nj(2)*ni(2)+nj(3)*ni(3)
            rij(3,2) = nj(1)*bi(1)+nj(2)*bi(2)+nj(3)*bi(3)
!
            rij(1,3) = bj(1)*ti(1)+bj(2)*ti(2)+bj(3)*ti(3)
            rij(2,3) = bj(1)*ni(1)+bj(2)*ni(2)+bj(3)*ni(3)
            rij(3,3) = bj(1)*bi(1)+bj(2)*bi(2)+bj(3)*bi(3)
!
!           CALCULATE THE EULER ANGLE
            alfa = 0.5d0*(rij(1,1)+rij(2,2)+rij(3,3)-1)
            if (alfa .gt. 1.d0) alfa=1.d0
            if (alfa .lt. -1.d0) alfa=-1.d0
            alfa = acos(alfa)
            zr(jeuler-1+7*(i-1-1)+1) = alfa
!
!           CALCULATE THE EULER VECTOR
            if ((alfa.gt.t0) .and. (alfa.lt.t180)) then
                zr(jeuler-1+7*(i-1-1)+2) = 0.5d0*(rij(2,3)-rij(3,2))/ sin(alfa)
                zr(jeuler-1+7*(i-1-1)+3) = 0.5d0*(rij(3,1)-rij(1,3))/ sin(alfa)
                zr(jeuler-1+7*(i-1-1)+4) = 0.5d0*(rij(1,2)-rij(2,1))/ sin(alfa)
            endif
!
        endif
!
    end do
!
! ***************************************************************
! ELABORATE EACH NODE IN THE MESH IN ORDER TO CALCULATE THE FOLLOWING:
! - PROPAGATION SPEED VECTOR
! - LOCAL REFERENCE SYSTEM (NORMAL AND TANGENTIAL AXES WITH RESPECT
!                           TO THE CRACK PLANE)
! - DISTANCE VECTOR BETWEEN THE NODE AND ITS PROJECTION ON THE FRONT
! ***************************************************************
!
!     ***************************************************************
!     EVALUATE THE PROJECTION OF EACH NODE ON THE CRACK FRONT
!     ***************************************************************
!
!     THESE ARE THE VALUES FOR THE 2D CASE. IN FACT ONLY FOR THE 3D CASE
!     THE PROJECTION OF THE VELOCITY IS NECESSARY. THE COORDINATES OF
!     THE CRACK TIP (ONLY ONE POINT!) FOR THE 2D CASE ARE STORED
!     IN XI1,YI1,ZI1.
    jmin = 1
    smin = 0.d0
!
!     BOUCLE SUR LES NOEUDS M DU MAILLAGE POUR CALCULER PROJ(V)=V
    eps = 1.d-12
    do i = 1, nbno
!
!        COORD DU NOEUD M DU MAILLAGE
        xm=zr(jcoor-1+(i-1)*3+1)
        ym=zr(jcoor-1+(i-1)*3+2)
        zm=zr(jcoor-1+(i-1)*3+3)
!
!        THE PROJ(V)=V IS NEEDED ONLY FOR THE 3D CASE
        if (ndim .eq. 3) then
!          INITIALISATION
            dmin = r8maem()
            jmin = 0
            smin = 0.d0
!          BOUCLE SUR PT DE FONFIS
            do j = 1, nbptff-1
!
                if (.not.fvirtu) then
!               CHECK IF THE CURRENT SEGMENT ON THE FRONT IS OUTSIDE
!               THE MODEL (ONLY IF THERE ARE MORE THAN ONE PIECE
!               FORMING THE FRONT)
                    do fon = 1, numfon
                        if (j .eq. zi(jfmult-1+2*fon)) goto 210
                    end do
                endif
!
!            COORD PT I, ET J
                xi1 = zr(jfonf-1+4*(j-1)+1)
                yi1 = zr(jfonf-1+4*(j-1)+2)
                zi1 = zr(jfonf-1+4*(j-1)+3)
                xj1 = zr(jfonf-1+4*(j-1+1)+1)
                yj1 = zr(jfonf-1+4*(j-1+1)+2)
                zj1 = zr(jfonf-1+4*(j-1+1)+3)
!            VECTEUR IJ ET IM
                xij = xj1-xi1
                yij = yj1-yi1
                zij = zj1-zi1
                xim = xm-xi1
                yim = ym-yi1
                zim = zm-zi1
!
!            PARAM S (PRODUIT SCALAIRE...)
                s = xij*xim + yij*yim + zij*zim
                norm2 = xij*xij + yij*yij + zij*zij
                s = s/norm2
!            SI N=P(M) SORT DU SEGMENT
                if ((s-1) .ge. eps) s = 1.d0
                if (s .le. eps) s = 0.d0
!            COORD DE N
                xn = s*xij+xi1
                yn = s*yij+yi1
                zn = s*zij+zi1
!            DISTANCE MN
!            SAVE CPU TIME: THE SQUARE OF THE DISTANCE IS EVALUATED!
                d = (xn-xm)*(xn-xm)+(yn-ym)*(yn-ym)+(zn-zm)*(zn-zm)
                if (d .lt. dmin) then
                    dmin = d
                    jmin = j
                    smin = s
!              STORE THE DISTANCE VECTOR
                    zr(jdis-1+3*(i-1)+1) = xm-xn
                    zr(jdis-1+3*(i-1)+2) = ym-yn
                    zr(jdis-1+3*(i-1)+3) = zm-zn
                endif
210             continue
            end do
!
        else
!
            dmin = r8maem()
!
!            2D CASE - ONLY ONE POINT AT THE CRACK TIP!
            do j = 1, nbptff
!
                xi1 = zr(jfonf-1+4*(j-1)+1)
                yi1 = zr(jfonf-1+4*(j-1)+2)
                zi1 = zr(jfonf-1+4*(j-1)+3)
!
!               SAVE CPU TIME: THE SQUARE OF THE DISTANCE IS EVALUATED!
                d = (xi1-xm)*(xi1-xm)+(yi1-ym)*(yi1-ym)+ (zi1-zm)*( zi1-zm)
                if (d .lt. dmin) then
                    dmin = d
                    jmin = j
!                 STORE THE DISTANCE VECTOR
                    zr(jdis-1+2*(i-1)+1) = xm-xi1
                    zr(jdis-1+2*(i-1)+2) = ym-yi1
!
!                 STORE THE PROJECTED POINT
                    zr(jlistp-1+3*(i-1)+1) = xi1
                    zr(jlistp-1+3*(i-1)+2) = yi1
                    zr(jlistp-1+3*(i-1)+3) = zi1
                endif
!
            end do
!
        endif
!
!        ***************************************************************
!        SMOOTH THE PROJECTION OF THE VELOCITY
!        ***************************************************************
!
        if (ndim .eq. 3) then
!
!           MAXIMUM NUMBER OF ITERATIONS
            maxite=25
!           INITIAL VALUE FOR DS
            ds= 2.0d-1
!           TOLERANCE TO CHECK THE CONVERGENCE
            tolld = 1.0d-2*lcmin
!           POINT PROJECTED ON ONE END OF THE FRONT FLAG
            endpnt = .false.
!
!           CALCULATE THE LIMITS FOR JMIN ON THE ACTUAL CRACK FRONT
            if (.not.fvirtu) then
                jlimsx = 0
                jlimdx = 0
                do fon = 1, numfon
                    if ((jmin.ge.zi(jfmult-1+2*fon-1)) .and. ( jmin.le.zi(jfmult-1+2*fon))) then
                        jlimsx = zi(jfmult-1+2*fon-1)
                        jlimdx = zi(jfmult-1+2*fon)-1
                        goto 204
                    endif
                end do
            else
                jlimsx = 1
                jlimdx = zi(jfmult-1+2*numfon)-1
            endif
!
            ASSERT(2.gt.1)
!
204         continue
!
!           SEARCH THE PROJECTED POINT BY THE BISECTION METHOD
            do j = 1, maxite
!
!              COORDINATES OF THE POINT AT THE END OF THE CRACK FRONT
!              SEGMENT
                xi1 = zr(jfonf-1+4*(jmin-1)+1)
                yi1 = zr(jfonf-1+4*(jmin-1)+2)
                zi1 = zr(jfonf-1+4*(jmin-1)+3)
                xj1 = zr(jfonf-1+4*(jmin-1+1)+1)
                yj1 = zr(jfonf-1+4*(jmin-1+1)+2)
                zj1 = zr(jfonf-1+4*(jmin-1+1)+3)
!
!              VECTEUR IJ
                xij = xj1-xi1
                yij = yj1-yi1
                zij = zj1-zi1
!
!              RETREIVE THE LENGTH OF THE CRACK FRONT SEGMENT
                norm2 = sqrt(xij*xij + yij*yij + zij*zij)
!
!              COORD DU NOEUD M DU MAILLAGE
                xm=zr(jcoor-1+(i-1)*3+1)
                ym=zr(jcoor-1+(i-1)*3+2)
                zm=zr(jcoor-1+(i-1)*3+3)
!
!              CALCULATE THE EULER ANGLE FOR THE NODE
                alfa = zr(jeuler-1+7*(jmin-1)+1)*smin
!
                if ((alfa.gt.t0) .and. (alfa.lt.t180)) then
!
!                 CALCULATE COS(ALFA) AND SIN(ALFA) TO SPEED UP THE CODE
                    calfa = cos(alfa)
                    salfa = sin(alfa)
!
!                 RETRIEVE THE EULER AXIS
                    axeul(1) = zr(jeuler-1+7*(jmin-1)+2)
                    axeul(2) = zr(jeuler-1+7*(jmin-1)+3)
                    axeul(3) = zr(jeuler-1+7*(jmin-1)+4)
!
!                 RETRIEVE THE LOCAL BASE IN THE PREVIOUS POINT ON THE
!                 FRONT (SMIN=0)
                    ni(1) = zr(jbasef-1+2*ndim*(jmin-1)+1)
                    ni(2) = zr(jbasef-1+2*ndim*(jmin-1)+2)
                    ni(3) = zr(jbasef-1+2*ndim*(jmin-1)+3)
                    ti(1) = zr(jbasef-1+2*ndim*(jmin-1)+4)
                    ti(2) = zr(jbasef-1+2*ndim*(jmin-1)+5)
                    ti(3) = zr(jbasef-1+2*ndim*(jmin-1)+6)
                    bi(1) = zr(jeuler-1+7*(jmin-1)+5)
                    bi(2) = zr(jeuler-1+7*(jmin-1)+6)
                    bi(3) = zr(jeuler-1+7*(jmin-1)+7)
!
!                 CALCULATE THE LOCAL BASE IN THE NODE WITH RESPECT TO
!                 THELOCAL BASE OF THE PREVIOUS POINT ON THE CRACK FRONT
!                 (SMIN=0)
                    bpl(1) = (1-calfa)*axeul(1)*axeul(3)-axeul(2)* salfa
                    bpl(2) = (1-calfa)*axeul(2)*axeul(3)+axeul(1)* salfa
                    bpl(3) = calfa+(1-calfa)*axeul(3)**2
!
!                 CALCULATE THE LOCAL BASE IN THE NODE WITH RESPECT TO
!                 THE GLOBAL REFERENCE SYSTEM OF THE MESH
                    b(1) = bpl(1)*ti(1)+bpl(2)*ni(1)+bpl(3)*bi(1)
                    b(2) = bpl(1)*ti(2)+bpl(2)*ni(2)+bpl(3)*bi(2)
                    b(3) = bpl(1)*ti(3)+bpl(2)*ni(3)+bpl(3)*bi(3)
!
!                 CALCULATE THE UNIT VECTOR FOR THE NORMAL AXIS
                    modvec = (b(1)**2+b(2)**2+b(3)**2)**0.5d0
!
                    ASSERT(modvec.gt.r8prem())
!
                    b(1) = b(1)/modvec
                    b(2) = b(2)/modvec
                    b(3) = b(3)/modvec
!
                else
!
                    if (alfa .lt. t0) then
                        b(1) = zr(jeuler-1+7*(jmin-1)+5)
                        b(2) = zr(jeuler-1+7*(jmin-1)+6)
                        b(3) = zr(jeuler-1+7*(jmin-1)+7)
                    endif
!
                    if (alfa .gt. t180) then
                        b(1) = zr(jeuler-1+7*(jmin-1+1)+5)
                        b(2) = zr(jeuler-1+7*(jmin-1+1)+6)
                        b(3) = zr(jeuler-1+7*(jmin-1+1)+7)
                    endif
!
                endif
!
!              COORD DE N
                xn = smin*xij+xi1
                yn = smin*yij+yi1
                zn = smin*zij+zi1
!
!              DISTANCE OF POINT M TO THE PLANE N-T
                d=(xm-xn)*b(1)+(ym-yn)*b(2)+(zm-zn)*b(3)
!
                if (abs(d) .lt. tolld) goto 207
!
!              INJECTION OF THE GOOD DPREC FOR THE FIRST ITERATION
                if (j .eq. 1) then
                    ds=ds*sign(1.d0,d)* sign(1.d0,(b(1)*xij+b(2)*yij+&
                    b(3)*zij))
                    dprec=d
                endif
!
!              CHANGE IN THE SEARCH DIRECTION
                if ((d*dprec) .lt. 0.d0) ds=ds*(-0.5d0)
!
!              UPDATE THE PROJECTED POINT POSITION ON THE FRONT
                smin=smin+ds
!
!              MANAGE THE CHANGING OF THE CRACK FRONT SEGMENT
                if ((smin.lt.0.d0) .and. (jmin.gt.jlimsx)) then
                    jmin=jmin-1
                    smin=1.d0
                else if ((smin.lt.0.d0).and.(jmin.eq.jlimsx)) then
                    smin=0.d0
                    endpnt = .true.
                    goto 207
                endif
!
                if ((smin.gt.1.d0) .and. (jmin.lt.jlimdx)) then
                    jmin=jmin+1
                    smin=0.d0
                else if ((smin.gt.1.d0).and.(jmin.eq.jlimdx)) then
                    smin=1.d0
                    endpnt = .true.
                    goto 207
                endif
!
                dprec=d
!
            end do
!
207         continue
!
!           CALCULATE THE PROJECTED POINT COORDINATES
            xn = smin*xij+xi1
            yn = smin*yij+yi1
            zn = smin*zij+zi1
            d = (xn-xm)*(xn-xm)+(yn-ym)*(yn-ym)+(zn-zm)*(zn-zm)
            dmin = d
!
!           STORE THE COORDINATES OF THE PROJECTED POINT
            zr(jlistp-1+3*(i-1)+1) = xn
            zr(jlistp-1+3*(i-1)+2) = yn
            zr(jlistp-1+3*(i-1)+3) = zn
!
!           STORE THE DISTANCE VECTOR
            zr(jdis-1+3*(i-1)+1) = xm-xn
            zr(jdis-1+3*(i-1)+2) = ym-yn
            zr(jdis-1+3*(i-1)+3) = zm-zn
            zr(jdisfr+i-1) = dmin
!
!          SI ON UTILISE LA METHODE UPWIND AVEC UNE GRILLE ET QU'IL
!          Y A PLUSIEURS FOND DE FISSURE, ON REACALCULE LES
!          LEVELS SETS
            if (fvirtu .and. (numfon.gt.1)) then
                fonvir=.false.
!
!        1: ON DETERMINE SI LE NOEUDS EST PROJETE SUR UN SEGMENT DU
!            FRONT VIRTUEL A L'INTERIEUR D'UN TROU (SEGMENT DU FRONT
!            VIRTUEL DE TYPES 2 ET 3)
!
                do k = 1, (numfon-1)
                    if ((jmin.eq.(zi(nfv+2*k-1)-1)) .or. (jmin.eq.(zi( nfv+2*k)))) then
                        fonvir=.true.
                    else
                        goto 861
                    endif
861                 continue
                end do
!
!         2: SI OUI, ON CALCULE LA CORRECTION A APPORTER
!
                if (fonvir) then
!          CALCUL DE LA VALEUR DE LA LEVEL SET NORMALE RECHERCHEE
                    lsnth(1)= zr(jdis-1+3*(i-1)+1)*zr(jbasef+6*(jmin-&
                    1)-1+1) +zr(jdis-1+3*(i-1)+2)*zr(jbasef+6*(jmin-1)&
                    -1+2) +zr(jdis-1+3*(i-1)+3)*zr(jbasef+6*(jmin-1)-&
                    1+3)
!
!          CALCUL DE LA VALEUR DE LA LEVEL SET TANGENTEE RECHERCHEE
                    lstth(1)= zr(jdis-1+3*(i-1)+1)*zr(jbasef+6*&
                    (jmin-1)-1+4)+zr(jdis-1+3*(i-1)+2)*zr(jbasef+6*(&
                    jmin-1)-1+5) +zr(jdis-1+3*(i-1)+3)*zr(jbasef+6*(&
                    jmin-1)-1+6)
!
!          CALCUL DE LA CORRECTION A APPORTER
                    zr(jdelta+2*(i-1))= lsnth(1)-zr(lsn+i-1)
                    zr(jdelta+2*(i-1)+1)= lstth(1)-zr(lst+i-1)
                endif
!
            endif
!
!           STORE THE DISTANCE VECTOR
!
!           attention: la distance du point au fond de fissure doit
!           etre calcule depuis le front de fissure reel et non le
!           front de fissure virtuel
            if (fvirtu) then
!
                if (jmin .eq. 1) then
!           Le projete est sur un segment virtuel du fond de
!           fissure (segment1)
                    pi(1)= xm-zr(cfv+4-1+1)
                    pi(2) = ym-zr(cfv+4-1+2)
                    pi(3) = zm-zr(cfv+4-1+3)
                    zr(jdisfr+i-1) = pi(1)**2+pi(2)**2+pi(3)**2
                else if (jmin.eq.(nbptff-1)) then
                    pi(1) = xm-zr(cfv+4*(nbptff-2)-1+1)
                    pi(2) = ym-zr(cfv+4*(nbptff-2)-1+2)
                    pi(3) = zm-zr(cfv+4*(nbptff-2)-1+3)
                    zr(jdisfr+i-1) = pi(1)**2+pi(2)**2+pi(3)**2
                else
                    do k = 1, (numfon-1)
!            Le point est projete sur un segment virtuel de type 1
                        if ((jmin) .eq. (zi(nfv+2*k-1)-1)) then
                            pi(1) = xm-zr(cfv-1+4*(jmin-1)+1)
                            pi(2) = ym-zr(cfv-1+4*(jmin-1)+2)
                            pi(3) = zm-zr(cfv-1+4*(jmin-1)+3)
                            zr(jdisfr+i-1) = pi(1)**2+pi(2)**2+pi(3)** 2
                            goto 61
                        else if ((jmin).eq.(zi(nfv+2*k))) then
!              Le point est projete sur un segment virtuel de type 3
                            pi(1) = xm-zr(cfv-1+4*(jmin-0)+1)
                            pi(2) = ym-zr(cfv-1+4*(jmin-0)+2)
                            pi(3) = zm-zr(cfv-1+4*(jmin-0)+3)
                            zr(jdisfr+i-1) = pi(1)**2+pi(2)**2+pi(3)** 2
                        else if ((jmin).eq.(zi(nfv+2*k-1))) then
!              Le point est projete sur un segment virtuel de type 2
                            normkl=(xm-zr(cfv-1+4*(jmin+1)+1))**2&
                            +(ym-zr(cfv-1+4*(jmin+1)+2))**2 +(zm-zr(&
                            cfv-1+4*(jmin+1)+3))**2
!
                            normij=(xm-zr(cfv-1+4*(jmin-2)+1))**2&
                            +(ym-zr(cfv-1+4*(jmin-2)+2))**2 +(zm-zr(&
                            cfv-1+4*(jmin-2)+3))**2
!
                            if (normkl .ge. normij) then
!                   On est plus proche du fond de fissure a gauche
                                pi(1) = xm-zr(cfv-1+4*(jmin-2)+1)
                                pi(2) = ym-zr(cfv-1+4*(jmin-2)+2)
                                pi(3) = zm-zr(cfv-1+4*(jmin-2)+3)
                                zr(jdisfr+i-1) = pi(1)**2+pi(2)**2+pi( 3)**2
                            else
!                   On est plus proche du fond de fissure a droite
                                pi(1) = xm-zr(cfv-1+4*(jmin+1)+1)
                                pi(2) = ym-zr(cfv-1+4*(jmin+1)+2)
                                pi(3) = zm-zr(cfv-1+4*(jmin+1)+3)
                                zr(jdisfr+i-1) = pi(1)**2+pi(2)**2+pi( 3)**2
                            endif
                            goto 61
                        endif
 61                     continue
                    end do
                endif
            endif
!
        endif
!
!        ***************************************************************
!        EVALUATE THE LOCAL REFERENCE SYSTEM IN THE NODE
!        ***************************************************************
!
        if (ndim .eq. 2) then
!
!           IN THE 2D CASE THERE'S NO NEED TO CALCULATE THE INTERMEDIATE
!           LOCAL BASE
!           N-AXIS
            zr(jbl-1+2*ndim*(i-1)+1) = zr(jbasef-1+2*ndim*(jmin-1)+1)
            zr(jbl-1+2*ndim*(i-1)+2) = zr(jbasef-1+2*ndim*(jmin-1)+2)
!           T-AXIS
            zr(jbl-1+2*ndim*(i-1)+3) = zr(jbasef-1+2*ndim*(jmin-1)+ ndim+1)
            zr(jbl-1+2*ndim*(i-1)+4) = zr(jbasef-1+2*ndim*(jmin-1)+ ndim+2)
!
        else
!
!           CALCULATE THE EULER ANGLE FOR THE NODE
            alfa = zr(jeuler-1+7*(jmin-1)+1)*smin
!
            if ((alfa.gt.t0) .and. (alfa.lt.t180)) then
!
!              CALCULATE COS(ALFA) AND SIN(ALFA) TO SPEED UP THE CODE
                calfa = cos(alfa)
                salfa = sin(alfa)
!
!              RETRIEVE THE EULER AXIS
                axeul(1) = zr(jeuler-1+7*(jmin-1)+2)
                axeul(2) = zr(jeuler-1+7*(jmin-1)+3)
                axeul(3) = zr(jeuler-1+7*(jmin-1)+4)
!
!             RETRIEVE THE LOCAL BASE IN THE PREVIOUS POINT ON THE FRONT
!              (SMIN=0)
                ni(1) = zr(jbasef-1+2*ndim*(jmin-1)+1)
                ni(2) = zr(jbasef-1+2*ndim*(jmin-1)+2)
                ni(3) = zr(jbasef-1+2*ndim*(jmin-1)+3)
                ti(1) = zr(jbasef-1+2*ndim*(jmin-1)+4)
                ti(2) = zr(jbasef-1+2*ndim*(jmin-1)+5)
                ti(3) = zr(jbasef-1+2*ndim*(jmin-1)+6)
                bi(1) = zr(jeuler-1+7*(jmin-1)+5)
                bi(2) = zr(jeuler-1+7*(jmin-1)+6)
                bi(3) = zr(jeuler-1+7*(jmin-1)+7)
!
!              CALCULATE THE LOCAL BASE IN THE NODE WITH RESPECT TO THE
!              LOCAL BASE OF THE PREVIOUS POINT ON THE CRACK FRONT
!              (SMIN=0)
                tpl(1) = calfa+(1-calfa)*axeul(1)**2
                tpl(2) = (1-calfa)*axeul(1)*axeul(2)-axeul(3)*salfa
                tpl(3) = (1-calfa)*axeul(1)*axeul(3)+axeul(2)*salfa
                npl(1) = (1-calfa)*axeul(1)*axeul(2)+axeul(3)*salfa
                npl(2) = calfa+(1-calfa)*axeul(2)**2
                npl(3) = (1-calfa)*axeul(2)*axeul(3)-axeul(1)*salfa
!
!              CALCULATE THE LOCAL BASE IN THE NODE WITH RESPECT TO THE
!              GLOBAL REFERENCE SYSTEM OF THE MESH
                t(1) = tpl(1)*ti(1)+tpl(2)*ni(1)+tpl(3)*bi(1)
                t(2) = tpl(1)*ti(2)+tpl(2)*ni(2)+tpl(3)*bi(2)
                t(3) = tpl(1)*ti(3)+tpl(2)*ni(3)+tpl(3)*bi(3)
                n(1) = npl(1)*ti(1)+npl(2)*ni(1)+npl(3)*bi(1)
                n(2) = npl(1)*ti(2)+npl(2)*ni(2)+npl(3)*bi(2)
                n(3) = npl(1)*ti(3)+npl(2)*ni(3)+npl(3)*bi(3)
!
!              CALCULATE THE UNIT VECTOR FOR THE TANGENTIAL AND NORMAL
!              AXIS (THEIR MODULE COULD BE SLIGHTLY DIFFERENT THAN 1 DUE
!              TO NUMERICAL APPROXIMATION)
!              N-AXIS
                modvec = (t(1)**2+t(2)**2+t(3)**2)**0.5d0
!
                ASSERT(modvec.gt.r8prem())
!
                zr(jbl-1+2*ndim*(i-1)+1) = n(1)/modvec
                zr(jbl-1+2*ndim*(i-1)+2) = n(2)/modvec
                zr(jbl-1+2*ndim*(i-1)+3) = n(3)/modvec
!              T-AXIS
                modvec = (n(1)**2+n(2)**2+n(3)**2)**0.5d0
!
                ASSERT(modvec.gt.r8prem())
!
                zr(jbl-1+2*ndim*(i-1)+4) = t(1)/modvec
                zr(jbl-1+2*ndim*(i-1)+5) = t(2)/modvec
                zr(jbl-1+2*ndim*(i-1)+6) = t(3)/modvec
!
            else
!
                if (alfa .gt. t180) jmin=jmin+1
!
!              N-AXIS
                zr(jbl-1+2*ndim*(i-1)+1) = zr(jbasef-1+2*ndim*(jmin-1) +1)
                zr(jbl-1+2*ndim*(i-1)+2) = zr(jbasef-1+2*ndim*(jmin-1) +2)
                zr(jbl-1+2*ndim*(i-1)+3) = zr(jbasef-1+2*ndim*(jmin-1) +3)
!              T-AXIS
                zr(jbl-1+2*ndim*(i-1)+4) = zr(jbasef-1+2*ndim*(jmin-1) +ndim+1)
                zr(jbl-1+2*ndim*(i-1)+5) = zr(jbasef-1+2*ndim*(jmin-1) +ndim+2)
                zr(jbl-1+2*ndim*(i-1)+6) = zr(jbasef-1+2*ndim*(jmin-1) +ndim+3)
!
            endif
!
!           CORRECTION OF THE LOCAL BASE FOR THE POINTS PROJECTED ON
!           ONE END OF THE CRACK FRONT
            if ((method.eq.'GEOMETRI') .and. endpnt) then
!
!              NORMAL AXIS OF THE LOCAL BASE
                n(1) = zr(jbl-1+2*ndim*(i-1)+1)
                n(2) = zr(jbl-1+2*ndim*(i-1)+2)
                n(3) = zr(jbl-1+2*ndim*(i-1)+3)
!
!              Q->P
                modvec =(xm-xn)*n(1)+(ym-yn)*n(2)+(zm-zn)*n(3)
                bi(1) = modvec*n(1)
                bi(2) = modvec*n(2)
                bi(3) = modvec*n(3)
!
!              NEW T-AXIS
                t(1) = (xm-xn)-bi(1)
                t(2) = (ym-yn)-bi(2)
                t(3) = (zm-zn)-bi(3)
!
!              CHECK THE DIRECTION OF THE NEW T-AXIS WITH RESPECT TO THE
!              ORIGINAL T-AXIS
                modvec=zr(jbl-1+2*ndim*(i-1)+4)*t(1)+ zr(jbl-1+2*ndim*&
                (i-1)+5)*t(2)+ zr(jbl-1+2*ndim*(i-1)+6)*t(3)
                if (modvec .lt. 0.d0) then
!                 MODULUS OF THE NEW T-AXIS. ITS DIRECTION MUST BE
!                 CHANGED (-1)
                    modvec = -1*(t(1)**2+t(2)**2+t(3)**2)**0.5d0
                else
!                 MODULUS OF THE NEW T-AXIS. ITS DIRECTION IS OK.
                    modvec = (t(1)**2+t(2)**2+t(3)**2)**0.5d0
                endif
!
!              STORE THE NEW T-AXIS (THE CORRECT DIRECTION IS DETERMINED
!              BY THE SIGN OF MODVEC)
                zr(jbl-1+2*ndim*(i-1)+4) = t(1)/modvec
                zr(jbl-1+2*ndim*(i-1)+5) = t(2)/modvec
                zr(jbl-1+2*ndim*(i-1)+6) = t(3)/modvec
!
            endif
!
            if (fvirtu .and. (numfon.gt.1)) then
!           Calcul de la correction a apporter au level set pour les
!           vecteurs projetes sur le front virtuel( SEGMENT DE TYPE 2)
!
                do k = 1, (numfon-1)
                    if ((jmin) .eq. (zi(nfv+2*k-1))) then
!
                        lsnth(1)= zr(jdis-1+3*(i-1)+1)*zr(jbl-1+2*&
                        ndim*(i-1)+1) +zr(jdis-1+3*(i-1)+2)*zr(jbl-1+&
                        2*ndim*(i-1)+2) +zr(jdis-1+3*(i-1)+3)*zr(jbl-&
                        1+2*ndim*(i-1)+3)
!
                        lstth(1)= zr(jdis-1+3*(i-1)+1)*zr(jbl-1+2*&
                        ndim*(i-1)+4) +zr(jdis-1+3*(i-1)+2)*zr(jbl-1+&
                        2*ndim*(i-1)+5) +zr(jdis-1+3*(i-1)+3)*zr(jbl-&
                        1+2*ndim*(i-1)+6)
!
                        zr(jdelta+2*(i-1))= lsnth(1)-zr(lsn+i-1)
                        zr(jdelta+2*(i-1)+1)= lstth(1)-zr(lst+i-1)
!
                    endif
                end do
            endif
        endif
!
!        ***************************************************************
!        EVALUATE THE NORM. AND TANG. COMPONENTS OF THE PROPAGATION
!        SPEED IN THE NODE WITH RESPECT TO THE LOCAL REFERENCE SYSTEM
!        ***************************************************************
!
        if (ndim .eq. 2) then
!
            betap=zr(jbeta-1+jmin)
            vp=zr(jvit-1+jmin)
!
        else if (ndim.eq.3) then
!
            betap=(zr(jbeta-1+jmin+1)-zr(jbeta-1+jmin))*smin+ zr(&
            jbeta-1+jmin)
!
            vp=(zr(jvit-1+jmin+1)-zr(jvit-1+jmin))*smin+zr(jvit-1+&
            jmin)
!
        endif
!
        zr(jvnv+i-1)=vp*sin(betap)
        zr(jvtv+i-1)=vp*cos(betap)
        zl(jvtl+i-1) = .true.
        zl(jvnl+i-1) = .true.
!
!        STORE THE NORM OF THE PROPAGATION VELOCITY AT THE PROJECTED
!        POINT
        zr(jvp+i-1) = vp
!
        zr(jdisfr+i-1) = dmin
!
!        STORE THE PROPAGATION ANGLE AT THE PROJECTED POINT
        zr(jcnsb+i-1) = betap
!
!
    end do
!
! ***************************************************************
! PRINT SOME INFORMATIONS
! ***************************************************************
!
!  IMPRESSION DES VITESSES DE PROPAGATION EN INFO=2
    if (niv .ge. 1) then
        write(ifm,*) ' '
        write(ifm,*) 'VITESSE DE PROPAGATION EN FOND DE FISSURE'
!
        do i = 1, nbptff
!
            do j = 1, numfon
!
                if (i .eq. zi(jfmult-1+2*j-1)) then
                    write(ifm,*) ' '
                    if (numfon .gt. 1) write(ifm,313) j
                    write(ifm,*)  ' NUM_PT    VITESSE         '&
     &               //'BETA          VT            VN'
                endif
!
            end do
!
            write(ifm,311) i,zr(jvit-1+i),zr(jbeta-1+i),zr(jvtff+i-1),&
            zr(jvnff+i-1)
!
        end do
!
        311     format(4x,i2,4x,4(1pd12.5,3x))
        313     format(1x,' FOND DE FISSURE ',i2)
!
    endif
!
!
    if (fvirtu) then
        nbptff=nbptff-2*numfon
        call jedetr(covir)
        call jedetr(bavir)
        call jedetr(vitvir)
        call jedetr(angvir)
        call jedetr(numvir)
    endif
!
    call jedetr('&&XPRVIT.V_PROPA_FF')
    call jedetr('&&XPRVIT.VT_PROPA_FF')
    call jedetr('&&XPRVIT.VN_PROPA_FF')
    call jedetr('&&XPRVIT.EULER')
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
