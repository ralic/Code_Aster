subroutine xprdom(dnoma, dcnxin, disfr, noma, cnxinv,&
                  fiss, damax, ndomp, edomg, radtor)
    implicit none
!
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeundf.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: dnoma, fiss, noma
    character(len=19) :: dcnxin, disfr, cnxinv, ndomp, edomg
    real(kind=8) :: damax, radtor
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
! person_in_charge: daniele.colombo at ifpen.fr
!
!     ------------------------------------------------------------------
!
!       XPRDOM   : X-FEM PROPAGATION : DEFINITION DES DOMAINES POUR LA
!       ------     -     --                           ---
!                  PROJECTION DES LEVEL SETS
!
!  DANS LE CADRE DE LA PROPAGATION X-FEM, SI UNE GRILLE AUXILIAIRE EST
!  UTILISEE, ON DOIT DEFINIR LES DOMAINES DE PROJECTION A LA FOIS SUR
!  LE MAILLAGE PHYSIQUE ET SUR LA GRILLE AUXILIAIRE.
!
!  ENTREE
!  ------
!
!    * MODELE POUR LA REPRESENTATION DES LEVEL SETS (GRILLE AUXILIAIRE)
!      ----------------------------------------------------------------
!     DNOMA  = NOM DU MAILLAGE
!     DCNXIN = CONNECTIVITE INVERSEE DU MAILLAGE DNOMA
!     DISFR  = NOM DU VECTEUR JEVEUX OU LES DISTANCE^2 ENTRE LES NOEUDS
!              DU MAILLAGE DNOMA ET LE FOND DU FISSURE SONT STOCKEES.
!              CE VECTEUR EST DONNE PAR XPRVIT.F.
!
!
!
!    * MODELE PHYSIQUE
!      ---------------
!     NOMA   = NOME DU MAILLAGE
!     CNXINV = CONNECTIVITE INVERSEE DU MAILLAGE NOMA
!
!
!     FISS   = NOM DU FISSURE AVANT LA PHASE DE MISE A JOUR DES LEVEL
!              SETS
!     DAMAX  = AVANCEE MAXIMALE DE LA FISSURE
!
!
!  SORTIE
!  ------
!
!    * MODELE POUR LA REPRESENTATION DES LEVEL SETS (GRILLE AUXILIAIRE)
!      ----------------------------------------------------------------
!     EDOMG  = NOM DU VECTEUR A CREER POUR STOCKER LA LISTE DU NUMERO
!              DES ELEMENTS DU TORE POUR LA GRILLE AUXILIAIRE
!
!    * MODELE PHYSIQUE
!      ---------------
!     NDOMP  = NOM DU VECTEUR A CREER POUR STOCKER LA LISTE DU NUMERO
!              DES NOEUDS DU TORE POUR LE MODELE PHYSIQUE
!
!     RADTOR = RAYON DU TORE QUI DEFINIT LE DOMAINE
!
!     ------------------------------------------------------------------
!
!
!     CHARACTERISTICS OF THE MESHES
    integer :: dnbno, dnbma, jtmdim
!
!     DOMAINE LEVEL SETS MESH (AUXILIARY GRID)
    real(kind=8) :: rayon
    character(len=24) :: econs
    integer :: jecons, jdisfr, nbelno, jelno, nbelpr, jefrom, nodadj
!
!     DOMAINE PHYSICAL MESH
    integer :: jfonf, nbptff, nbno, jcoor, jnto, nunopr, jnofla, jdist, node
    integer :: numelm, jmai, itypma, jconx1, jconx2, ndim, jaux, jnofl1, jdmai
    integer :: eldim
    real(kind=8) :: eps, xm, ym, zm, xi1, yi1, zi1, xj1, yj1, zj1, xij, yij, zij
    real(kind=8) :: xim, yim, zim, s, norm2, xn, yn, zn, d, dmin
    character(len=24) :: nodfla, nodfl1, distno
!
!     GENERAL PURPOSE
    integer :: iret, i, j, k
    character(len=8) :: k8b
    integer :: ifm, niv
!
!     MULTIPLE CRACK FRONTS
    integer :: jfmult, numfon, fon
    integer :: iarg
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
!     CALCULATE THE RADIUS OF THE TORUS AROUND THE CRACK FRONT USED TO
!     DETERMINE WHICH NODES MUST BE CONSIDERED FOR THE PROJECTION.
!     THIS VALUE WILL BE INCREASED BELOW IN ORDER TO INCLUDE ALL THE
!     ELEMENTS CUT BY THE BORDER OF THE TORE.
    call getvr8(' ', 'RAYON', scal=rayon, nbret=iret)
    radtor = (rayon+damax)**2
!
! ----------------------------------------------------------------------
!     PHYSICAL MESH: RETREIVE THE NODES INVOLVED IN THE PROJECTION
! ----------------------------------------------------------------------
!
!     RETRIEVE THE NUMBER OF NODES IN THE MESH
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbno,&
                k8b, iret)
!
!     RETRIEVE THE TYPE OF EACH ELEMENT IN THE MESH
    call jeveuo(noma//'.TYPMAIL', 'L', jmai)
!
!     RETRIEVE THE DIMENSIONS OF THE EXISTING ELEMENTS
    call jeveuo('&CATA.TM.TMDIM', 'L', jtmdim)
!
!     RETRIEVE THE DEFINITION OF THE ELEMENTS IN TERMS OF NODES
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
!     RETRIEVE THE PROBLEM DIMENSION
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                k8b, iret)
!
!     RETRIEVE THE COORDINATES OF THE NODES
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
!     RETRIEVE THE POINTS ON THE CRACK FRONT
    call jeveuo(fiss//'.FONDFISS', 'L', jfonf)
    call dismoi('F', 'NB_POINT_FOND', fiss, 'FISS_XFEM', nbptff,&
                k8b, iret)
!
!     RETRIEVE THE DIFFERENT PIECES OF THE CRACK FRONT
    call jeveuo(fiss//'.FONDMULT', 'L', jfmult)
    call dismoi('F', 'NB_FOND', fiss, 'FISS_XFEM', numfon,&
                k8b, iret)
!
!     CREATE A TEMPORARY LOGICAL VECTOR TO MARK THE NODES THAT HAVE
!     BEEN SELECTED
    nodfla='&&XPRDOM.NODEFLAG'
    call wkvect(nodfla, 'V V L', nbno, jnofla)
    call jeundf(nodfla)
    call jeveuo(nodfla, 'E', jnofla)
!
!     CREATE A TEMPORARY VECTOR TO STORE THE DISTANCES OF EACH NODE
    distno='&&XPRDOM.DISTNO'
    call wkvect(distno, 'V V R8', nbno, jdist)
!
    xi1 = zr(jfonf-1+4*(1-1)+1)
    yi1 = zr(jfonf-1+4*(1-1)+2)
    zi1 = zr(jfonf-1+4*(1-1)+3)
!
!     COUNTER FOR THE NODES THAT HAVE BEEN SELECTED
    nunopr = 0
!
!     EACH NODE OF THE PHYSICAL MESH IS CONSIDERED
    eps = 1.d-12
    do 2000 i = 1, nbno
!
!        COORDINATES OF NODE M
        xm=zr(jcoor-1+(i-1)*3+1)
        ym=zr(jcoor-1+(i-1)*3+2)
        zm=zr(jcoor-1+(i-1)*3+3)
!
!        THE PROJECTION IS NEEDED ONLY FOR THE 3D CASE
        if (ndim .eq. 3) then
            dmin = r8maem()
!
!          CONSIDER EACH POINT OF THE CRACK FRONT
            do 2100 j = 1, nbptff-1
!
!            CHECK IF THE CURRENT SEGMENT ON THE FRONT IS OUTSIDE THE
!            MODEL (ONLY IF THERE ARE MORE THAN ONE PIECE FORMING THE
!            FRONT)
                do 2113 fon = 1, numfon
                    if (j .eq. zi(jfmult-1+2*fon)) goto 2100
2113              continue
!
!            COORD PT I, AND J
                xi1 = zr(jfonf-1+4*(j-1)+1)
                yi1 = zr(jfonf-1+4*(j-1)+2)
                zi1 = zr(jfonf-1+4*(j-1)+3)
                xj1 = zr(jfonf-1+4*(j-1+1)+1)
                yj1 = zr(jfonf-1+4*(j-1+1)+2)
                zj1 = zr(jfonf-1+4*(j-1+1)+3)
!            VECTORS IJ AND IM
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
!            DISTANCE MN**2
                d = (xn-xm)*(xn-xm)+(yn-ym)*(yn-ym)+(zn-zm)*(zn-zm)
                if (d .lt. dmin) then
                    dmin = d
                endif
!
2100          continue
!
        else
!
            dmin = r8maem()
!
!            2D CASE - ONLY ONE POINT AT THE CRACK TIP!
            do 2114 j = 1, nbptff
!
                xi1 = zr(jfonf-1+4*(j-1)+1)
                yi1 = zr(jfonf-1+4*(j-1)+2)
                zi1 = zr(jfonf-1+4*(j-1)+3)
!
!               SAVE CPU TIME: THE SQUARE OF THE DISTANCE IS EVALUATED!
                d = (xi1-xm)*(xi1-xm)+(yi1-ym)*(yi1-ym)+ (zi1-zm)*( zi1-zm)
!
                if (d .lt. dmin) dmin = d
!
2114          continue
!
        endif
!
!        STORE THE DISTANCE
        zr(jdist-1+i) = dmin
!
        if (dmin .le. radtor) then
!           THE NODE IS INSIDE THE TORUS AND THEREFORE IT IS STORED.
            nunopr = nunopr+1
            zl(jnofla-1+i) = .true.
        endif
!
2000  end do
!
    nodfl1='&&XPRDOM.NODEFLAG1'
    call jedupo(nodfla, 'V', nodfl1, .false.)
    call jeveuo(nodfl1, 'L', jnofl1)
!
!     ALL THE ELEMENTS CONTAINING THE SELECTED NODES MUST BE
!     CONSIDERED. THEIR NODES MUST BE SELECTED.
    nodadj=0
    do 1000 i = 1, nbno
!
        if (zl(jnofl1-1+i)) then
!
!        THE NODE IS INSIDE THE TORUS. RETRIEVE ALL THE ELEMENTS
!        CONTAINING THIS NODE
            call jelira(jexnum(cnxinv, i), 'LONMAX', nbelno)
            call jeveuo(jexnum(cnxinv, i), 'L', jelno)
!
            do 1150 j = 1, nbelno
!
                numelm=zi(jelno-1+j)
!
!           ONLY THE ELEMENTS OF THE SAME DIMENSION OF THE MODEL ARE
!           CONSIDERED
                itypma=zi(jmai-1+numelm)
                eldim=zi(jtmdim-1+itypma)
!
                if (eldim .eq. ndim) then
!
!              RETRIEVE THE NUMBER OF NODES FORMING THE ELEMENT
                    call jeveuo(jexnum('&CATA.TM.NBNO' , itypma), 'L', jaux)
!
!              RETRIEVE EACH NODE IN THE ELEMENT
                    do 1700 k = 1, zi(jaux)
!
                        node = zi(jconx1-1+zi(jconx2-1+numelm)+k-1)
!
                        if (.not.zl(jnofla-1+node)) then
                            nodadj = nodadj+1
                            zl(jnofla-1+node) = .true.
!
!                    UPDATE THE RADIUS OF THE TORUS
                            if (radtor .lt. zr(jdist-1+node)) then
                                radtor = zr(jdist-1+node)
                            endif
!
                        endif
!
1700                  continue
!
                endif
!
1150          continue
!
        endif
!
1000  end do
!
    call jedetr(nodfl1)
!
!     UPDATE THE NUMBER OF THE SELECTED NODES
    nunopr = nunopr+nodadj
!
!     CREATE THE LIST OF THE SELECTED NODES
    call wkvect(ndomp, 'V V I', nunopr, jnto)
!
!     TEMPORARY POINTER
    j=0
!
    do 1500 i = 1, nbno
!
        if (zl(jnofla-1+i)) then
            j=j+1
            ASSERT(j.le.nunopr)
            zi(jnto-1+j) = i
        endif
!
1500  end do
!
!     DESTROY THE TEMPORARY OBJECTS
    call jedetr(nodfla)
    call jedetr(distno)
!
! ----------------------------------------------------------------------
!     LEVEL SETS MESH: RETREIVE THE ELEMENTS THAT MUST BE USED FOR THE
!                      PROJECTION
! ----------------------------------------------------------------------
!
!     RETRIEVE THE NUMBER OF NODES AND ELEMENTS IN THE MESH
    call dismoi('F', 'NB_NO_MAILLA', dnoma, 'MAILLAGE', dnbno,&
                k8b, iret)
    call dismoi('F', 'NB_MA_MAILLA', dnoma, 'MAILLAGE', dnbma,&
                k8b, iret)
!
!     RETRIEVE THE TYPE OF EACH ELEMENT IN THE MESH
    call jeveuo(dnoma//'.TYPMAIL', 'L', jdmai)
!
!     RETRIEVE THE DEFINITION OF THE ELEMENTS IN TERMS OF NODES
    call jeveuo(dnoma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(dnoma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
!     RETRIEVE THE DISTANCE OF EACH NODE FROM THE FRONT
    call jeveuo(disfr, 'L', jdisfr)
!
!     THE NODES OF THE LEVEL SETS MESH THAT ARE INSIDE THE TORUS ARE
!     RETRIEVED. THE ELEMENTS CONTAINING EACH OF THESE NODES ARE
!     STORED.
    econs = '&&XPRDOM.ECONS'
    call wkvect(econs, 'V V L', dnbma, jecons)
!
    do 50 i = 1, dnbma
        zl(jecons-1+i) = .false.
50  end do
!
    do 100 i = 1, dnbno
!
        if (zr(jdisfr-1+i) .le. radtor) then
!
!           THE NODE IS INSIDE THE TORUS. RETRIEVE ALL THE ELEMENTS
!           CONTAINING THIS NODE
            call jelira(jexnum(dcnxin, i), 'LONMAX', nbelno)
            call jeveuo(jexnum(dcnxin, i), 'L', jelno)
!
            do 150 j = 1, nbelno
!
                numelm = zi(jelno-1+j)
!
!              ONLY THE ELEMENTS OF THE SAME DIMENSION OF THE MODEL ARE
!              CONSIDERED
                itypma = zi(jdmai-1+numelm)
                eldim = zi(jtmdim-1+itypma)
!
!              MARK THE SELECTED ELEMENT
                if (eldim .eq. ndim) zl(jecons-1+numelm) = .true.
!
150          continue
!
        endif
!
100  end do
!
!     COUNT THE NUMBER OF ELEMENTS INVOLVED IN THE PROJECTION
    nbelpr = 0
!
    do 200 i = 1, dnbma
        if (zl(jecons-1+i)) nbelpr = nbelpr+1
200  end do
!
!     STORE THE NUMBER OF THESE ELEMENTS ONLY. THESE INFORMATIONS ARE
!     USED FOR THE PROJECTION
    call wkvect(edomg, 'V V I', nbelpr, jefrom)
!
    nbelpr = 0
!
    do 300 i = 1, dnbma
!
        if (zl(jecons-1+i)) then
!
            nbelpr = nbelpr + 1
            zi(jefrom-1+nbelpr) = i
!
        endif
!
300  end do
!
    call jedetr(econs)
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
