subroutine xbaslo(noma, fiss, grlt, grln, ndim)
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnscre.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/imprsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: ndim
    character(len=8) :: noma, fiss
    character(len=19) :: grlt, grln
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (PREPARATION)
!
! CREATION D'UN CHAM_EL QUI CONTIENT LA BASE
! LOCALE AU POINT DU FOND DE FISSURE ASSOCIE - (2D/3D)
!
! ----------------------------------------------------------------------
!
!
! IN  FISS   : NOM DE LA FISSURE
! IN  NOMA   : NOM DU MAILLAGE
! IN  MODELE : NOM DE L'OBJET MODELE
! IN  GRLT   : CHAM_NO_S DES GRADIENTS DE LA LEVEL-SET TANGENTE
! IN  GRLN   : CHAM_NO_S DES GRADIENTS DE LA LEVEL-SET NORMALE
! IN  NDIM   : DIMENSION DU MAILLAGE
! OUT FISS   : FISSURE AVEC LE .BASLOC EN PLUS
!
!
!
!
    integer :: nbcmp
    character(len=8) :: licmp(9)
    character(len=24) :: coorn
    character(len=24) :: xfonfi
    integer :: ifon, npoint, ifm, niv, ier, ibid
    character(len=19) :: cnsbas, basloc
    integer :: iadrco, jgsv, jgsl, jgt, jgtl, jgn
    integer :: long, nfon, nbno, ino, j
    real(kind=8) :: xi1, yi1, zi1, xj1, yj1, zj1, xij, yij, zij, eps, d, norm2
    real(kind=8) :: xm, ym, zm, xim, yim, zim, s, dmin, xn, yn, zn, a(3)
!
    data licmp / 'X1','X2','X3',&
     &             'X4','X5','X6',&
     &             'X7','X8','X9'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
!
! --- CREATION DU CHAM_NO
!
    cnsbas = '&&OP0041.CNSBAS'
    nbcmp = ndim*3
    call cnscre(noma, 'NEUT_R', nbcmp, licmp, 'V',&
                cnsbas)
    call jeveuo(cnsbas//'.CNSV', 'E', jgsv)
    call jeveuo(cnsbas//'.CNSL', 'E', jgsl)
!
! --- ACCES AU MAILLAGE
!
    coorn = noma//'.COORDO    .VALE'
    call jeveuo(coorn, 'L', iadrco)
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
!
! --- ACCES AUX OBJETS: NOM DES POINTS DU FOND DE FISSURE
!
    xfonfi = fiss(1:8)//'.FONDFISS'
    call jeexin(xfonfi, ier)
    if (ier .eq. 0) then
!       LE FOND DE FISSURE N'EXISTE PAS (CAS D'UNE INTERFACE)
!       ON MET TOUT A ZERO ET ON SORT
        do 10 ino = 1, nbno
            do 20 j = 1, ndim
                zr(jgsv-1+3*ndim*(ino-1)+j)=0.d0
                zl(jgsl-1+3*ndim*(ino-1)+j)=.true.
                zr(jgsv-1+3*ndim*(ino-1)+j+ndim)=0.d0
                zl(jgsl-1+3*ndim*(ino-1)+j+ndim)=.true.
                zr(jgsv-1+3*ndim*(ino-1)+j+2*ndim)=0.d0
                zl(jgsl-1+3*ndim*(ino-1)+j+2*ndim)=.true.
 20         continue
 10     continue
        goto 999
    endif
!
    call jeveuo(xfonfi, 'L', ifon)
    call jelira(xfonfi, 'LONMAX', long)
    nfon = long/4
!
! --- RÉCUPÉRATION DES GRADIENTS DE LST ET LSN
!
    call jeveuo(grlt//'.CNSV', 'L', jgt)
    call jeveuo(grlt//'.CNSL', 'L', jgtl)
    call jeveuo(grln//'.CNSV', 'L', jgn)
!
!     CALCUL DES PROJETÉS DES NOEUDS SUR LE FOND DE FISSURE
    eps = 1.d-12
    do 100 ino = 1, nbno
        if (.not. zl(jgtl-1+ndim*(ino-1)+1)) goto 100
!       COORD DU NOEUD M DU MAILLAGE
        xm = zr(iadrco+(ino-1)*3+1-1)
        ym = zr(iadrco+(ino-1)*3+2-1)
        zm = zr(iadrco+(ino-1)*3+3-1)
!       INITIALISATION
        dmin = r8maem()
!       BOUCLE SUR PT DE FONFIS
        if (ndim .eq. 2) npoint = nfon
        if (ndim .eq. 3) npoint = nfon-1
        do 110 j = 1, npoint
            if (ndim .eq. 2) then
!           COORD PT N
                xn = zr(ifon-1+4*(j-1)+1)
                yn = zr(ifon-1+4*(j-1)+2)
                zn = 0.d0
!           DISTANCE MN
                d = sqrt((xn-xm)*(xn-xm)+(yn-ym)*(yn-ym))
            else if (ndim.eq.3) then
!           COORD PT I, ET J
                xi1 = zr(ifon-1+4*(j-1)+1)
                yi1 = zr(ifon-1+4*(j-1)+2)
                zi1 = zr(ifon-1+4*(j-1)+3)
                xj1 = zr(ifon-1+4*(j-1+1)+1)
                yj1 = zr(ifon-1+4*(j-1+1)+2)
                zj1 = zr(ifon-1+4*(j-1+1)+3)
!           VECTEUR IJ ET IM
                xij = xj1-xi1
                yij = yj1-yi1
                zij = zj1-zi1
                xim = xm-xi1
                yim = ym-yi1
                zim = zm-zi1
!           PARAM S (PRODUIT SCALAIRE...)
                s = xij*xim + yij*yim + zij*zim
                norm2 = xij*xij + yij *yij + zij*zij
                s = s/norm2
!           SI N=P(M) SORT DU SEGMENT
                if ((s-1) .ge. eps) s = 1.d0
                if (s .le. eps) s = 0.d0
!           COORD DE N
                xn = s*xij+xi1
                yn = s*yij+yi1
                zn = s*zij+zi1
!           DISTANCE MN
                d = sqrt((xn-xm)*(xn-xm)+(yn-ym)*(yn-ym)+ (zn-zm)*(zn- zm))
            endif
            if (d .lt. (dmin*(1-abs(r8prem())*100))) then
                dmin = d
                a(1)=xn
                a(2)=yn
                a(3)=zn
            endif
110     continue
!       STOCKAGE DU PROJETÉ ET DES GRADIENTS
        do 120 j = 1, ndim
            zr(jgsv-1+3*ndim*(ino-1)+j)=a(j)
            zl(jgsl-1+3*ndim*(ino-1)+j)=.true.
            zr(jgsv-1+3*ndim*(ino-1)+j+ndim)=zr(jgt-1+ndim*(ino-1)+j)
            zl(jgsl-1+3*ndim*(ino-1)+j+ndim)=.true.
            zr(jgsv-1+3*ndim*(ino-1)+j+2*ndim)=zr(jgn-1+ndim*(ino-1)+&
            j)
            zl(jgsl-1+3*ndim*(ino-1)+j+2*ndim)=.true.
120     continue
100 end do
!
999 continue
!
!     ENREGISTREMENT DU .BASLOC DANS LA SD FISS_XFEM
    basloc = fiss(1:8)//'.BASLOC'
    call cnscno(cnsbas, ' ', 'NON', 'G', basloc,&
                'F', ibid)
    call detrsd('CHAM_NO_S', cnsbas)
!
    if (niv .gt. 2) then
        call imprsd('CHAMP', basloc, ifm, 'FISSURE.BASLOC=')
    endif
!
    call jedema()
end subroutine
