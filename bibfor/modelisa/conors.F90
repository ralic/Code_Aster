subroutine conors(i1, i2, i3, macoc, nbcoc,&
                  macor, nbcor, loreor, mailla)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!
! DESCRIPTION : REORIENTATION D'UNE MAILLE DE FISSURE
! -----------   LA NORMALE A CHAQUE BORD EST SORTANTE
!
!               HYPOTHESE (CF MODI_MAILLAGE) :
!               LA MAILLE DE FISSURE PEUT ETRE DU TYPE
!               - QUAD4 OU QUAD8 EN 2D
!               - PENTA6 OU PENTA15 OU HEXA8 OU HEXA20 EN 3D
!
! IN     : MAILLA : CHARACTER*8 , SCALAIRE
!                   NOM DU CONCEPT MAILLAGE
! IN     : NBCOC  : INTEGER , SCALAIRE
!                   NOMBRE DE NOEUDS DE LA MAILLE DE FISSURE
! IN/OUT : MACOC  : CHARACTER*8 , VECTEUR DE DIMENSION NBCOC+2
!                   MACOC(1) : NOM DE LA MAILLE DE FISSURE
!                   MACOC(2) : NOM DU TYPE DE LA MAILLE DE FISSURE
!                   MACOC(2+1) A MACOC(2+NBCOC) : NOMS DES NOEUDS DE
!                   LA MAILLE DE FISSURE, DANS L'ORDRE DEFINI PAR LA
!                   CONNECTIVITE. EN SORTIE L'ORDRE PEUT ETRE CHANGE
!                   SI LA MAILLE A ETE REORIENTEE.
! OUT    : LOREOR : LOGICAL , SCALAIRE
!                   INDICATEUR DE REORIENTATION DE LA MAILLE DE FISSURE
!                   SI LOREOR =.TRUE. LA CONNECTIVITE DOIT ETRE MODIFIEE
!                   AFIN D'OBTENIR UNE NORMALE SORTANTE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/u2mesk.h"
    integer :: i1, i2, i3
    character(len=8) :: mailla
    integer :: nbcoc
    character(len=8) :: macoc(2+nbcoc)
    logical :: loreor
    integer :: nbcor
    character(len=8) :: macor(2+nbcor)
!
! VARIABLES LOCALES
! -----------------
    integer :: jcoor, no, no1, no2, no3, inor
    character(len=24) :: coorno, nonoma
    real(kind=8) :: x1, y1, z1, x2, y2, z2, x3, y3, z3, scal
    real(kind=8) :: xg, yg, zg, vnx, vny, vnz, vrx, vry, vrz
    real(kind=8) :: vx1, vy1, vz1, vx2, vy2, vz2
!
! FONCTIONS EXTERNES
! ------------------
!     EXTERNAL      JEXNOM, JEXNUM, R8DOT, R8NRM2
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL      JEDEMA, JEMARQ, JENONU, JEVEUO,
!                   DISMOI
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
!
!-----------------------------------------------------------------------
!     INITIALISATIONS - ACCES AUX OBJETS DU CONCEPT MAILLAGE
!-----------------------------------------------------------------------
!
    coorno = mailla//'.COORDO    .VALE'
    call jeveuo(coorno, 'L', jcoor)
    nonoma = mailla//'.NOMNOE         '
!
!     DETERMINATION D'UN VECTEUR NORMAL A LA MAILLE DE FISSURE
!
    call jenonu(jexnom(nonoma, macoc(2+i1)), no1)
    x1=zr(jcoor+3*(no1-1)+0)
    y1=zr(jcoor+3*(no1-1)+1)
    z1=zr(jcoor+3*(no1-1)+2)
!
    call jenonu(jexnom(nonoma, macoc(2+i2)), no2)
    x2=zr(jcoor+3*(no2-1)+0)
    y2=zr(jcoor+3*(no2-1)+1)
    z2=zr(jcoor+3*(no2-1)+2)
    vx1=x2-x1
    vy1=y2-y1
    vz1=z2-z1
!     SI I3 EST NUL NOUS SOMMES EN PRESENCE D'UN QUADRILATERE
    if (i3 .eq. 0) then
        vnx=-vy1
        vny= vx1
        vnz= 0
    else
        call jenonu(jexnom(nonoma, macoc(2+i3)), no3)
        x3=zr(jcoor+3*(no3-1)+0)
        y3=zr(jcoor+3*(no3-1)+1)
        z3=zr(jcoor+3*(no3-1)+2)
        vx2=x3-x1
        vy2=y3-y1
        vz2=z3-z1
        vnx=vy1*vz2-vy2*vz1
        vny=vz1*vx2-vz2*vx1
        vnz=vx1*vy2-vx2*vy1
    endif
!
!     CENTRE DE GRAVITE DE LA MAILLE DE REFERENCE
!
    xg=0.d0
    yg=0.d0
    zg=0.d0
    do 10 inor = 1, nbcor
        call jenonu(jexnom(nonoma, macor(2+inor)), no)
        xg=xg+zr(jcoor+3*(no-1)+0)
        yg=yg+zr(jcoor+3*(no-1)+1)
        zg=zg+zr(jcoor+3*(no-1)+2)
10  end do
    xg=xg/nbcor
    yg=yg/nbcor
    zg=zg/nbcor
!
!     VECTEUR NOEUD 1 - CENTRE DE GRAVITE
!
    vrx=xg-x1
    vry=yg-y1
    vrz=zg-z1
!
!     VERIFICATION QUE LA NORMALE EST SORTANTE
!
    scal=vnx*vrx+vny*vry+vnz*vrz
!
    if (scal .eq. 0) call u2mesk('E', 'MODELISA4_36', 1, macoc(1))
!
    loreor=scal.gt.0
!
    call jedema()
!
end subroutine
