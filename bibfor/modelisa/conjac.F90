subroutine conjac(i0, i1, i2, i3, macoc,&
                  nbcoc, mailla)
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
! DESCRIPTION : VERIFICATION D'UNE MAILLE FISSURE
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
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
    include 'jeveux.h'
!
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/u2mess.h'
    integer :: i0, i1, i2, i3
    character(len=8) :: mailla
    integer :: nbcoc
    character(len=8) :: macoc(2+nbcoc)
!
! VARIABLES LOCALES
! -----------------
    integer :: jcoor, no0, no1, no2, no3, niv, ifm
    character(len=24) :: coorno, nonoma
    real(kind=8) :: x0, y0, z0, x1, y1, z1, x2, y2, z2, x3, y3, z3, scal
    real(kind=8) :: vnx, vny, vnz
    real(kind=8) :: vx1, vy1, vz1, vx2, vy2, vz2, vx3, vy3, vz3
!
! FONCTIONS EXTERNES
! ------------------
!     EXTERNAL      JEXNOM, JEXNUM, R8DOT, R8NRM2
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL      JEDEMA, JEMARQ, JENONU, JEVEUO,
!                   DISMOI.
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
    call infniv(ifm, niv)
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
    call jenonu(jexnom(nonoma, macoc(2+i0)), no0)
    x0=zr(jcoor+3*(no0-1)+0)
    y0=zr(jcoor+3*(no0-1)+1)
    z0=zr(jcoor+3*(no0-1)+2)
!
    call jenonu(jexnom(nonoma, macoc(2+i1)), no1)
    x1=zr(jcoor+3*(no1-1)+0)
    y1=zr(jcoor+3*(no1-1)+1)
    z1=zr(jcoor+3*(no1-1)+2)
    vx1=x1-x0
    vy1=y1-y0
    vz1=z1-z0
!
    call jenonu(jexnom(nonoma, macoc(2+i2)), no2)
    x2=zr(jcoor+3*(no2-1)+0)
    y2=zr(jcoor+3*(no2-1)+1)
    z2=zr(jcoor+3*(no2-1)+2)
    vx2=x2-x0
    vy2=y2-y0
    vz2=z2-z0
!
    vnx=vy1*vz2-vy2*vz1
    vny=vz1*vx2-vz2*vx1
    vnz=vx1*vy2-vx2*vy1
!     SI I3 EST NUL NOUS SOMMES EN PRESENCE D'UN QUADRILATERE
    if (i3 .eq. 0) then
        vx3= 0
        vy3= 0
        vz3=-1
    else
        call jenonu(jexnom(nonoma, macoc(2+i3)), no3)
        x3=zr(jcoor+3*(no3-1)+0)
        y3=zr(jcoor+3*(no3-1)+1)
        z3=zr(jcoor+3*(no3-1)+2)
        vx3=x3-x0
        vy3=y3-y0
        vz3=z3-z0
    endif
!
!     VERIFICATION DU SIGNE DU JACOBIEN
!
    scal=vnx*vx3+vny*vy3+vnz*vz3
    if (niv .eq. 2) then
        write(ifm,*) 'SOMMET ',macoc(2+i0),' DE LA MAILLE ',macoc(1),&
        ' VERS LES SOMMETS :'
        if (i3 .eq. 0) then
            write(ifm,*) macoc(2+i1),macoc(2+i2)
        else
            write(ifm,*) macoc(2+i1),macoc(2+i2),macoc(2+i3)
        endif
        write(ifm,*) 'JACOBIEN ',scal
    endif
    if (scal .lt. 0) call u2mess('E', 'MODELISA4_35')
!
    call jedema()
!
end subroutine
