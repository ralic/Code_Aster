subroutine cargeo(mailla)
    implicit   none
    include 'jeveux.h'
!
    include 'asterc/r8gaem.h'
    include 'asterfort/assert.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/ltcrsd.h'
    include 'asterfort/ltnotb.h'
    include 'asterfort/rminsp.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbcrsd.h'
    character(len=*) :: mailla
!     ------------------------------------------------------------------
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
!  CALCULER DES CARACTERISTIQUES DU MAILLAGES:
!  X_MIN    : ABSCISSE MINIMALE DES NOEUDS DU MAILLAGE
!  X_MAX    : ABSCISSE MAXIMALE DES NOEUDS DU MAILLAGE
!  Y_MIN    : ORDONNEE MINIMALE DES NOEUDS DU MAILLAGE
!  Y_MAX    : ORDONNEE MAXIMALE DES NOEUDS DU MAILLAGE
!  Z_MIN    : COTE MINIMALE DES NOEUDS DU MAILLAGE
!  Z_MAX    : COTE MAXIMALE DES NOEUDS DU MAILLAGE
!  APPLAT_Z : = ( Z_MAX - Z_MIN ) / D
!             AVEC D = MAX((X_MAX-X_MIN),(Y_MAX-Y_MIN),1.D-100)
!  AR_MIN   : LONGUEUR DE LA PLUS PETITE ARRETE DU MAILLAGE (NON NULLE)
!  AR_MAX   : LONGUEUR DE LA PLUS GRANDE ARRETE DU MAILLAGE
!
! IN  : MAILLA  : NOM DE LA SD MAILLAGE
!     ------------------------------------------------------------------
!
    integer :: nbnoeu, jvale, jdime, nbmail, i, nbpara, nbpart, ibid, im, jtypm
    integer :: jcone, n, iret
    parameter    ( nbpara = 9 )
    real(kind=8) :: applat, xmax, ymax, zmax, xmin, ymin, zmin, vale(nbpara)
    real(kind=8) :: armin, armax, x(8), y(8), z(8), d1, d2, d3, d4
    complex(kind=8) :: c16b
    character(len=8) :: k8b, ma, nopara(nbpara), typara(nbpara), typm
    character(len=1) :: bas1
    character(len=19) :: nomt19
    character(len=24) :: nodime, connex, coordo, typmai
!
    data nopara / 'X_MIN' , 'X_MAX' , 'Y_MIN' , 'Y_MAX' , 'Z_MIN' ,&
     &              'Z_MAX' , 'APPLAT_Z' , 'AR_MIN' , 'AR_MAX' /
    data typara / 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R' /
! DEB------------------------------------------------------------------
!
    call jemarq()
!
    ma = mailla
    nodime = ma//'.DIME           '
    connex = ma//'.CONNEX         '
    coordo = ma//'.COORDO    .VALE'
    typmai = ma//'.TYPMAIL        '
!
    call jelira(nodime, 'CLAS', ibid, bas1)
    call assert(bas1.eq.'G' .or. bas1.eq.'V')
!
    call jeveuo(nodime, 'L', jdime)
    nbnoeu = zi(jdime)
    nbmail = zi(jdime+3-1)
!
    call jeveuo(coordo, 'L', jvale)
    xmax = zr(jvale )
    ymax = zr(jvale+1)
    zmax = zr(jvale+2)
    xmin = xmax
    ymin = ymax
    zmin = zmax
    do 10 i = 2, nbnoeu
        xmax = max ( xmax , zr(jvale+3*(i-1) ) )
        xmin = min ( xmin , zr(jvale+3*(i-1) ) )
        ymax = max ( ymax , zr(jvale+3*(i-1)+1) )
        ymin = min ( ymin , zr(jvale+3*(i-1)+1) )
        zmax = max ( zmax , zr(jvale+3*(i-1)+2) )
        zmin = min ( zmin , zr(jvale+3*(i-1)+2) )
10  end do
    d1 = max( (xmax-xmin) , (ymax-ymin) , 1.d-100 )
    applat = ( zmax - zmin ) / d1
    vale(1) = xmin
    vale(2) = xmax
    vale(3) = ymin
    vale(4) = ymax
    vale(5) = zmin
    vale(6) = zmax
    vale(7) = applat
!
    call jeexin(connex, iret)
    if (iret .eq. 0) then
        nbpart = 7
        goto 100
    else
        nbpart = nbpara
    endif
!
    armin = r8gaem()
    armax = 0.d0
    call jeveuo(typmai, 'L', jtypm)
    do 20 im = 1, nbmail
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtypm+im-1)), typm)
        call jeveuo(jexnum(connex, im), 'L', jcone)
        if (typm(1:3) .eq. 'POI') then
        else if (typm(1:3) .eq. 'SEG') then
            do 27 i = 1, 2
                n = zi(jcone+i-1)
                x(i) = zr(jvale+3*(n-1) )
                y(i) = zr(jvale+3*(n-1)+1)
                z(i) = zr(jvale+3*(n-1)+2)
27          continue
            d1 = (x(2)-x(1))**2 + (y(2)-y(1))**2 + (z(2)-z(1))**2
            armin = rminsp( armin , d1 , 0.d0, 0.d0, 0.d0)
            armax = max ( armax , d1 )
        else if (typm(1:4) .eq. 'TRIA') then
            do 21 i = 1, 3
                n = zi(jcone+i-1)
                x(i) = zr(jvale+3*(n-1) )
                y(i) = zr(jvale+3*(n-1)+1)
                z(i) = zr(jvale+3*(n-1)+2)
21          continue
            d1 = (x(2)-x(1))**2 + (y(2)-y(1))**2 + (z(2)-z(1))**2
            d2 = (x(3)-x(2))**2 + (y(3)-y(2))**2 + (z(3)-z(2))**2
            d3 = (x(1)-x(3))**2 + (y(1)-y(3))**2 + (z(1)-z(3))**2
            armin = rminsp( armin , d1 , d2 , d3 ,0.d0)
            armax = max ( armax , d1 , d2 , d3 )
        else if (typm(1:4) .eq. 'QUAD') then
            do 22 i = 1, 4
                n = zi(jcone+i-1)
                x(i) = zr(jvale+3*(n-1) )
                y(i) = zr(jvale+3*(n-1)+1)
                z(i) = zr(jvale+3*(n-1)+2)
22          continue
            d1 = (x(2)-x(1))**2 + (y(2)-y(1))**2 + (z(2)-z(1))**2
            d2 = (x(3)-x(2))**2 + (y(3)-y(2))**2 + (z(3)-z(2))**2
            d3 = (x(4)-x(3))**2 + (y(4)-y(3))**2 + (z(4)-z(3))**2
            d4 = (x(1)-x(4))**2 + (y(1)-y(4))**2 + (z(1)-z(4))**2
            armin = rminsp( armin , d1 , d2 , d3 , d4 )
            armax = max ( armax , d1 , d2 , d3 , d4 )
        else if (typm(1:4) .eq. 'HEXA') then
            do 23 i = 1, 8
                n = zi(jcone+i-1)
                x(i) = zr(jvale+3*(n-1) )
                y(i) = zr(jvale+3*(n-1)+1)
                z(i) = zr(jvale+3*(n-1)+2)
23          continue
            d1 = (x(2)-x(1))**2 + (y(2)-y(1))**2 + (z(2)-z(1))**2
            d2 = (x(3)-x(2))**2 + (y(3)-y(2))**2 + (z(3)-z(2))**2
            d3 = (x(4)-x(3))**2 + (y(4)-y(3))**2 + (z(4)-z(3))**2
            d4 = (x(1)-x(4))**2 + (y(1)-y(4))**2 + (z(1)-z(4))**2
            armin = rminsp( armin , d1 , d2 , d3 , d4 )
            armax = max ( armax , d1 , d2 , d3 , d4 )
            d1 = (x(6)-x(5))**2 + (y(6)-y(5))**2 + (z(6)-z(5))**2
            d2 = (x(7)-x(6))**2 + (y(7)-y(6))**2 + (z(7)-z(6))**2
            d3 = (x(8)-x(7))**2 + (y(8)-y(7))**2 + (z(8)-z(7))**2
            d4 = (x(5)-x(8))**2 + (y(5)-y(8))**2 + (z(5)-z(8))**2
            armin = rminsp( armin , d1 , d2 , d3 , d4 )
            armax = max ( armax , d1 , d2 , d3 , d4 )
            d1 = (x(1)-x(5))**2 + (y(1)-y(5))**2 + (z(1)-z(5))**2
            d2 = (x(2)-x(6))**2 + (y(2)-y(6))**2 + (z(2)-z(6))**2
            d3 = (x(3)-x(7))**2 + (y(3)-y(7))**2 + (z(3)-z(7))**2
            d4 = (x(4)-x(8))**2 + (y(4)-y(8))**2 + (z(4)-z(8))**2
            armin = rminsp( armin , d1 , d2 , d3 , d4 )
            armax = max ( armax , d1 , d2 , d3 , d4 )
        else if (typm(1:5) .eq. 'PENTA') then
            do 24 i = 1, 6
                n = zi(jcone+i-1)
                x(i) = zr(jvale+3*(n-1) )
                y(i) = zr(jvale+3*(n-1)+1)
                z(i) = zr(jvale+3*(n-1)+2)
24          continue
            d1 = (x(2)-x(1))**2 + (y(2)-y(1))**2 + (z(2)-z(1))**2
            d2 = (x(3)-x(2))**2 + (y(3)-y(2))**2 + (z(3)-z(2))**2
            d3 = (x(1)-x(3))**2 + (y(1)-y(3))**2 + (z(1)-z(3))**2
            armin = rminsp( armin , d1 , d2 , d3 ,0.d0)
            armax = max ( armax , d1 , d2 , d3 )
            d1 = (x(5)-x(4))**2 + (y(5)-y(4))**2 + (z(5)-z(4))**2
            d2 = (x(6)-x(5))**2 + (y(6)-y(5))**2 + (z(6)-z(5))**2
            d3 = (x(4)-x(6))**2 + (y(4)-y(6))**2 + (z(4)-z(6))**2
            armin = rminsp( armin , d1 , d2 , d3 ,0.d0)
            armax = max ( armax , d1 , d2 , d3 )
            d1 = (x(1)-x(4))**2 + (y(1)-y(4))**2 + (z(1)-z(4))**2
            d2 = (x(2)-x(5))**2 + (y(2)-y(5))**2 + (z(2)-z(5))**2
            d3 = (x(3)-x(6))**2 + (y(3)-y(6))**2 + (z(3)-z(6))**2
            armin = rminsp( armin , d1 , d2 , d3 ,0.d0)
            armax = max ( armax , d1 , d2 , d3 )
        else if (typm(1:5) .eq. 'TETRA') then
            do 25 i = 1, 4
                n = zi(jcone+i-1)
                x(i) = zr(jvale+3*(n-1) )
                y(i) = zr(jvale+3*(n-1)+1)
                z(i) = zr(jvale+3*(n-1)+2)
25          continue
            d1 = (x(2)-x(1))**2 + (y(2)-y(1))**2 + (z(2)-z(1))**2
            d2 = (x(3)-x(2))**2 + (y(3)-y(2))**2 + (z(3)-z(2))**2
            d3 = (x(1)-x(3))**2 + (y(1)-y(3))**2 + (z(1)-z(3))**2
            armin = rminsp( armin , d1 , d2 , d3 ,0.d0)
            armax = max ( armax , d1 , d2 , d3 )
            d1 = (x(4)-x(1))**2 + (y(4)-y(1))**2 + (z(4)-z(1))**2
            d2 = (x(4)-x(2))**2 + (y(4)-y(2))**2 + (z(4)-z(2))**2
            d3 = (x(4)-x(3))**2 + (y(4)-y(3))**2 + (z(4)-z(3))**2
            armin = rminsp( armin , d1 , d2 , d3 ,0.d0)
            armax = max ( armax , d1 , d2 , d3 )
        else if (typm(1:4) .eq. 'PYRA') then
            do 26 i = 1, 5
                n = zi(jcone+i-1)
                x(i) = zr(jvale+3*(n-1) )
                y(i) = zr(jvale+3*(n-1)+1)
                z(i) = zr(jvale+3*(n-1)+2)
26          continue
            d1 = (x(2)-x(1))**2 + (y(2)-y(1))**2 + (z(2)-z(1))**2
            d2 = (x(3)-x(2))**2 + (y(3)-y(2))**2 + (z(3)-z(2))**2
            d3 = (x(4)-x(3))**2 + (y(4)-y(3))**2 + (z(4)-z(3))**2
            d4 = (x(1)-x(4))**2 + (y(1)-y(4))**2 + (z(1)-z(4))**2
            armin = rminsp( armin , d1 , d2 , d3 , d4 )
            armax = max ( armax , d1 , d2 , d3 , d4 )
            d1 = (x(5)-x(1))**2 + (y(5)-y(1))**2 + (z(5)-z(1))**2
            d2 = (x(5)-x(2))**2 + (y(5)-y(2))**2 + (z(5)-z(2))**2
            d3 = (x(5)-x(3))**2 + (y(5)-y(3))**2 + (z(5)-z(3))**2
            d4 = (x(5)-x(4))**2 + (y(5)-y(4))**2 + (z(5)-z(4))**2
            armin = rminsp( armin , d1 , d2 , d3 , d4 )
            armax = max ( armax , d1 , d2 , d3 , d4 )
        endif
20  end do
    if (armin .eq. r8gaem()) armin = 0.d0
    vale(8) = sqrt( armin )
    vale(9) = sqrt( armax )
!
100  continue
!
    nomt19 = ' '
    call jeexin(ma//'           .LTNT', iret)
    if (iret .ne. 0) then
        call ltnotb(mailla, 'CARA_GEOM', nomt19)
        call detrsd('TABLE', nomt19)
    else
        call ltcrsd(mailla, bas1)
    endif
    call ltnotb(mailla, 'CARA_GEOM', nomt19)
!
    call jeexin(nomt19//'.TBBA', iret)
    if (iret .ne. 0) call detrsd('TABLE', nomt19)
!
    call tbcrsd(nomt19, bas1)
    call tbajpa(nomt19, nbpart, nopara, typara)
    call tbajli(nomt19, nbpart, nopara, ibid, vale,&
                c16b, k8b, 0)
!
    call jedema()
end subroutine
