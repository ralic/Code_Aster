subroutine i3pdm3(epsi, k, desc, desctm, conexk,&
                  coordo, pt, dedans)
    implicit none
    include 'jeveux.h'
    include 'asterfort/i3afk2.h'
    include 'asterfort/i3tstf.h'
    include 'asterfort/mgauss.h'
    include 'asterfort/u2mesi.h'
    integer :: k, desc(*), desctm(*), conexk(*)
    real(kind=8) :: epsi, pt(*), coordo(*)
    logical :: dedans
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
!     ------------------------------------------------------------------
!     APPARTENANCE DU POINT PT A LA MAILLE 3D K
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! IN  K      : I : -
! IN  DESC   : I :  !--> OBJ MAILLE POINTEE (ET CE QU' ELLE POINTE)
! IN  DESCTM : I : -
! IN  CONEXK : I : CONNECTIVITE DE LA MAILLE POINTEE
! IN  COORDO : R : TABLE GLOBALE DES COORDONEES
! IN  PT     : R : COORDONNEES DU POINTS
! OUT DEDANS : I : REPONSE
!     ------------------------------------------------------------------
!
!
    integer :: f, i, j, nbf, nbs, adestm, decf, col, iret, ising
    integer :: vali(2)
    real(kind=8) :: zero, un, deux, cs(3, 4), a(10, 10), fk(4, 4), b(10), val1
    real(kind=8) :: val2, det
    logical :: fini, gauche
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    adestm = desctm(desc(k))
    dedans = .true.
    fini = .false.
    f = 0
    iret = 0
    zero = 0.0d0
    un = 1.0d0
    deux = 2.0d0
    nbf = zi(adestm)
100  continue
    if (.not. fini) then
        f = f + 1
        decf = 8 + f
        nbs = zi(adestm-1 + 2+f)
        do 110, i = 1, nbs, 1
        col = conexk(zi(adestm-1 + decf + 6*(i-1)))
        do 111, j = 1, 3, 1
        cs(j,i) = coordo(3*(col-1) + j)
111      continue
110      continue
        if (nbs .eq. 3) then
            gauche = .false.
        else
            call i3tstf(k, f, desc, desctm, conexk,&
                        coordo, gauche, epsi)
        endif
        if (gauche) then
            do 10, j = 1, 10, 1
            do 11, i = 1, 9, 1
            a(i,j) = zero
11          continue
            a(10,j) = un
            b(j) = zero
10          continue
            a(1,1) = un
            b(10) = un
            call i3afk2(cs, fk, iret)
            if (iret .ne. -1) then
                do 120, i = 1, 4, 1
                do 121, j = 1, 3, 1
                a(i,j+1) = deux*fk(i,j)
121              continue
120              continue
                col = 5
                do 130, i = 1, 3, 1
                a(1,col) = fk(1,i)*fk(1,i)
                a(2,col) = fk(1,i)*fk(2,i)+fk(2,i)*fk(1,i)
                a(3,col) = fk(1,i)*fk(3,i)+fk(3,i)*fk(1,i)
                a(4,col) = fk(1,i)*fk(4,i)+fk(2,i)*fk(3,i)
                a(4,col) = fk(3,i)*fk(2,i)+fk(4,i)*fk(1,i)+a(4, col)
                a(5,col) = fk(2,i)*fk(2,i)
                a(6,col) = fk(3,i)*fk(3,i)
                a(7,col) = fk(4,i)*fk(2,i)+fk(2,i)*fk(4,i)
                a(8,col) = fk(4,i)*fk(3,i)+fk(3,i)*fk(4,i)
                a(9,col) = fk(4,i)*fk(4,i)
                col = col + 1
                do 131, j = i+1, 3, 1
                a(1,col) = deux*(fk(1,i)*fk(1,j))
                a(2,col) = deux*(fk(1,i)*fk(2,j)+fk(2,i)*fk(1, j))
                a(3,col) = deux*(fk(1,i)*fk(3,j)+fk(3,i)*fk(1, j))
                a(4,col) = deux*(fk(1,i)*fk(4,j)+fk(2,i)*fk(3, j))
                a(4,col) = fk(3,i)*fk(2,j)+fk(4,i)*fk(1,j)+a( 4,col)
                a(4,col) = a(4,col)*deux
                a(5,col) = deux*(fk(2,i)*fk(2,j))
                a(6,col) = deux*(fk(3,i)*fk(3,j))
                a(7,col) = deux*(fk(4,i)*fk(2,j)+fk(2,i)*fk(4, j))
                a(8,col) = deux*(fk(4,i)*fk(3,j)+fk(3,i)*fk(4, j))
                a(9,col) = deux*(fk(4,i)*fk(4,j))
                col = col + 1
131              continue
130              continue
                call mgauss('NFVP', a, b, 10, 10,&
                            1, det, ising)
            endif
            col = 1
            val1 = zero
            if (iret .ne. -1) then
                do 140, i = 1, 4, 1
                a(i,i) = b(col)
                col = col + 1
                do 141, j = i+1, 4, 1
                a(i,j) = b(col)
                a(j,i) = b(col)
                col = col + 1
141              continue
140              continue
            endif
            b(1) = un
            do 150, i = 1, 3, 1
            b(i+1) = pt(i)
150          continue
            do 160 , j = 1, 4, 1
            do 161, i = 1, 4, 1
            val1 = val1 + b(i)*a(i,j)*b(j)
161          continue
160          continue
            col = conexk(zi(adestm-1 + 32+f))
            do 170, i = 1, 3, 1
            b(i+1) = coordo(3*(col-1)+i)
170          continue
            val2 = zero
            do 180 , j = 1, 4, 1
            do 181, i = 1, 4, 1
            val2 = val2 + b(i)*a(i,j)*b(j)
181          continue
180          continue
            val1 = -val1*val2
        else
            b(1) = (cs(2,2)-cs(2,1))*(cs(3,3)-cs(3,1))
            b(1) = -(cs(2,3)-cs(2,1))*(cs(3,2)-cs(3,1)) + b(1)
            b(2) = -(cs(1,2)-cs(1,1))*(cs(3,3)-cs(3,1))
            b(2) = (cs(1,3)-cs(1,1))*(cs(3,2)-cs(3,1)) + b(2)
            b(3) = (cs(1,2)-cs(1,1))*(cs(2,3)-cs(2,1))
            b(3) = -(cs(1,3)-cs(1,1))*(cs(2,2)-cs(2,1)) + b(3)
            val1 = sqrt(b(1)*b(1)+b(2)*b(2)+b(3)*b(3))
            if (abs(val1) .le. epsi) then
                iret = -1
            else
                val1 = un/val1
                do 132, i = 1, 3, 1
                b(i) = b(i)*val1
132              continue
            endif
            val1 = zero
            do 133,i = 1, 3, 1
            val1 = val1 + b(i)*(pt(i)-cs(i,1))
133          continue
        endif
        dedans = ( val1 .le. zero ) .or. ( abs(val1) .le. epsi*epsi)
        fini = ((f .eq. nbf) .or. (.not. dedans) .or. (iret .eq. -1))
        goto 100
    endif
    if (iret .eq. -1) then
        vali (1) = k
        vali (2) = f
        call u2mesi('F', 'INTEMAIL_24', 2, vali)
    endif
end subroutine
