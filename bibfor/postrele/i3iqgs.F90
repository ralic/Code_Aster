subroutine i3iqgs(epsi, k, f, desc, desctm,&
                  conexk, coordo, sgt, nbpt, lstpt,&
                  fink)
    implicit none
!
#include "jeveux.h"
#include "asterfort/i3afk2.h"
#include "asterfort/i3crqp.h"
#include "asterfort/i3efk2.h"
#include "asterfort/i3icfp.h"
#include "asterfort/i3icfs.h"
#include "asterfort/i3ptrv.h"
#include "asterfort/i3qpsp.h"
#include "asterfort/i3rpqp.h"
#include "asterfort/i3sl3r.h"
#include "asterfort/utmess.h"
    integer :: k, desc(*), desctm(*), conexk(*), nbpt, lstpt(*), f
    real(kind=8) :: epsi, sgt(*), coordo(*)
    logical :: fink
!
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
!     INTERSECTION FACE QUADANGLE GAUCHE F SGT (AB)
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! IN  K      : I : -
! IN  DESC   : I :  !--> OBJ MAILLE POINTEE (ET CE QU' ELLE POINTE)
! IN  DESCTM : I : -
! IN  F      : I : NUMERO LOCALE DE LA FACE TRAITEE
! IN  CONEXK : I : CONNECTIVITE DE LA MAILLE POINTEE
! IN  COORDO : R : TABLE GLOBALE DES COORDONEES
! IN  SGT    : R : COORDONNEES DES POINTS A ET B -
! VAR FINK   : L : INDICATEUR DE FIN DE REPERAGE NIVEAU MAILLE 3D
! OUT NBPT   : I : NOMBRE DE POINT TROUVE
!            :   : CONVENTION NBPT = -2 <=> CARD(INTER) = INFINI
!            :   : DANS CE CAS OUT = EXTREMITES
! OUT LSTPT  : I : OBJ LISTE_POINT
!     ------------------------------------------------------------------
!
    integer :: i, j, ds1, decf, adescm, iret, npt, tf, ipos1, ipos2
    integer :: vali(2)
    real(kind=8) :: zero, unsur2, un, seuil, lcara, tole
    real(kind=8) :: a(3, 3), fk(4, 3), c, r1, s1, t1, r2, s2, t2, normab, x1, x2
    real(kind=8) :: sgtf(6)
    real(kind=8) :: e1(3), e2(3), e3(3), cs(3, 4), sgtp(6), x(3), r(3), s(3)
    real(kind=8) :: t(3)
    real(kind=8) :: e1i(3), e2i(3)
    logical :: pb, djala1, djala2
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    pb = .false.
    tf = 0
    decf = 8 + f
    adescm = desctm(desc(k))
    zero = 0.0d0
    unsur2 = 0.5d0
    seuil = 0.2d0
    un = 1.0d0
    normab = zero
!
! --- RECUPERATION DES NOEUDS SOMMET DE LA FACE ET DE SES COORDONNEES
!
    do 10, i = 1, 4, 1
    ds1 = conexk(zi(adescm-1 + decf + (i-1)*6))
    do 11, j = 1, 3, 1
    cs(j,i) = coordo(3*(ds1-1) + j)
11  continue
    10 end do
!
! --- LCARA : LONGUEUR DE LA PLUS PETITE ARETE
!     AFIN DE DEFINIR UNE PRECISION RELATIVE
!
    lcara = 1.d+50
    do 20, j = 1, 3, 1
    c = zero
    do 22, i = 1, 3, 1
    s1 = cs(i,j+1) - cs(i,j)
    c = c + s1*s1
22  continue
    c = sqrt( c )
    lcara = min ( c, lcara )
    20 end do
    c = zero
    do 24, i = 1, 3, 1
    s1 = cs(i,4) - cs(i,1)
    c = c + s1*s1
    24 end do
    c = sqrt( c )
    lcara = min ( c, lcara )
    tole = lcara * epsi
!
! --- DEFINITION DU REPERE LOCAL DE LA FACE
!     E1 : DEFINIT PAR L'ARETE N1 N2
!     E2 : DEFINIT PAR L'ARETE N1 N4
!     E3 : PERPENDICULAIRE A E1 E2
!
    c = zero
    t1 = zero
    do 15, i = 1, 3, 1
    s1 = cs(i,2) - cs(i,1)
    r1 = cs(i,4) - cs(i,1)
    e1(i) = s1
    e2(i) = r1
    c = c + s1*s1
    t1 = t1 + r1*r1
    15 end do
    c = sqrt(c)
    t1 = sqrt(t1)
    if ((c.le.epsi) .or. (t1.le.epsi)) then
        pb = .true.
    else
        c = un/c
        t1 = un/t1
        do 16, i = 1, 3, 1
        e1(i) = e1(i)*c
        e2(i) = e2(i)*t1
16      continue
        e3(1) = e1(2)*e2(3) - e1(3)*e2(2)
        e3(2) = e1(3)*e2(1) - e1(1)*e2(3)
        e3(3) = e1(1)*e2(2) - e1(2)*e2(1)
        c = zero
        do 17, i = 1, 3, 1
        c = c + e3(i)*e3(i)
        x(i) = cs(i,1)
        a(i,1) = sgt(i)
        a(i,2) = sgt(i+3)
17      continue
        c = sqrt(c)
        if (c .le. epsi) then
            pb = .true.
        else
            c = un/c
            e3(1) = e3(1)*c
            e3(2) = e3(2)*c
            e3(3) = e3(3)*c
            e2(1) = -e1(2)*e3(3) + e1(3)*e3(2)
            e2(2) = -e1(3)*e3(1) + e1(1)*e3(3)
            e2(3) = -e1(1)*e3(2) + e1(2)*e3(1)
            c = un/sqrt(e2(1)*e2(1)+e2(2)*e2(2)+e2(3)*e2(3))
            e2(1) = e2(1)*c
            e2(2) = e2(2)*c
            e2(3) = e2(3)*c
!
! --- UN TOUR DE PASSE-PASSE POUR AVOIR DES VECTEURS AVEC DES 1 ET 0
!     AFIN DE SUPPRIMER LES ERREURS NUMERIQUES
!
            if (abs(abs(e3(1))-un) .le. tole) then
                e3(1) = sign(un,e3(1))
                e3(2) = zero
                e3(3) = zero
            else if (abs(abs(e3(2))-un) .le. tole) then
                e3(2) = sign(un,e3(2))
                e3(1) = zero
                e3(3) = zero
            else if (abs(abs(e3(3))-un) .le. tole) then
                e3(3) = sign(un,e3(3))
                e3(1) = zero
                e3(2) = zero
            endif
!
            if ((abs(abs(e1(1))-un) .le. tole) .and.&
                ((e1(2) .ne. zero) .or. (e1(3) .ne. zero))) then
                if (abs(e1(1)) .ne. un) then
                    e1i(1) = sign(un,e1(1))
                    e1i(2) = zero
                    e1i(3) = zero
                    call i3sl3r(e1, e1i, e3, cs)
                    e1(1) = sign(un,e1(1))
                    e1(3) = zero
                    e1(2) = zero
                endif
                else if ( (abs(abs(e1(2))-un) .le. tole) .and. ((e1(1)&
            .ne. zero) .or. (e1(3) .ne. zero)) ) then
                if (abs(e1(2)) .ne. un) then
                    e1i(2) = sign(un,e1(2))
                    e1i(1) = zero
                    e1i(3) = zero
                    call i3sl3r(e1, e1i, e3, cs)
                    e1(2) = sign(un,e1(2))
                    e1(1) = zero
                    e1(3) = zero
                endif
                else if ( (abs(abs(e1(3))-un) .le. tole) .and. ((e1(2)&
            .ne. zero) .or. (e1(1) .ne. zero)) ) then
                if (abs(e1(3)) .ne. un) then
                    e1i(3) = sign(un,e1(3))
                    e1i(1) = zero
                    e1i(2) = zero
                    call i3sl3r(e1, e1i, e3, cs)
                    e1(3) = sign(un,e1(3))
                    e1(1) = zero
                    e1(2) = zero
                endif
            endif
!
            if ((abs(abs(e2(1))-un) .le. tole) .and.&
                ((e2(2) .ne. zero) .or. (e2(3) .ne. zero))) then
                if (abs(e2(1)) .ne. un) then
                    e2i(1) = sign(un,e2(1))
                    e2i(3) = zero
                    e2i(2) = zero
                    call i3sl3r(e2, e2i, e3, cs)
                    e2(1) = sign(un,e2(1))
                    e2(3) = zero
                    e2(2) = zero
                endif
                else if ( (abs(abs(e2(2))-un) .le. tole) .and. ((e2(1)&
            .ne. zero) .or. (e2(3) .ne. zero)) ) then
                if (abs(e2(2)) .ne. un) then
                    e2i(2) = sign(un,e2(2))
                    e2i(1) = zero
                    e2i(3) = zero
                    call i3sl3r(e2, e2i, e3, cs)
                    e2(2) = sign(un,e2(2))
                    e2(1) = zero
                    e2(3) = zero
                endif
                else if ( (abs(abs(e2(3))-un) .le. tole) .and. ((e2(1)&
            .ne. zero) .or. (e2(2) .ne. zero)) ) then
                if (abs(e2(3)) .ne. un) then
                    e2i(3) = sign(un,e2(3))
                    e2i(1) = zero
                    e2i(2) = zero
                    call i3sl3r(e2, e2i, e3, cs)
                    e2(3) = sign(un,e2(3))
                    e2(1) = zero
                    e2(2) = zero
                endif
            endif
!
!
            call i3rpqp(x, e1, e2, e3, a,&
                        2)
            call i3rpqp(x, e1, e2, e3, cs(1, 2),&
                        3)
            do 18, i = 1, 2, 1
            c = a(i,2) - a(i,1)
            sgtp(i) = a(i,1)
            sgtp(i+3) = a(i,2)
            sgtf(i) = a(i,1)
            sgtf(i+3) = a(i,2)
            cs(i,1) = zero
            normab = normab + c*c
18          continue
            sgtp(3) = zero
            sgtf(3) = a(3,1)
            sgtp(6) = zero
            sgtf(6) = a(3,2)
            cs(3,1) = zero
            normab = sqrt(normab)
            call i3afk2(cs, fk, iret)
            pb = ( iret .eq. -1 )
            if (.not. pb) then
                if (normab .gt. epsi*sgt(6)) then
!              /* SGT PROJETE NON REDUIT A UN POINT */
                    call i3qpsp(tole, k, f, sgtp, cs,&
                                a, npt)
                    if (npt .eq. 2) then
                        r1 = a(1,1)
                        r2 = a(1,2)
                        s1 = a(2,1)
                        s2 = a(2,2)
                        t1 = a(3,1)
                        t2 = a(3,2)
                        do 42, i = 1,2, 1
                        r(i) = a(1,i)
                        s(i) = a(2,i)
42                      continue
                        call i3efk2(fk, 2, r, s, a)
                        do 40, i = 1, 3, 1
                        a(i,3) = (a(i,1)+a(i,2))*unsur2
40                      continue
                        r(1) = zero
                        r(2) = a(3,1)
                        s(1) = abs(t2-t1)*normab
                        s(2) = a(3,2)
                        if ((abs(r1-r2) .le. epsi) .or. (abs(s1-s2) .le. epsi)) then
!                    /* COUPE DE LA FACE = SGT */
                            call i3icfs(tole, a, sgtf, r, s,&
                                        npt, iret)
                        else
!                    /* COUPE DE LA FACE = ARC PARABOLE */
                            x1 = a(1,3)
                            x2 = a(2,3)
                            call i3crqp(epsi, seuil, cs, x1, x2,&
                                        x, iret)
                            pb = ( iret .eq. -1 )
                            if (.not. pb) then
                                t(1) = x(2)
                                call i3efk2(fk, 1, x, t, a(1, 3))
                                t(1) = s(1)*unsur2
                                t(2) = a(3,3)
                                call i3icfp(epsi, a, sgtf, r, s,&
                                            t, tf, npt, iret)
                            else
                                npt = 0
                            endif
                        endif
                        if (iret .eq. -1) then
                            pb = .true.
                            npt = 0
                        endif
                        if (npt .eq. -2) then
                            fink = .true.
                            npt = -npt
                        endif
                        if ((npt .ge. 1) .and. (.not. pb)) then
                            c = r(1)
                            t1 = r(2)
                            call i3ptrv(tole, lstpt, nbpt, t1, djala1,&
                                        ipos1)
                            x1 = c* (a(1,2)-a(1,1))*unsur2+a(1,3)
                            x1 = c*c*((a(1,2)+a(1,1))*unsur2-a(1,3))+ x1
                            x2 = c* (a(2,2)-a(2,1))*unsur2+a(2,3)
                            x2 = c*c*((a(2,2)+a(2,1))*unsur2-a(2,3))+ x2
                            call i3crqp(epsi, seuil, cs, x1, x2,&
                                        x, iret)
                            r1 = x(1)
                            s1 = x(2)
                            pb = ( iret .eq. -1 )
                        endif
                        if ((npt .ge. 2) .and. (.not. pb)) then
                            c = s(1)
                            t2 = s(2)
                            call i3ptrv(tole, lstpt, nbpt, t2, djala2,&
                                        ipos2)
                            x1 = c* (a(1,2)-a(1,1))*unsur2+a(1,3)
                            x1 = c*c*((a(1,2)+a(1,1))*unsur2-a(1,3))+ x1
                            x2 = c* (a(2,2)-a(2,1))*unsur2+a(2,3)
                            x2 = c*c*((a(2,2)+a(2,1))*unsur2-a(2,3))+ x2
                            call i3crqp(epsi, seuil, cs, x1, x2,&
                                        x, iret)
                            r2 = x(1)
                            s2 = x(2)
                            pb = ( iret .eq. -1 )
                        endif
                    else
                        npt = 0
                    endif
                else
!              /* SGT PROJETE REDUIT A UN POINT */
                    x1 = (sgtp(4)+sgtp(1))*unsur2
                    x2 = (sgtp(5)+sgtp(2))*unsur2
                    call i3crqp(epsi, seuil, cs, x1, x2,&
                                x, iret)
                    r1 = x(1)
                    s1 = x(2)
                    pb = ( iret .eq. -1 )
                    if (.not. pb) then
                        t(1) = x(2)
                        call i3efk2(fk, 1, x, t, a)
                        t1 = zero
                        t2 = zero
                        do 30, i = 1, 3, 1
                        c = sgtf(3+i)-sgtf(i)
                        t2 = t2 + c* c
                        t1 = t1 + c*(a(i,1)-sgtf(i))
30                      continue
                        t2 = un/sqrt(t2)
                        t1 = t1*t2
                        call i3ptrv(tole, lstpt, nbpt, t1, djala1,&
                                    ipos1)
                        npt = 1
                        tf = 0
                    endif
                endif
            endif
        endif
    endif
!
    if (pb) then
        vali (1) = k
        vali (2) = f
        call utmess('F', 'INTEMAIL_24', ni=2, vali=vali)
    endif
!
    if (fink) nbpt = 0
    if (npt .ge. 1) then
        if (.not. djala1) then
            zr(lstpt(1) + nbpt) = t1
            zi(lstpt(2) + nbpt) = f
            zi(lstpt(3) + nbpt) = 0
            zi(lstpt(4) + nbpt) = tf
            zr(lstpt(5) + 2*nbpt+1-1) = r1
            zr(lstpt(5) + 2*nbpt+2-1) = s1
            zi(lstpt(6) + nbpt) = nbpt + 1
            nbpt = nbpt + 1
        else
            zr(lstpt(1) + ipos1-1 ) = t1
            zi(lstpt(2) + ipos1-1 ) = f
            zi(lstpt(3) + ipos1-1 ) = 0
            zi(lstpt(4) + ipos1-1 ) = tf
            zr(lstpt(5) + 2*(ipos1-1)+1-1) = r1
            zr(lstpt(5) + 2*(ipos1-1)+2-1) = s1
        endif
    endif
    if (npt .ge. 2) then
        if (.not. djala2) then
            zr(lstpt(1) + nbpt) = t2
            zi(lstpt(2) + nbpt) = f
            zi(lstpt(3) + nbpt) = 0
            zi(lstpt(4) + nbpt) = tf
            zr(lstpt(5) + 2*nbpt+1-1) = r2
            zr(lstpt(5) + 2*nbpt+2-1) = s2
            zi(lstpt(6) + nbpt) = nbpt + 1
            nbpt = nbpt + 1
        else
            zr(lstpt(1) + ipos2-1 ) = t2
            zi(lstpt(2) + ipos2-1 ) = f
            zi(lstpt(3) + ipos2-1 ) = 0
            zi(lstpt(4) + ipos2-1 ) = tf
            zr(lstpt(5) + 2*(ipos2-1)+1-1) = r2
            zr(lstpt(5) + 2*(ipos2-1)+2-1) = s2
        endif
    endif
    if (fink) nbpt = -2
!
end subroutine
