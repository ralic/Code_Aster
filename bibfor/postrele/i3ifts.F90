subroutine i3ifts(epsi, k, f, desc, desctm,&
                  conexk, coordo, sgt, nbpt, lstpt,&
                  fink)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/i3crtp.h"
#include "asterfort/i3idfs.h"
#include "asterfort/i3pdm2.h"
#include "asterfort/i3ptrv.h"
#include "asterfort/i3sl33.h"
#include "asterfort/utmess.h"
    integer :: k, desc(*), desctm(*), conexk(*), nbpt, lstpt(*), f
    real(kind=8) :: epsi, sgt(*), coordo(*)
    aster_logical :: fink
!
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     INTERSECTION FACE TRIANGLE F SGT (AB)
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
!
    character(len=4) :: typsl
    integer :: sm(3), i, j, ds1, decf, adescm, arete, nba, iret, ipos, vali(3)
    real(kind=8) :: c, zero, un, r, s, t, lcara, unsur2, eps
    real(kind=8) :: a(3, 3), b(3), x(3), cs(3, 3), e1(3), e2(3), e3(3)
    aster_logical :: djala1, djala2
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    nba = 3
    decf = 8 + f
    adescm = desctm(desc(k))
    zero = 0.0d0
    un = 1.0d0
    unsur2 = 0.5d0
    c = zero
    do 10 i = 1, 3, 1
        ds1 = conexk(zi(adescm-1 + decf + (i-1)*6))
        sm(i) = ds1
        do 11 j = 1, 3, 1
            cs(j,i) = coordo(3*(ds1-1) + j)
 11     continue
        c = c + (sgt(i+3)-sgt(i))*(sgt(i+3)-sgt(i))
 10 end do
    a(1,3) = zero
    a(2,3) = zero
    a(3,3) = -sqrt(c)
    c = zero
    t = zero
    do 15 i = 1, 3, 1
        s = cs(i,2) - cs(i,1)
        r = cs(i,3) - cs(i,1)
        e1(i) = s
        e2(i) = r
        c = c + s*s
        t = t + r*r
 15 end do
    c = sqrt(c)
    t = sqrt(t)
    lcara = unsur2*(c + t)
    eps = max(epsi/lcara,epsi)
    if ((c.le.abs(cs(1,1))*epsi) .or. (t.le.abs(cs(1,1))*epsi)) then
        vali(1) = k
        vali(2) = f
        call utmess('F', 'INTEMAIL_24', ni=2, vali=vali)
    else
        c = un/c
        t = un/t
        do 16 i = 1, 3, 1
            e1(i) = e1(i)*c
            e2(i) = e2(i)*t
 16     continue
        e3(1) = e1(2)*e2(3) - e1(3)*e2(2)
        e3(2) = e1(3)*e2(1) - e1(1)*e2(3)
        e3(3) = e1(1)*e2(2) - e1(2)*e2(1)
        c = zero
        do 17 i = 1, 3, 1
            c = c + e3(i)*e3(i)
 17     continue
        c = sqrt(c)
        if (c .le. abs(e1(1))*epsi) then
            vali(1) = k
            vali(2) = f
            call utmess('F', 'INTEMAIL_24', ni=2, vali=vali)
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
        endif
    endif
    ds1 = 3*(sm(1) - 1)
    do 20 i = 1, 3, 1
        c = coordo(ds1 + i)
        b(i) = -c
        do 21 j = 1, 2, 1
            a(i,j) = coordo(3*(sm(j+1)-1)+i) - c
 21     continue
 20 end do
    do 30 i = 1, 3, 1
        c = zero
        do 31 j = 1, 3, 1
            c = max(c,abs(a(i,j)))
 31     continue
        if (abs(c) .gt. epsi*coordo(ds1+i)) then
            c = un/c
            do 32 j = 1, 3, 1
                a(i,j) = a(i,j)*c
 32         continue
            b(i) = b(i)*c
        endif
 30 end do
    call i3sl33(eps, a, b, x, typsl)
    if (typsl .eq. 'INCO') then
!        PLAN INTER DROITE = VIDE ==> FACE INTER SGT = VIDE
!        DONC ACTION = VIDE
    else if (typsl .eq. 'DETE') then
        r = x(1)
        s = x(2)
        t = x(3)
        if (abs(t-un) .le. epsi) then
            t = un
        endif
        if (abs(t) .le. epsi) then
            t = zero
        endif
        if (abs(r) .le. epsi) then
            r = zero
        endif
        if (abs(r-un) .le. epsi) then
            r = un
        endif
        if (abs(s) .le. epsi) then
            s = zero
        endif
        if (abs(s-un) .le. epsi) then
            s = un
        endif
        call i3ptrv(eps, lstpt, nbpt, t, djala1,&
                    ipos)
        if ((r .le. (un+epsi)) .and. (r .ge. -epsi) .and. (s .le. ( un+epsi)) .and.&
            (s .ge. -epsi) .and. ((r+s-un) .le. epsi ) .and. (t .le. (un+epsi)) .and.&
            (t .ge. -epsi) .and. ( .not. djala1 )) then
            if (abs(s) .lt. epsi) then
                arete = 1
                s = zero
            else if (abs(r) .lt. epsi) then
                arete = 3
                r = zero
            else if (abs(un-r-s) .lt. epsi) then
                arete = 2
            else
                arete = 0
            endif
            zr(lstpt(1) + nbpt) = t
            zi(lstpt(2) + nbpt) = f
            zi(lstpt(3) + nbpt) = arete
            zi(lstpt(4) + nbpt) = 0
            zr(lstpt(5) + 2*nbpt+1-1) = r
            zr(lstpt(5) + 2*nbpt+2-1) = s
            zi(lstpt(6) + nbpt) = nbpt + 1
            nbpt = nbpt + 1
        endif
    else if (typsl .eq. 'INDE') then
        call i3idfs(epsi, k, f, nba, sgt,&
                    cs, nbpt, lstpt, fink)
        fink = .true.
        if (nbpt .eq. 0) then
            call i3pdm2(epsi, e3, cs, nba, sgt,&
                        djala1)
            call i3pdm2(epsi, e3, cs, nba, sgt(4),&
                        djala2)
            if (djala1 .and. djala2) then
                call i3crtp(epsi, cs, sgt, zr(lstpt(5)), iret)
                if (iret .ne. 0) then
                    vali(1) = k
                    vali(2) = f
                    call utmess('F', 'INTEMAIL_24', ni=2, vali=vali)
                endif
                zr(lstpt(1)) = zero
                zi(lstpt(2)) = f
                zi(lstpt(3)) = 0
                zi(lstpt(4)) = 0
                zi(lstpt(6)) = 1
                call i3crtp(epsi, cs, sgt(4), zr(lstpt(5)+2), iret)
                if (iret .ne. 0) then
                    vali(1) = k
                    vali(2) = f
                    call utmess('F', 'INTEMAIL_24', ni=2, vali=vali)
                endif
                zr(lstpt(1)+1) = un
                zi(lstpt(2)+1) = f
                zi(lstpt(3)+1) = 0
                zi(lstpt(4)+1) = 0
                zi(lstpt(6)+1) = 2
                nbpt = -2
            endif
        else if (nbpt .eq. 1) then
            t = zr(lstpt(1))
            if ((abs(t-un) .lt. epsi) .or. (abs(t) .lt. epsi)) then
                if (abs(t-un) .lt. epsi) then
                    i = 1
                    j = 4
                    t = zero
                else
                    i = 4
                    j = 1
                    t = un
                endif
                call i3pdm2(epsi, e3, cs, nba, sgt(i),&
                            djala1)
                if (djala1) then
                    call i3crtp(epsi, cs, sgt(j), zr(lstpt(5)), iret)
                    call i3crtp(epsi, cs, sgt(i), zr(lstpt(5)+2), iret)
                    if (iret .eq. -1) then
                        vali(1) = k
                        vali(2) = f
                        call utmess('F', 'INTEMAIL_24', ni=2, vali=vali)
                    endif
                    zr(lstpt(1)+1) = t
                    zi(lstpt(2)+1) = f
                    zi(lstpt(3)+1) = 0
                    zi(lstpt(4)+1) = 0
                    zi(lstpt(6)+1) = 2
                    nbpt = -2
                endif
            else
                call i3pdm2(epsi, e3, cs, nba, sgt,&
                            djala1)
                if (djala1) then
                    call i3crtp(epsi, cs, sgt, zr(lstpt(5)+2), iret)
                    if (iret .eq. -1) then
                        vali(1) = k
                        vali(2) = f
                        call utmess('F', 'INTEMAIL_24', ni=2, vali=vali)
                    endif
                    zr(lstpt(1)+1) = zero
                    zi(lstpt(2)+1) = f
                    zi(lstpt(3)+1) = 0
                    zi(lstpt(4)+1) = 0
                    zi(lstpt(6)+1) = 2
                    nbpt = -2
                else
                    call i3pdm2(epsi, e3, cs, nba, sgt(4),&
                                djala1)
                    if (djala1) then
                        call i3crtp(epsi, cs, sgt(4), zr(lstpt(5)+2), iret)
                        if (iret .eq. -1) then
                            vali(1) = k
                            vali(2) = f
                            call utmess('F', 'INTEMAIL_24', ni=2, vali=vali)
                        endif
                        zr(lstpt(1)+1) = un
                        zi(lstpt(2)+1) = f
                        zi(lstpt(3)+1) = 0
                        zi(lstpt(4)+1) = 0
                        zi(lstpt(6)+1) = 2
                        nbpt = -2
                    endif
                endif
            endif
            if (nbpt .ne. -2) then
                nbpt = 0
            else
                r = zr(lstpt(1))
                s = zr(lstpt(1)+1)
            endif
        else if (nbpt .eq. 2) then
            nbpt = -2
            r = zr(lstpt(1))
            s = zr(lstpt(1)+1)
        else if (nbpt .gt. 2) then
            vali(1) = k
            vali(2) = f
            vali(3) = nbpt
            call utmess('F', 'INTEMAIL_25', ni=2, vali=vali)
        else
        endif
    else
        call utmess('F', 'INTEMAIL_8', sk=typsl)
    endif
end subroutine
