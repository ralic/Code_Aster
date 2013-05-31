subroutine i3iqps(epsi, k, f, desc, desctm,&
                  conexk, coordo, sgt, nbpt, lstpt,&
                  fink)
! aslint: disable=W1501
    implicit none
    include 'jeveux.h'
    include 'asterfort/i3crqp.h'
    include 'asterfort/i3idfs.h'
    include 'asterfort/i3pdm2.h'
    include 'asterfort/i3ptrv.h'
    include 'asterfort/i3rpqp.h'
    include 'asterfort/i3sl33.h'
    include 'asterfort/i3sl3r.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mesk.h'
    integer :: k, desc(*), desctm(*), conexk(*), nbpt, lstpt(*), f
    real(kind=8) :: epsi, sgt(*), coordo(*)
    logical :: fink
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
!     INTERSECTION FACE QUADANGLE PLANE F SGT (AB)
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
    character(len=4) :: typsl
    integer :: i, j, ds1, decf, adescm, arete, nbs, iret, ipos, vali(3)
    real(kind=8) :: a(3, 3), b(3), x(3), c, zero, un, r, s, t, normab
    real(kind=8) :: e1(3), e2(3), e3(3), cs(3, 4), lcara, tole
    real(kind=8) :: e1i(3), e2i(3)
    logical :: dedans, djala1, djala2
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    zero = 0.0d0
    un = 1.0d0
    normab = zero
!
! --- RECUPERATION DES NOEUDS SOMMET DE LA FACE ET DE SES COORDONNEES
!
    nbs = 4
    decf = 8 + f
    adescm = desctm(desc(k))
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
    s = cs(i,j+1) - cs(i,j)
    c = c + s*s
22  continue
    c = sqrt( c )
    lcara = min ( c, lcara )
    20 end do
    c = zero
    do 24, i = 1, 3, 1
    s = cs(i,4) - cs(i,1)
    c = c + s*s
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
    t = zero
    do 15, i = 1, 3, 1
    s = cs(i,2) - cs(i,1)
    r = cs(i,4) - cs(i,1)
    e1(i) = s
    e2(i) = r
    c = c + s*s
    t = t + r*r
    15 end do
    c = sqrt(c)
    t = sqrt(t)
    if ((c.le.epsi) .or. (t.le.epsi)) then
        vali(1) = k
        vali(2) = f
        call u2mesi('F', 'INTEMAIL_24', 2, vali)
    endif
    do 16, i = 1, 3, 1
    e1(i) = e1(i)/c
    e2(i) = e2(i)/t
    16 end do
    e3(1) = e1(2)*e2(3) - e1(3)*e2(2)
    e3(2) = e1(3)*e2(1) - e1(1)*e2(3)
    e3(3) = e1(1)*e2(2) - e1(2)*e2(1)
    c = zero
    do 17, i = 1, 3, 1
    c = c + e3(i)*e3(i)
    17 end do
    c = sqrt(c)
    if (c .le. epsi) then
        vali(1) = k
        vali(2) = f
        call u2mesi('F', 'INTEMAIL_24', 2, vali)
    endif
    e3(1) = e3(1)/c
    e3(2) = e3(2)/c
    e3(3) = e3(3)/c
    e2(1) = -e1(2)*e3(3) + e1(3)*e3(2)
    e2(2) = -e1(3)*e3(1) + e1(1)*e3(3)
    e2(3) = -e1(1)*e3(2) + e1(2)*e3(1)
    c = sqrt(e2(1)*e2(1)+e2(2)*e2(2)+e2(3)*e2(3))
    e2(1) = e2(1)/c
    e2(2) = e2(2)/c
    e2(3) = e2(3)/c
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
    if ((abs(abs(e1(1))-un) .le. tole) .and. ((e1(2) .ne. zero) .or. (e1(3) .ne. zero))) then
        if (abs(e1(1)) .ne. un) then
            e1i(1) = sign(un,e1(1))
            e1i(2) = zero
            e1i(3) = zero
            call i3sl3r(e1, e1i, e3, cs)
            e1(1) = sign(un,e1(1))
            e1(3) = zero
            e1(2) = zero
        endif
        else if ( (abs(abs(e1(2))-un) .le. tole) .and. ((e1(1) .ne. zero)&
    .or. (e1(3) .ne. zero)) ) then
        if (abs(e1(2)) .ne. un) then
            e1i(2) = sign(un,e1(2))
            e1i(1) = zero
            e1i(3) = zero
            call i3sl3r(e1, e1i, e3, cs)
            e1(2) = sign(un,e1(2))
            e1(1) = zero
            e1(3) = zero
        endif
        else if ( (abs(abs(e1(3))-un) .le. tole) .and. ((e1(2) .ne. zero)&
    .or. (e1(1) .ne. zero)) ) then
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
    if ((abs(abs(e2(1))-un) .le. tole) .and. ((e2(2) .ne. zero) .or. (e2(3) .ne. zero))) then
        if (abs(e2(1)) .ne. un) then
            e2i(1) = sign(un,e2(1))
            e2i(3) = zero
            e2i(2) = zero
            call i3sl3r(e2, e2i, e3, cs)
            e2(1) = sign(un,e2(1))
            e2(3) = zero
            e2(2) = zero
        endif
        else if ( (abs(abs(e2(2))-un) .le. tole) .and. ((e2(1) .ne. zero)&
    .or. (e2(3) .ne. zero)) ) then
        if (abs(e2(2)) .ne. un) then
            e2i(2) = sign(un,e2(2))
            e2i(1) = zero
            e2i(3) = zero
            call i3sl3r(e2, e2i, e3, cs)
            e2(2) = sign(un,e2(2))
            e2(1) = zero
            e2(3) = zero
        endif
        else if ( (abs(abs(e2(3))-un) .le. tole) .and. ((e2(1) .ne. zero)&
    .or. (e2(2) .ne. zero)) ) then
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
! --- UN DEUXIEME TOUR DE PASSE-PASSE POUR METTRE LE POINT 3 DANS LE
!     PLAN DE LA FACE
!
    c = zero
    do 80, i = 1, 3, 1
    c = c - ( (cs(i,3)-cs(i,1)) * e3(i) )
    80 end do
    do 82, j = 1, 3, 1
    cs(j,3) = cs(j,3) + ( c * e3(j) )
    82 end do
!
! --- RECHERCHE DE L'INTERSECTION FACE SEGMENT AB
!     MATRICE A : VECTEUR DE LA FACE
!     VECTEUR B : COORDONNEES DU NOEUD 1 DE LA FACE
!     VECTEUR C : SEGMENT AB
!
    do 25, i = 1, 3, 1
    a(i,1) = e1(i)
    a(i,2) = e2(i)
    c = sgt(i+3) - sgt(i)
    normab = normab + c*c
    b(i) = -cs(i,1)
    25 end do
    normab = sqrt(normab)
    a(1,3) = zero
    a(2,3) = zero
    a(3,3) = -normab
    do 30, i = 1, 3, 1
    c = zero
    do 31, j = 1, 3, 1
    c = max(c,abs(a(i,j)))
31  continue
    if (c .gt. epsi) then
        do 32, j = 1, 3, 1
        a(i,j) = a(i,j)/c
32      continue
        b(i) = b(i)/c
    endif
    30 end do
!
! --- RESOLUTION DU SYSTEME
!
    call i3sl33(tole, a, b, x, typsl)
!
    if (typsl .eq. 'INCO') then
!          -----------------
!        PLAN INTER DROITE = VIDE ==> FACE INTER SGT = VIDE
!        DONC ACTION = VIDE
!
    else if (typsl .eq. 'DETE') then
!              -----------------
        r = x(1)
        s = x(2)
        t = x(3)
        if (abs(t) .le. tole) then
            t = zero
        else if (abs(t-un) .le. tole) then
            t = un
        endif
        if (abs(r+un) .lt. tole) then
            r = -un
        else if (abs(r-un) .lt. tole) then
            r = un
        endif
        if (abs(un-s) .lt. tole) then
            s = un
        else if (abs(un+s) .lt. tole) then
            s = -un
        endif
        call i3ptrv(tole, lstpt, nbpt, t, djala1,&
                    ipos)
        if ((t.ge.zero) .and. (t.le.un) .and. (.not.djala1)) then
            x(1) = zero
            x(2) = zero
            x(3) = t*normab
            call i3pdm2(epsi, e3, cs, nbs, x,&
                        dedans)
            if (dedans) then
                do 50, i = 1, 3, 1
                x(i) = cs(i,1)
                cs(i,1) = zero
50              continue
                call i3rpqp(x, e1, e2, e3, cs(1, 2),&
                            nbs-1)
                call i3crqp(epsi, epsi, cs, r, s,&
                            x, iret)
                if (iret .eq. -1) then
                    vali(1) = k
                    vali(2) = f
                    call u2mesi('F', 'INTEMAIL_24', 2, vali)
                else
                    r = x(1)
                    s = x(2)
                    if (abs(r+un) .lt. tole) then
                        arete = 4
                        r = -un
                    else if (abs(r-un) .lt. tole) then
                        arete = 2
                        r = un
                    else if (abs(un-s) .lt. tole) then
                        arete = 3
                        s = un
                    else if (abs(un+s) .lt. tole) then
                        arete = 1
                        s = -un
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
            endif
        endif
!
    else if (typsl .eq. 'INDE') then
!              -----------------
!        ---> NIVEAU DIRECTEMENT INFERRIEUR
        call i3idfs(tole, k, f, nbs, sgt,&
                    cs, nbpt, lstpt, fink)
        fink = .true.
        if (nbpt .eq. 0) then
            call i3pdm2(epsi, e3, cs, nbs, sgt,&
                        djala1)
            call i3pdm2(epsi, e3, cs, nbs, sgt(4),&
                        djala2)
            if (djala1 .and. djala2) then
                do 60, i = 1, 3, 1
                x(i) = cs(i,1)
                cs(i,1) = zero
                a (i,1) = sgt(i)
                a (i,2) = sgt(i+3)
60              continue
                call i3rpqp(x, e1, e2, e3, cs(1, 2),&
                            nbs-1)
                call i3rpqp(x, e1, e2, e3, a,&
                            2)
                r = a(1,1)
                s = a(2,1)
                call i3crqp(epsi, epsi, cs, r, s,&
                            zr(lstpt(5)), iret)
                if (iret .eq. -1) then
                    vali(1) = k
                    vali(2) = f
                    call u2mesi('F', 'INTEMAIL_24', 2, vali)
                endif
                zr(lstpt(1)) = zero
                zi(lstpt(2)) = f
                zi(lstpt(3)) = 0
                zi(lstpt(4)) = 0
                zi(lstpt(6)) = 1
                r = a(1,2)
                s = a(2,2)
                call i3crqp(epsi, epsi, cs, r, s,&
                            zr(lstpt(5)+2), iret)
                if (iret .eq. -1) then
                    vali(1) = k
                    vali(2) = f
                    call u2mesi('F', 'INTEMAIL_24', 2, vali)
                endif
                zr(lstpt(1)+1) = un
                zi(lstpt(2)+1) = f
                zi(lstpt(3)+1) = 0
                zi(lstpt(4)+1) = 0
                zi(lstpt(6)+1) = 1
                nbpt = -2
            endif
        else if (nbpt .eq. 1) then
            t = zr(lstpt(1))
            if ((abs(t).le.tole) .or. (abs(t-un).le.tole)) then
                if (abs(t-un) .le. tole) then
                    i = 1
                    j = 4
                    t = zero
                else
                    j = 1
                    i = 4
                    t = un
                endif
                call i3pdm2(epsi, e3, cs, nbs, sgt(i),&
                            djala1)
                if (djala1) then
                    do 61, ds1 = 1, 3, 1
                    x (ds1) = cs(ds1,1)
                    cs(ds1,1) = zero
                    a (ds1,1) = sgt(i-1+ds1)
                    a (ds1,2) = sgt(j-1+ds1)
61                  continue
                    call i3rpqp(x, e1, e2, e3, cs(1, 2),&
                                nbs-1)
                    call i3rpqp(x, e1, e2, e3, a,&
                                2)
                    r = a(1,1)
                    s = a(2,1)
                    call i3crqp(epsi, epsi, cs, r, s,&
                                zr(lstpt(5)+2), iret)
                    if (iret .eq. -1) then
                        vali(1) = k
                        vali(2) = f
                        call u2mesi('F', 'INTEMAIL_24', 2, vali)
                    endif
                    r = a(1,2)
                    s = a(2,2)
                    call i3crqp(epsi, epsi, cs, r, s,&
                                zr(lstpt(5)), iret)
                    if (iret .eq. -1) then
                        vali(1) = k
                        vali(2) = f
                        call u2mesi('F', 'INTEMAIL_24', 2, vali)
                    endif
                    zr(lstpt(1)+1) = t
                    zi(lstpt(2)+1) = f
                    zi(lstpt(3)+1) = 0
                    zi(lstpt(4)+1) = 0
                    zi(lstpt(6)+1) = 2
                    nbpt = -2
                endif
            else
                call i3pdm2(epsi, e3, cs, nbs, sgt,&
                            djala1)
                if (djala1) then
                    do 62, i = 1, 3, 1
                    x (i) = cs(i,1)
                    cs(i,1) = zero
                    a (i,1) = sgt(i)
62                  continue
                    call i3rpqp(x, e1, e2, e3, cs(1, 2),&
                                nbs-1)
                    call i3rpqp(x, e1, e2, e3, a,&
                                1)
                    r = a(1,1)
                    s = a(2,1)
                    call i3crqp(epsi, epsi, cs, r, s,&
                                zr(lstpt(5)+2), iret)
                    if (iret .eq. -1) then
                        vali(1) = k
                        vali(2) = f
                        call u2mesi('F', 'INTEMAIL_24', 2, vali)
                    endif
                    zr(lstpt(1)+1) = zero
                    zi(lstpt(2)+1) = f
                    zi(lstpt(3)+1) = 0
                    zi(lstpt(4)+1) = 0
                    zi(lstpt(6)+1) = 2
                    nbpt = -2
                else
                    call i3pdm2(epsi, e3, cs, nbs, sgt(4),&
                                djala1)
                    if (djala1) then
                        do 63, i = 1, 3, 1
                        x (i) = cs(i,1)
                        cs(i,1) = zero
                        a (i,1) = sgt(i+3)
63                      continue
                        call i3rpqp(x, e1, e2, e3, cs(1, 2),&
                                    nbs-1)
                        call i3rpqp(x, e1, e2, e3, a,&
                                    1)
                        r = a(1,1)
                        s = a(2,1)
                        call i3crqp(epsi, epsi, cs, r, s,&
                                    zr(lstpt(5)+2), iret)
                        if (iret .eq. -1) then
                            vali(1) = k
                            vali(2) = f
                            call u2mesi('F', 'INTEMAIL_24', 2, vali)
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
            endif
        else if (nbpt .eq. 2) then
            nbpt = -2
        else if (nbpt .gt. 2) then
            vali(1) = k
            vali(2) = f
            vali(3) = nbpt
            call u2mesi('F', 'INTEMAIL_26', 3, vali)
        endif
!
    else
        call u2mesk('F', 'INTEMAIL_8', 1, typsl)
    endif
!
end subroutine
