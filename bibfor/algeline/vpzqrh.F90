subroutine vpzqrh(h, neq, ih, k, l,&
                  wr, wi, z, iz, mxiter,&
                  ier, nitqr)
    implicit none
    include 'asterc/r8prem.h'
    integer :: neq, ih, k, l, iz, ier, nitqr
    real(kind=8) :: h(ih, neq), wr(neq), wi(neq), z(iz, neq)
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
!     RECHERCHE DES VALEURS PROPRES PAR LA METHODE QR SUR UNE MATRICE
!     MISE SOUS LA FORME DE HESSENBERG
!     ------------------------------------------------------------------
!     REFERENCE: F.L. BAUER - J.H. WILKINSON - C. REINSCH
!        HANDBOOK FOR AUTOMATIC COMPUTATION - LINEAR ALGEBRA - VOL.2
!        PAGE XXX
!     ------------------------------------------------------------------
    integer :: i, ien, ienm2, npl, ll, lb, naml, mm, m, mp2, ka, na
    integer :: iter, j, jj
    real(kind=8) :: epsmac, t, x, y, w, s, zz
    real(kind=8) :: r, p, q, rnorm, ra, sa, scale, vr, vi
    complex(kind=8) :: z3
    logical :: notlas
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ii, mxiter, nn
!-----------------------------------------------------------------------
    ier = 0
    epsmac = r8prem()
    nitqr = 0
!
!     --- STOCKER LES RACINES TROUVEES PAR VPZBAL (DIT LA BALANCE) ---
    rnorm = 0.0d0
    ka = 1
    do 10 i = 1, neq
        do 5 j = ka, neq
            rnorm = rnorm+abs(h(i,j))
 5      continue
        ka = i
        if (i .ge. k .and. i .le. l) goto 10
        wr(i) = h(i,i)
        wi(i) = 0.d0
10  end do
    ien = l
    t = 0.d0
!
!     --- RECHERCHE DES VALEURS PROPRES SUIVANTES ---
15  continue
    if (ien .lt. k) goto 145
    iter = 0
    na = ien-1
    ienm2 = na-1
!
!     --- RECHERCHE DU PLUS PETIT ELEMENT (SIMPLE) SUR LA SUR-DIAGONALE
20  continue
    npl = ien+k
    do 25 ll = k, ien
        lb = npl-ll
        if (lb .eq. k) goto 30
        s = abs(h(lb-1,lb-1))+abs(h(lb,lb))
        if (s .eq. 0.0d0) s = rnorm
        if (abs(h(lb,lb-1)) .le. epsmac*s) goto 30
25  end do
!
30  continue
    x = h(ien,ien)
    if (lb .eq. ien) goto 110
    y = h(na,na)
    w = h(ien,na)*h(na,ien)
    if (lb .eq. na) goto 115
    if (iter .eq. mxiter) goto 250
!
!     --- FORMER UN DECALAGE TOUT LES DIX COUPS ---
    iter = iter+1
    if (iter .gt. nitqr) then
        nitqr = iter
    endif
    if (mod(iter,10) .eq. 0) then
        t = t+x
        do 35 i = k, ien
            h(i,i) = h(i,i)-x
35      continue
        s = abs(h(ien,na))+abs(h(na,ienm2))
        x = 0.75d0*s
        y = x
        w = -0.4375d0*s*s
    endif
!
!     --- RECHERCHE DES 2 PLUS PETITS ELEMENTS SUR LA SUR-DIAGONALE
    naml = ienm2+lb
    do 45 mm = lb, ienm2
        m = naml-mm
        zz = h(m,m)
        r = x-zz
        s = y-zz
        p = (r*s-w)/h(m+1,m)+h(m,m+1)
        q = h(m+1,m+1)-zz-r-s
        r = h(m+2,m+1)
        s = abs(p)+abs(q)+abs(r)
        p = p/s
        q = q/s
        r = r/s
        if (m .eq. lb) goto 50
        if (abs(h(m,m-1))*(abs(q)+abs(r)) .le.&
            epsmac*abs(p)*(abs(h(m-1, m-1))+abs(zz)+abs(h(m+1,m+1)))) goto 50
45  end do
50  continue
    mp2 = m+2
    do 55 i = mp2, ien
        h(i,i-2) = 0.d0
        if (i .eq. mp2) goto 55
        h(i,i-3) = 0.d0
55  end do
!
!     EN AVANT POUR LE "DOUBLE QR" SUR LA SOUS MATRICE
!              LIGNES DE "L" A "EN" ET COLONNES DE "M" A "EN"
    do 105 ka = m, na
        notlas = ka.ne.na
        if (ka .eq. m) goto 60
        p = h(ka,ka-1)
        q = h(ka+1,ka-1)
        r = 0.d0
        if (notlas) r = h(ka+2,ka-1)
        x = abs(p)+abs(q)+abs(r)
        if (x .eq. 0.d0) goto 105
        p = p/x
        q = q/x
        r = r/x
60      continue
        s = sign(sqrt(p*p+q*q+r*r),p)
        if (ka .eq. m) then
            if (lb .ne. m) h(ka,ka-1) = -h(ka,ka-1)
        else
            h(ka,ka-1) = -s*x
        endif
        p = p+s
        x = p/s
        y = q/s
        zz = r/s
        q = q/p
        r = r/p
!        --- ALTERATION DES LIGNES ---
        do 80 j = ka, neq
            p = h(ka,j)+q*h(ka+1,j)
            if (notlas) then
                p = p+r*h(ka+2,j)
                h(ka+2,j) = h(ka+2,j)-p*zz
            endif
            h(ka+1,j) = h(ka+1,j)-p*y
            h(ka,j) = h(ka,j)-p*x
80      continue
        j = min(ien,ka+3)
!        --- ALTERATION DES COLONNES ---
        do 90 i = 1, j
            p = x*h(i,ka)+y*h(i,ka+1)
            if (.not.notlas) goto 85
            p = p+zz*h(i,ka+2)
            h(i,ka+2) = h(i,ka+2)-p*r
85          continue
            h(i,ka+1) = h(i,ka+1)-p*q
            h(i,ka) = h(i,ka)-p
90      continue
        if (iz .ge. neq) then
!           --- ON GARDE LES TRANSFORMATIONS POUR LES VECTEURS ----
            do 100 i = k, l
                p = x*z(i,ka)+y*z(i,ka+1)
                if (notlas) then
                    p = p+zz*z(i,ka+2)
                    z(i,ka+2) = z(i,ka+2)-p*r
                endif
                z(i,ka+1) = z(i,ka+1)-p*q
                z(i,ka) = z(i,ka)-p
100          continue
        endif
105  end do
    goto 20
!
!     ---------------------------- AU CAS PAR CAS  ---------------------
!
!     --- UNE RACINE TROUVEE ---
110  continue
    h(ien,ien) = x+t
    wr(ien) = h(ien,ien)
    wi(ien) = 0.d0
    ien = na
    goto 15
!
!     --- DEUX RACINES TROUVEES ---
115  continue
    p = (y-x)*0.5d0
    q = p*p+w
    zz = sqrt(abs(q))
    h(ien,ien) = x+t
    x = h(ien,ien)
    h(na,na) = y+t
    if (q .lt. 0.d0) goto 135
!
!     --- RACINES DOUBLES REELLES ---
    zz = p+sign(zz,p)
    wr(na) = x+zz
    wr(ien) = wr(na)
    if (zz .ne. 0.d0) wr(ien) = x-w/zz
    wi(na) = 0.d0
    wi(ien) = 0.d0
    x = h(ien,na)
!
!     --- SI X ET ZZ TROP PETIT ALORS ON FAIT UNE NORMALISATION   ---
!     --- MISE A L'ECHELLE OU RECADRAGE C'EST SELON VOTRE CULTURE ---
    scale = abs(x) + abs(zz)
    r = scale * sqrt( (x/scale)**2 + (zz/scale)**2 )
    p = x/r
    q = zz/r
!
!     --- ALTERATION DES LIGNES ---
    do 120 j = na, neq
        zz = h(na,j)
        h(na,j) = q*zz+p*h(ien,j)
        h(ien,j) = q*h(ien,j)-p*zz
120  end do
!
!     --- ALTERATION DES COLONNES ---
    do 125 i = 1, ien
        zz = h(i,na)
        h(i,na) = q*zz+p*h(i,ien)
        h(i,ien) = q*h(i,ien)-p*zz
125  end do
!
    if (iz .ge. neq) then
!        --- STOCKER LES TRANSFORMATIONS POUR LES VECTEURS ---
        do 130 i = k, l
            zz = z(i,na)
            z(i,na) = q*zz+p*z(i,ien)
            z(i,ien) = q*z(i,ien)-p*zz
130      continue
    endif
    goto 140
!
!     --- VALEURS COMPLEXES CONJUGUEES ---
135  continue
    wr(na) = x+p
    wr(ien) = x+p
    wi(na) = zz
    wi(ien) = -zz
140  continue
    ien = ienm2
    goto 15
!
!
!     --- TOUTES LES RACINES SONT TROUVEES, ON DEBUTE LA REMONTEE ---
145  continue
    if (iz .lt. neq) goto 9999
!
!     ---- ON S'OCCUPE MAINTENANT DES VECTEURS ---
!
    if (rnorm .eq. 0.d0) goto 9999
    do 220 nn = 1, neq
        ien = neq+1-nn
        p = wr(ien)
        q = wi(ien)
        na = ien-1
        if (q .gt. 0.d0) goto 220
        if (q .lt. 0.d0) goto 180
!
!        --- VECTEUR REEL ---
        m = ien
        h(ien,ien) = 1.d0
        if (na .eq. 0) goto 220
        do 175 ii = 1, na
            i = ien-ii
            w = h(i,i)-p
            r = h(i,ien)
            do 150 j = m, na
                r = r+h(i,j)*h(j,ien)
150          continue
            if (wi(i) .ge. 0.d0) goto 160
            zz = w
            s = r
            goto 175
160          continue
            m = i
            if (wi(i) .ne. 0.d0) goto 165
            t = w
            if (w .eq. 0.d0) t = epsmac*rnorm
            h(i,ien) = -r/t
            goto 175
!
!           RESOLUTION DANS LE CAS REEL ---
165          continue
            x = h(i,i+1)
            y = h(i+1,i)
            q = (wr(i)-p)*(wr(i)-p)+wi(i)*wi(i)
            t = (x*s-zz*r)/q
            h(i,ien) = t
            if (abs(x) .le. abs(zz)) then
                h(i+1,ien) = (-s-y*t)/zz
            else
                h(i+1,ien) = (-r-w*t)/x
            endif
175      continue
        goto 220
!
!        --- CAS OU LE DERNIER VECTEUR EST IMAGINAIRE ---
180      continue
        m = na
!        --- VECTEUR COMPLEXE ---
        if (abs(h(ien,na)) .le. abs(h(na,ien))) goto 185
        h(na,na) = q/h(ien,na)
        h(na,ien) = -(h(ien,ien)-p)/h(ien,na)
        goto 190
185      continue
        z3 = dcmplx(0.d0,-h(na,ien))/dcmplx(h(na,na)-p,q)
        h(na,na) = dble (z3)
        h(na,ien) = dimag(z3)
190      continue
        h(ien,na) = 0.d0
        h(ien,ien) = 1.d0
        ienm2 = na-1
        if (ienm2 .eq. 0) goto 220
        do 215 ii = 1, ienm2
            i = na-ii
            w = h(i,i)-p
            ra = 0.d0
            sa = h(i,ien)
            do 195 j = m, na
                ra = ra+h(i,j)*h(j,na)
                sa = sa+h(i,j)*h(j,ien)
195          continue
            if (wi(i) .lt. 0.d0) then
                zz = w
                r = ra
                s = sa
            else if (wi(i).eq.0.d0) then
                m = i
                z3 = dcmplx(-ra,-sa)/dcmplx(w,q)
                h(i,na) = dble (z3)
                h(i,ien) = dimag(z3)
            else
!
!              --- RESOUDRE LES EQUATIONS (EN COMPLEXE)
                m = i
                x = h(i,i+1)
                y = h(i+1,i)
                vr = (wr(i)-p)*(wr(i)-p)+wi(i)*wi(i)-q*q
                vi = (wr(i)-p)*q
                vi = vi+vi
                if (vr .eq. 0.d0 .and. vi .eq. 0.d0) vr=epsmac*rnorm*(&
                                                     abs(w) + abs(q)+abs(x)+abs(y)+abs(zz))
                z3 = dcmplx(x*r-zz*ra+q*sa,x*s-zz*sa-q*ra)/dcmplx(vr, vi)
                h(i,na) = dble (z3)
                h(i,ien) = dimag(z3)
                if (abs(x) .le. abs(zz)+abs(q)) then
                    z3 = dcmplx(-r-y*h(i,na),-s-y*h(i,ien))/dcmplx(zz, q)
                    h(i+1,na) = dble (z3)
                    h(i+1,ien) = dimag(z3)
                else
                    h(i+1,na) = (-ra-w*h(i,na)+q*h(i,ien))/x
                    h(i+1,ien) = (-sa-w*h(i,ien)-q*h(i,na))/x
                endif
            endif
215      continue
220  end do
!     --- FIN DE LA REMONTEE ---
!
!     --- VECTEURS DES RACINES ISOLEES ---
    do 230 i = 1, neq
        if (i .ge. k .and. i .le. l) goto 230
        do 225 j = i, neq
            z(i,j) = h(i,j)
225      continue
230  end do
    if (l .eq. 0) goto 9999
!
!     APPLICATION DES TRANSFORMATIONS ---
    do 245 jj = k, neq
        j = neq+k-jj
        m = min(j,l)
        do 240 i = k, l
            zz = 0.d0
            do 235 ka = k, m
                zz = zz+z(i,ka)*h(ka,j)
235          continue
            z(i,j) = zz
240      continue
245  end do
    goto 9999
!
!     PAS DE CONVERGEBCE APRES "MXITER" INTERATION
!         ==>  IER = L'INDICE DE LA VALEUR PROPRE COURANTE
!
250  continue
    ier = ien
    do 255 i = 1, ien
        wr(i) = 0.d0
        wi(i) = 0.d0
255  end do
    if (iz .ge. neq) then
        do 265 i = 1, neq
            do 260 j = 1, neq
                z(i,j) = 0.d0
260          continue
265      continue
    endif
!     --- SORTIE ---
9999  continue
end subroutine
