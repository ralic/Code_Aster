subroutine vpzvph(nn, acc, prerel, hh, ih,&
                  valpr, valpi, icnt, ier)
!
!----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!----------------------------------------------------------------------
!     PROCEDURE HQR
!     (CF. WILKINSON & REINSCH LINEAR ALGEBRA PP.363-365)
!     RECHERCHE DES VALEURS PROPRES D UNE MATRICE DE HESSENBERG REELLE
!     SUPERIEURE, HH, STOCKEE DANS LE TABLEAU HH(N,N) ET STOCKE LES
!     PARTIES REELLES DANS LE TABLEAU VALPR(N) ET LES PARTIES
!     IMAGINAIRES DANS LE TABLEAU VALPI(N).
!     ACC EST LA PRECISION MACHINE RELATIVE.
!     LA ROUTINE ECHOUE SI LA RECHERCHE DES VALEURS PROPRES NECESSITE
!     PLUS DE 30 ITERATIONS.
!
!
! --- DECLARATIONS
!
    implicit none
!
! ARGUMENTS
    integer :: nn, ih, ier
    real(kind=8) :: acc, prerel
    integer :: icnt(nn)
    real(kind=8) :: hh(ih, nn), valpr(nn), valpi(nn)
!
! VARIABLES LOCALES
    real(kind=8) :: norm, p, q, r, s, t, w, x, y, z
    integer :: i, itn, its, j, k, l, ll, m, m2, m3, mm
    integer :: n, n2, na, nhs
    logical :: notlst
!
!**********************************************************************
!                        DEBUT DU CODE EXECUTABLE
!**********************************************************************
!
!
! --- INITIALISATION
    ier = 0
    t = 0.0d0
    n = nn
    itn = 30*n
!
! --- CALCUL DE LA NORME DE LA MATRICE
    norm = 0.0d0
    k = 1
    do 40 i = 1, n
        do 20 j = k, n
            norm = norm + abs(hh(i,j))
20      continue
        k = i
40  end do
    nhs = n*(n+1)/2 + n - 1
60  continue
    if (n .eq. 0) goto 9999
    its = 0
    na = n - 1
!
! --- RECHERCHE D ELEMENTS SIMPLES DE LA SOUS-DIAGONALE
80  continue
    l = n + 1
    if (n .ge. 2) then
        do 100 ll = 2, n
            l = l - 1
            s = abs(hh(l-1,l-1)) + abs(hh(l,l))
            if (s .lt. prerel) then
                s = norm/dble(nhs)
            endif
            if (abs(hh(l,l-1)) .le. acc*s) then
                goto 120
            endif
100      continue
    endif
    l = 1
120  continue
    x = hh(n,n)
    if (l .eq. n) goto 360
    y = hh(na,na)
    w = hh(n,na)*hh(na,n)
    if (l .eq. na) goto 380
    if (itn .le. 0) then
        ier = 1
        goto 9999
    endif
    if ((its.eq.10) .or. (its.eq.20)) then
!
! --- DECALAGE
        t = t + x
        if (na .ge. 1) then
            do 140 i = 1, na
                hh(i,i) = hh(i,i) - x
140          continue
        endif
        hh(n,n) = 0.0d0
        s = abs(hh(n,na)) + abs(hh(na,n-2))
        x = 0.75d0*s
        y = 0.75d0*s
        w = -0.4375d0*s**2
    endif
    its = its + 1
    itn = itn - 1
!
! --- RECHERCHE DE DEUX ELEMENTS CONSECUTIFS DE LA SOUS-DIAGONALE
    if (l .le. (n-2)) then
        m = n - 1
        n2 = n - 2
        do 160 mm = l, n2
            m = m - 1
            z = hh(m,m)
            r = x - z
            s = y - z
            p = (r*s-w)/hh(m+1,m) + hh(m,m+1)
            q = hh(m+1,m+1) - z - r - s
            r = hh(m+2,m+1)
            s = abs(p) + abs(q) + abs(r)
            p = p/s
            q = q/s
            r = r/s
            if (m .eq. l) then
                goto 180
            endif
            if ((abs(hh(m,m-1))*(abs(q)+abs(r))) .le.&
                (acc*abs(p) *(abs(hh(m-1,m-1))+abs(z)+abs(hh(m+1,m+1))))) then
                goto 180
            endif
160      continue
    endif
180  continue
    m2 = m + 2
    if (m2 .le. n) then
        do 200 i = m2, n
            hh(i,i-2) = 0.0d0
200      continue
    endif
    m3 = m + 3
    if (m3 .le. n) then
        do 220 i = m3, n
            hh(i,i-3) = 0.0d0
220      continue
    endif
    if (m .gt. na) goto 80
!
! --- DOUBLE ETAPE QR CONCERNANT LES RANGEES L A N ET LES COLONNES M A N
    do 340 k = m, na
        notlst = .true.
        if (k .eq. na) notlst = .false.
        if (k .ne. m) then
            p = hh(k,k-1)
            q = hh(k+1,k-1)
            r = 0.0d0
            if (notlst) then
                r = hh(k+2,k-1)
            endif
            x = abs(p) + abs(q) + abs(r)
            if (x .eq. 0.0d0) goto 340
            p = p/x
            q = q/x
            r = r/x
        endif
        s = sqrt(p**2+q**2+r**2)
        if (p .lt. 0.0d0) then
            s = -s
        endif
        if (k .ne. m) then
            hh(k,k-1) = -s*x
        else
            if (l .ne. m) then
                hh(k,k-1) = -hh(k,k-1)
            endif
        endif
        p = p + s
        x = p/s
        y = q/s
        z = r/s
        q = q/p
        r = r/p
        if (k .le. n) then
!
! --- MODIFICATION DES RANGEES
            if (.not. notlst) then
                do 240 j = k, n
                    p = hh(k,j) + q*hh(k+1,j)
                    hh(k+1,j) = hh(k+1,j) - p*y
                    hh(k,j) = hh(k,j) - p*x
240              continue
                goto 280
            endif
            do 260 j = k, n
                p = hh(k,j) + q*hh(k+1,j) + r*hh(k+2,j)
                hh(k+2,j) = hh(k+2,j) - p*z
                hh(k+1,j) = hh(k+1,j) - p*y
                hh(k,j) = hh(k,j) - p*x
260          continue
        endif
280      continue
        j = n
        if ((k+3) .lt. n) then
            j = k + 3
        endif
        if (l .le. j) then
!
! --- MODIFICATION DES COLONNES
            if (.not. notlst) then
                do 300 i = l, j
                    p = x*hh(i,k) + y*hh(i,k+1)
                    hh(i,k+1) = hh(i,k+1) - p*q
                    hh(i,k) = hh(i,k) - p
300              continue
                goto 340
            endif
            do 320 i = l, j
                p = x*hh(i,k) + y*hh(i,k+1) + z*hh(i,k+2)
                hh(i,k+2) = hh(i,k+2) - p*r
                hh(i,k+1) = hh(i,k+1) - p*q
                hh(i,k) = hh(i,k) - p
320          continue
        endif
340  end do
    goto 80
!
! --- UNE RACINE ETE TROUVEE (REELLE)
360  continue
    valpr(n) = x + t
    valpi(n) = 0.0d0
    icnt(n) = its
    n = na
    goto 60
!
! --- DEUX RACINES ONT ETE TROUVEES
380  continue
    p = (y-x)/2.0d0
    q = p**2 + w
    y = sqrt(abs(q))
    x = x + t
    icnt(n) = -its
    icnt(na) = its
!
! --- PAIRE DE RACINES COMPLEXES CONJUGUEES
    if (q .le. 0.0d0) then
        valpr(na) = x + p
        valpr(n) = x + p
        valpi(na) = y
        valpi(n) = -y
    else
! --- PAIRE DE RACINES REELLES
        if (p .lt. 0.0d0) then
            y = -y
        endif
        y = p + y
        valpr(na) = x + y
        valpr(n) = x - w/y
        valpi(n) = 0.0d0
        valpi(na) = 0.0d0
    endif
    n = n - 2
    goto 60
!
9999  continue
end subroutine
