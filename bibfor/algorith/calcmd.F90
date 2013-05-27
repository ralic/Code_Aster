subroutine calcmd(np1, a, a0, n, m,&
                  typj, vg, vgt, vgt0, vd,&
                  vd0, rr, rr0, ri, n2,&
                  ier, ichoc, premac, prerel, mtmp1,&
                  mtmp2, ttr, u, w, d,&
                  intge1, intge2, indx, indxf, loc)
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE  CRP_21
!-----------------------------------------------------------------------
! DESCRIPTION : DIAGONALISATION DE LA MATRICE DE RAIDEUR DU SYSTEME
! -----------   A L'INSTANT N+1
!
!               APPELANTS : ALITMI, NEWTON
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterfort/indexx.h'
    include 'asterfort/prmama.h'
    include 'asterfort/trvpmd.h'
    include 'asterfort/tstjac.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vpzbaa.h'
    include 'asterfort/vpzbab.h'
    include 'asterfort/vpzhea.h'
    include 'asterfort/vpzheb.h'
    include 'asterfort/vpzqrs.h'
    include 'asterfort/vpzvph.h'
    integer :: np1
    real(kind=8) :: a(np1, *), a0(np1, *)
    integer :: n, m, typj
    real(kind=8) :: vg(np1, *), vgt(np1, *), vgt0(np1, *), vd(np1, *)
    real(kind=8) :: vd0(np1, *), rr(*), rr0(*), ri(*)
    integer :: n2, ier, ichoc
    real(kind=8) :: premac, prerel, mtmp1(np1, *), mtmp2(np1, *), ttr(n2, *)
    real(kind=8) :: u(*), w(*), d(*)
    integer :: intge1(*), intge2(*), indx(*), indxf(*)
    logical :: loc(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: ib, k, l, i, j, ifail, iima, ineg, iprod
    real(kind=8) :: temp, temp1, tol, eps
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  ABS, SIGN, SQRT
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL   PRMAMA, TSTJAC,
!    &           VPZBAA, VPZHEA, VPZVPH, INDEXX, VPZQRS, VPZHEB, VPZBAB,
!    &           TRVPMD
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
! 0. INITIALISATIONS
!    ---------------
    ier = 0
    ineg = 0
    iima = 0
    typj = 0
    ib = 2
    tol = 1.0d-08
    eps = premac
!
!---- CAS DU SYSTEME EN CHOC A L'INSTANT N+1
!----  => DIAGONALISATION DE LA MATRICE DE RAIDEUR
!
    if (ichoc .eq. 1) then
!
!======================================================================
! 1.     TEST SUR LA VARIATION DU JACOBIEN
!======================================================================
!
        call tstjac(np1, n, typj, a, a0)
!
!------- VARIATION DE LA MATRICE DE RAIDEUR ENTRE LES INSTANTS N ET N+1
!
        if (typj .eq. 1) then
!
!---------- SI UN SEUL VECTEUR PROPRE EST SOUHAITE
!
            if ((n.eq.1) .and. (m.eq.1)) then
                rr(1) = sqrt(a(1,1))
                rr0(1) = sqrt(a(1,1))
                vd(1,1) = 1.0d0
                vd0(1,1) = 1.0d0
                vgt(1,1) = 1.0d0
                vgt0(1,1) = 1.0d0
!
!---------- SINON : CALCUL DES N VALEURS PROPRES ET DES M PREMIERS
!---------- VECTEURS PROPRES A DROITE ET A GAUCHE
!
            else
!
!=======================================================================
! 2.           MATRICE A ET VECTEURS PROPRES A DROITE VD
!=======================================================================
!
!............. STOCKAGE DE LA MATRICE A DANS LA MATRICE MTMP2
                do 1 j = 1, n
                    do 2 i = 1, n
                        mtmp2(i,j) = a(i,j)
 2                  continue
 1              continue
!
!............. CONDITIONNEMENT DE LA MATRICE MTMP2
                call vpzbaa(n, ib, mtmp2, np1, k,&
                            l, d)
!
!............. STOCKAGE DE LA MATRICE CONDITIONNEE MTMP2 DANS MTMP1
                do 3 j = 1, n
                    do 4 i = 1, n
                        mtmp1(i,j) = mtmp2(i,j)
 4                  continue
 3              continue
!
!............. CALCUL DE LA MATRICE DE HESSENBERG, ECRASEMENT DE MTMP1
                call vpzhea(n, k, l, mtmp1, np1,&
                            intge1)
!
!............. STOCKAGE DE LA MATRICE MTMP1 DANS LA MATRICE MTMP2
                do 5 j = 1, n
                    do 6 i = 1, n
                        mtmp2(i,j) = mtmp1(i,j)
 6                  continue
 5              continue
!
!............. CALCUL DES VALEURS PROPRES DE LA MATRICE MTMP2
                call vpzvph(n, eps, prerel, mtmp2, np1,&
                            rr, ri, intge2, ifail)
                if (ifail .ne. 0) call u2mess('F', 'ALGORITH_62')
!
!............. VERIFICATION DES VALEURS PROPRES DE LA MATRICE MTMP2
!............. (VALEURS PROPRES REELLES)
                do 100 i = 1, n
                    if (ri(i) .gt. (tol*rr(i))) then
                        ier = 1
                        if (rr(i) .lt. 0.0d0) ineg = 1
                        if (ri(i) .gt. abs(tol*rr(i))) iima = 1
                    endif
100              continue
!
                if (ier .ne. 0) then
!
                    if (iima .eq. 1) call u2mess('A', 'ALGORITH_63')
!
                    if (ineg .eq. 1) call u2mess('A', 'ALGORITH_64')
!
                    goto 999
!
                endif
!
!............. RECHERCHE DES M PREMIERS MODES (TRI SUR LA PARTIE REELLE
!............. DES VALEURS PROPRES DE LA MATRICE MTMP2)
                do 7 i = 1, n
                    loc(i) = .false.
 7              continue
                call indexx(n, rr, indx)
                do 8 i = 1, m
                    loc(indx(i)) = .true.
 8              continue
!
!............. CALCUL DES M PREMIERS VECTEURS PROPRES
!............. DE LA MATRICE MTMP1
                call vpzqrs(n, m, mtmp1, np1, loc,&
                            ri, rr, vd, np1, ttr,&
                            n2, u, w, eps, ifail)
                if (ifail .ne. 0) call u2mess('F', 'ALGORITH_65')
!
!............. RETOUR AUX M PREMIERS VECTEURS PROPRES
!............. DE LA MATRICE CONDITIONNEE
                call vpzheb(k, l, m, mtmp1, np1,&
                            intge1, vd, np1, n)
!
!............. RETOUR AUX M PREMIERS VECTEURS PROPRES
!............. DE LA MATRICE A (VD)
                call vpzbab(n, k, l, m, d,&
                            vd, np1)
!
!............. RECUPERATION DES M VALEURS PROPRES REELLES
!............. DE LA MATRICE A
                call trvpmd(np1, n, m, rr, loc,&
                            indxf, intge1, intge2, u, w)
!
!............. CALCUL DE LA MATRICE D'ORDRE M CONSTITUEE DES M PREMIERS
!............. VECTEURS PROPRES A DROITE (VD)
                do 11 j = 1, n
                    do 12 i = 1, n
                        mtmp1(i,j) = vd(i,indxf(j))
12                  continue
11              continue
                do 13 j = 1, n
                    do 14 i = 1, n
                        vd(i,j) = mtmp1(i,j)
14                  continue
13              continue
!
!=======================================================================
! 3.           TRANSPOSEE DE LA MATRICE A ET VECTEURS PROPRES
!              A GAUCHE VG
!=======================================================================
!
!............. STOCKAGE DE LA MATRICE AT (TRANSPOSEE DE LA MATRICE A)
!............. DANS LA MATRICE MTMP2
                do 15 j = 1, n
                    do 16 i = 1, n
                        mtmp2(i,j) = a(j,i)
16                  continue
15              continue
!
!............. CONDITIONNEMENT DE LA MATRICE MTMP2
                call vpzbaa(n, ib, mtmp2, np1, k,&
                            l, d)
!
!............. STOCKAGE DE LA MATRICE CONDITIONNEE MTMP2
!............. DANS LA MATRICE MTMP1
                do 19 j = 1, n
                    do 20 i = 1, n
                        mtmp1(i,j) = mtmp2(i,j)
20                  continue
19              continue
!
!............. CALCUL DE LA MATRICE DE HESSENBERG, ECRASEMENT DE MTMP1
                call vpzhea(n, k, l, mtmp1, np1,&
                            intge1)
!
!............. STOCKAGE DE LA MATRICE MTMP1 DANS LA MATRICE MTMP2
                do 21 j = 1, n
                    do 22 i = 1, n
                        mtmp2(i,j) = mtmp1(i,j)
22                  continue
21              continue
!
!............. CALCUL DES VALEURS PROPRES DE LA MATRICE MTMP2
                call vpzvph(n, eps, prerel, mtmp2, np1,&
                            rr, ri, intge2, ifail)
                if (ifail .ne. 0) call u2mess('F', 'ALGORITH_62')
!
!............. VERIFICATION DES VALEURS PROPRES DE LA MATRICE MTMP2
!............. (VALEURS PROPRES REELLES)
                ier = 0
                ineg = 0
                iima = 0
                do 200 i = 1, n
                    if (ri(i) .gt. (tol*rr(i))) then
                        ier = 1
                        if (rr(i) .lt. 0.0d0) ineg = 1
                        if (ri(i) .gt. abs(tol*rr(i))) iima = 1
                    endif
200              continue
!
                if (ier .ne. 0) then
!
                    if (iima .eq. 1) call u2mess('A', 'ALGORITH_63')
!
                    if (ineg .eq. 1) call u2mess('A', 'ALGORITH_64')
!
                    goto 999
!
                endif
!
!............. RECHERCHE DES M PREMIERS MODES (TRI SUR LA PARTIE REELLE
!............. DES VALEURS PROPRES DE LA MATRICE MTMP2)
                do 23 i = 1, n
                    loc(i) = .false.
23              continue
                call indexx(n, rr, indx)
                do 24 i = 1, m
                    loc(indx(i)) = .true.
24              continue
!
!............. CALCUL DES M PREMIERS VECTEURS PROPRES
!............. DE LA MATRICE MTMP1
                call vpzqrs(n, m, mtmp1, np1, loc,&
                            ri, rr, vg, np1, ttr,&
                            n2, u, w, eps, ifail)
                if (ifail .ne. 0) call u2mess('F', 'ALGORITH_65')
!
!............. RETOUR AUX M PREMIERS VECTEURS PROPRES
!............. DE LA MATRICE CONDITIONNEE
                call vpzheb(k, l, m, mtmp1, np1,&
                            intge1, vg, np1, n)
!
!............. RETOUR AUX M PREMIERS VECTEURS PROPRES
!............. DE LA MATRICE AT (VG)
                call vpzbab(n, k, l, m, d,&
                            vg, np1)
!
!............. RECUPERATION DES M VALEURS PROPRES REELLES
!............. DE LA MATRICE AT
                call trvpmd(np1, n, m, rr, loc,&
                            indxf, intge1, intge2, u, w)
!
!............. CALCUL DE LA MATRICE D'ORDRE M CONSTITUEE DES M PREMIERS
!............. VECTEURS PROPRES A GAUCHE (VG)
                do 27 j = 1, n
                    do 28 i = 1, n
                        mtmp2(i,j) = vg(i,indxf(j))
28                  continue
27              continue
                do 29 j = 1, n
                    do 30 i = 1, n
                        vg(i,j) = mtmp2(i,j)
30                  continue
29              continue
!
!............. CALCUL DE LA MATRICE DIAGONALE B = (TRANSPOSEE DE VG)*VD
                do 31 j = 1, n
                    do 32 i = 1, n
                        mtmp2(j,i) = vg(i,j)
32                  continue
31              continue
                iprod = 1
                call prmama(iprod, mtmp2, np1, n, n,&
                            vd, np1, n, n, mtmp1,&
                            np1, n, n, ier)
                if (ier .ne. 0) call u2mess('F', 'ALGORITH_66')
!
!............. NORMALISATION DES VECTEURS PROPRES VD ET VG
                do 33 j = 1, m
                    temp = abs(mtmp1(j,j))
                    temp1 = sign(temp,mtmp1(j,j))
                    do 34 i = 1, n
                        vd(i,j) = vd(i,j)/temp1
34                  continue
33              continue
                do 35 j = 1, n
                    do 36 i = 1, n
                        if (j .le. m) then
                            vgt(j,i) = vg(i,j)
                        else
                            vgt(j,i) = 0.0d0
                        endif
36                  continue
35              continue
                do 37 i = 1, n
                    rr(i) = sqrt(rr(i))
37              continue
!
!............. SAUVEGARDE DES RESULTATS DANS RR0, VGT0, ET VD0
                do 38 i = 1, n
                    rr0(i) = rr(i)
38              continue
                do 39 j = 1, n
                    do 40 i = 1, m
                        vgt0(i,j) = vgt(i,j)
40                  continue
39              continue
                do 41 j = 1, m
                    do 42 i = 1, n
                        vd0(i,j) = vd(i,j)
42                  continue
41              continue
!
            endif
!
!---------- FIN DU CALCUL DES ELEMENTS PROPRES
!
!------- PAS DE VARIATION DE LA MATRICE DE RAIDEUR
!------- ENTRE LES INSTANTS N ET N+1
!
        else
!
!.......... RECUPERATION DES VALEURS CALCULEES A LA DIAGONALISATION
!.......... PRECEDENTE
!
            do 43 i = 1, n
                rr(i) = rr0(i)
43          continue
            do 44 j = 1, n
                do 45 i = 1, m
                    vgt(i,j) = vgt0(i,j)
45              continue
44          continue
            do 46 j = 1, m
                do 47 i = 1, n
                    vd(i,j) = vd0(i,j)
47              continue
46          continue
!
        endif
!
!------- FIN DU CAS SYSTEME EN CHOC
!
!---- CAS PARTICULIER OU LE SYSTEME EST EN VOL A L'INSTANT N+1
!----  => PAS DE DIAGONALISATION
!
    else
!
        do 48 i = 1, n
            rr(i) = sqrt(a(i,i))
48      continue
!
    endif
!
999  continue
!
! --- FIN DE CALCMD.
end subroutine
