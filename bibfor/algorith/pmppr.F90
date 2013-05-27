subroutine pmppr(amat, na1, na2, ka, bmat,&
                 nb1, nb2, kb, cmat, nc1,&
                 nc2)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!***********************************************************************
!    P. RICHARD     DATE 12/03/91
!-----------------------------------------------------------------------
!  BUT:  PRODUIT DE DEUX MATRICES STOCKEE PLEINE AVEC PRISE EN COMPTE
! DE TRANSPOSITION PAR L'INTERMEDIAIRE D'INDICATEUR K
    implicit none
!
!-----------------------------------------------------------------------
!
! AMAT     /I/: PREMIERE MATRICE
! NA1      /I/: NOMBRE DE LIGNE DE LA PREMIERE MATRICE
! NA2      /I/: NOMBRE DE COLONNE DE LA PREMIERE MATRICE
! KB       /I/: INDICATEUR TRANSPOSITION PREMIERE MATRICE
! BMAT     /I/: DEUXIEME MATRICE
! NB1      /I/: NOMBRE DE LIGNE DE LA DEUXIEME MATRICE
! NB2      /I/: NOMBRE DE COLONNE DE LA DEUXIEME MATRICE
! KB       /I/: INDICATEUR TRANSPOSITION DEUXIEME MATRICE
! CMAT     /I/: MATRICE RESULTAT
! NC1      /I/: NOMBRE DE LIGNE DE LA MATRICE RESULTAT
! NC2      /I/: NOMBRE DE COLONNE DE LA MATRICE RESULTAT
!
!-----------------------------------------------------------------------
!
    include 'asterfort/u2mesg.h'
    real(kind=8) :: amat(na1, na2), bmat(nb1, nb2), cmat(nc1, nc2)
!
!-----------------------------------------------------------------------
!
!
!   CAS SANS TRANSPOSITION
!
!-----------------------------------------------------------------------
    integer :: i, j, k, ka, kb, na1, na2
    integer :: nb1, nb2, nc1, nc2
!-----------------------------------------------------------------------
    if (ka .eq. 1 .and. kb .eq. 1) then
        if (na2 .ne. nb1) then
            call u2mesg('F', 'ALGORITH13_91', 0, ' ', 0,&
                        0, 0, 0.d0)
        endif
        if (nc1 .ne. na1 .or. nc2 .ne. nb2) then
            call u2mesg('F', 'ALGORITH13_92', 0, ' ', 0,&
                        0, 0, 0.d0)
        endif
        do 10 i = 1, na1
            do 20 j = 1, nb2
                cmat(i,j)=0.d0
                do 30 k = 1, nb1
                    cmat(i,j)=cmat(i,j)+amat(i,k)*bmat(k,j)
30              continue
20          continue
10      continue
!
    endif
!
    if (ka .eq. -1 .and. kb .eq. 1) then
        if (na1 .ne. nb1) then
            call u2mesg('F', 'ALGORITH13_91', 0, ' ', 0,&
                        0, 0, 0.d0)
        endif
        if (nc1 .ne. na2 .or. nc2 .ne. nb2) then
            call u2mesg('F', 'ALGORITH13_92', 0, ' ', 0,&
                        0, 0, 0.d0)
        endif
        do 40 i = 1, na2
            do 50 j = 1, nb2
                cmat(i,j)=0.d0
                do 60 k = 1, nb1
                    cmat(i,j)=cmat(i,j)+amat(k,i)*bmat(k,j)
60              continue
50          continue
40      continue
!
    endif
!
!
    if (ka .eq. 1 .and. kb .eq. -1) then
        if (na2 .ne. nb2) then
            call u2mesg('F', 'ALGORITH13_91', 0, ' ', 0,&
                        0, 0, 0.d0)
        endif
        if (nc1 .ne. na1 .or. nc2 .ne. nb1) then
            call u2mesg('F', 'ALGORITH13_92', 0, ' ', 0,&
                        0, 0, 0.d0)
        endif
        do 70 i = 1, na1
            do 80 j = 1, nb1
                cmat(i,j)=0.d0
                do 90 k = 1, na2
                    cmat(i,j)=cmat(i,j)+amat(i,k)*bmat(j,k)
90              continue
80          continue
70      continue
!
    endif
!
!
    if (ka .eq. -1 .and. kb .eq. -1) then
        if (na1 .ne. nb2) then
            call u2mesg('F', 'ALGORITH13_91', 0, ' ', 0,&
                        0, 0, 0.d0)
        endif
        if (nc1 .ne. na2 .or. nc2 .ne. nb1) then
            call u2mesg('F', 'ALGORITH13_92', 0, ' ', 0,&
                        0, 0, 0.d0)
        endif
        do 100 i = 1, na2
            do 110 j = 1, nb1
                cmat(i,j)=0.d0
                do 120 k = 1, nb2
                    cmat(i,j)=cmat(i,j)+amat(k,i)*bmat(j,k)
120              continue
110          continue
100      continue
!
    endif
!
end subroutine
