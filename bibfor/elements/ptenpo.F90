subroutine ptenpo(n, x, mat, ep, itype,&
                  iform)
    implicit none
#include "asterfort/vtmv.h"
    integer :: n, itype, iform
    real(kind=8) :: x(*), mat(n, n), ep(*)
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
!     CALCUL ENERGIE DE DEFORMATION POUR
!         - ELEMENT DE POUTRE (POU_D_T, POU_D_E, POU_C_T)
!         - ELEMENT DISCRET
!         - ELEMENT BARRE
!     ------------------------------------------------------------------
! IN  N      :  DIMENSION DE LA MATRICE MAT
! IN  X      :  VECTEUR DE DEPLACEMENT
! IN  MAT    :  MATRICE DE RAIDEUR
! OUT EP     :  ENERGIE DE DEFORMATION
! IN  ITYPE  :  TYPE DE LA SECTION
! IN  IFORM  :
!     ------------------------------------------------------------------
    integer :: jcft(8), icou(6, 2), ncft(3), icft(6, 3), na(4), ia(4, 4)
    real(kind=8) :: x2(12), mat2(144)
!-DEL LOGICAL  LTEST
!
!             COUPLAGE FLEXION-TORSION
!-----------------------------------------------------------------------
    integer :: i, j, kk, l, nn
    real(kind=8) :: deux, r, zero
!-----------------------------------------------------------------------
    data jcft/  2 ,  3 ,  5 ,  6 ,  8 ,  9 , 11 , 12 /
    data ncft/  2 ,  6 ,  6 /
    data icft/  1 ,  7 ,  0 ,  0 ,  0 ,  0 ,&
     &            2 ,  4 ,  6 ,  8 , 10 , 12 ,&
     &            3 ,  4 ,  5 ,  9 , 10 , 11 /
!
!             ELEMENT COURBE
    data icou/  1 ,  2 ,  6 ,  7 ,  8 , 12 ,&
     &            3 ,  4 ,  5 ,  9 , 10 , 11 /
!
!            ELEMENT DROIT CLASSIQUE
    data na/  2 ,  2 ,  4 ,  4  /
    data ia/  1 ,  7 ,  0 ,  0 ,&
     &          4 , 10 ,  0 ,  0 ,&
     &          2 ,  6 ,  8 , 12 ,&
     &          3 ,  5 ,  9 , 11  /
!
!-DEL LTEST = .FALSE.
    zero = 0.d0
    deux = 2.d0
!
!     --- ENERGIE POTENTIELLE GLOBALE ---
    call vtmv(n, x, mat, r)
    ep(1) = r / deux
    if (iform .eq. 0) goto 900
    if (abs(ep(1)) .lt. 1.d-06) goto 900
!-DEL IFORM = 0
!
!                    -----------------------------
!                    --- REPARTITION D'ENERGIE ---
!                    -----------------------------
    if (itype .eq. 0 .or. itype .eq. 1 .or. itype .eq. 2) then
!       --- ELEMENT DROIT DE SECTION CONSTANTE OU VARIABLE ---
        nn = 4
        do 10 kk = 1, 8
            if (mat( 4,jcft(kk)) .ne. zero .or. mat(10,jcft(kk)) .ne. zero) then
!             --- COUPLAGE FLEXION-TORSION ---
                do 5 l = 1, 3
                    do 4 i = 1, ncft(l)
                        x2(i) = x(icft(i,l))
                        do 3 j = 1, ncft(l)
                            mat2(ncft(l)*(j-1)+i) = mat(icft(i,l), icft(j,l) )
 3                      continue
 4                  continue
                    call vtmv(ncft(l), x2, mat2, r)
                    ep(1+l) = r / deux
 5              continue
                iform= 101
                goto 900
            endif
10      continue
!       --- ELEMENT DROIT CLASSIQUE ---
!-DEL    IFORM = 100
        do 15 l = 1, 4
            do 14 i = 1, na(l)
                x2(i) = x(ia(i,l))
                do 13 j = 1, na(l)
                    mat2(na(l)*(j-1)+i) = mat ( ia(i,l) , ia(j,l) )
13              continue
14          continue
            call vtmv(na(l), x2, mat2, r)
            ep(1+l) = r / deux
15      continue
!
    else if (itype .eq. 10) then
!       --- ELEMENT COURBE DE SECTION CONSTANTE ---
!-DEL    IFORM = 110
        nn = 2
        do 25 l = 1, 2
            do 24 i = 1, 6
                x2(i) = x(icou(i,l))
                do 23 j = 1, 6
                    mat2(6*(j-1)+i) = mat ( icou(i,l) , icou(j,l) )
23              continue
24          continue
            call vtmv(6, x2, mat2, r)
            ep(1+l) = r / deux
25      continue
!
    else if (itype .eq. 20 .or. itype.eq.21) then
!        --- ELEMENT DISCRET TYPE NODALE ..._N ---
!-DEL    IFORM = 10
        nn = n
        do 202 i = 2, n
            do 201 j = 1, i-1
                if (mat(i,j) .ne. zero) goto 900
201          continue
202      continue
        do 203 i = 1, n
            ep(1+i) = x(i) * mat(i,i) * x(i) / deux
203      continue
!
    else if (itype .eq. 22 .or. itype.eq.23) then
!        --- ELEMENT DISCRET TYPE NODALE ..._N_NS ---
        nn = n
        do 205 i = 1, n
            ep(1+i) = x(i) * mat(i,i) * x(i) / deux
205      continue
!
    else if (itype .eq. 40 .or. itype.eq.41) then
!        --- ELEMENT DISCRET TYPE LIAISON ..._L ---
!-DEL    IFORM = 10
        nn = n / 2
        do 402 i = 2, nn
            do 401 j = 1, i-1
                if (mat(i,j) .ne. zero .or. mat(i,j+nn) .ne. zero .or. mat(i+nn,j+nn) .ne. zero) &
                goto 900
401          continue
402      continue
        do 403 i = 1, nn
            ep(1+i) = (&
                      x(i) * mat(i,i) * x(i) + 2 * x(i) * mat(i,i+ nn) * x(i+nn) + x(i+nn) * mat(&
                      &i+nn,i+nn) * x(i+nn)&
                      ) / deux
403      continue
!
    else if (itype .eq. 42 .or. itype.eq.43) then
!        --- ELEMENT DISCRET TYPE LIAISON ..._L_NS ---
        nn = n / 2
        do 405 i = 1, nn
            ep(1+i) = (&
                      x(i) * mat(i,i) * x(i) + x(i) * mat(i,i+nn) * x(i+nn) + x(i+nn) * mat(i+nn,&
                      &i) * x(i) + x(i+nn) * mat(i+ nn,i+nn) * x(i+nn)&
                      ) / deux
405      continue
    endif
!
!     --- POURCENTAGE ----
    do 410 i = 2, nn+1
        ep(i) = ep(i)/ep(1)
410  end do
!
900  continue
!-DEL IF ( LTEST ) THEN
!-DEL    WRITE(6,*)'--------ENERGIE POTENTIELLE--------'
!-DEL    WRITE(6,*)'ENERGIE POTENTIELLE GLOBALE :',EP(1)
!-DEL    IF ( IFORM.EQ.100 ) THEN
!-DEL       WRITE(6,*)'ELEMENT DROIT "ORDINAIRE"'
!-DEL       WRITE(6,*)'FRACTION EN TRACTION-COMPRESSION :',EP(2)
!-DEL       WRITE(6,*)'FRACTION EN TORSION              :',EP(3)
!-DEL       WRITE(6,*)'FRACTION EN FLEXION Y            :',EP(4)
!-DEL       WRITE(6,*)'FRACTION EN FLEXION Z            :',EP(5)
!-DEL    ELSEIF ( IFORM.EQ.101 ) THEN
!-DEL       WRITE(6,*)'ELEMENT DROIT AVEC COUPLAGE FLEXION-TORSION'
!-DEL       WRITE(6,*)'FRACTION EN TRACTION-COMPRESSION :',EP(2)
!-DEL       WRITE(6,*)'FRACTION EN FLEXION-TORSION Y    :',EP(3)
!-DEL       WRITE(6,*)'FRACTION EN FLEXION-TORSION Z    :',EP(4)
!-DEL    ELSEIF ( IFORM.EQ.110 ) THEN
!-DEL       WRITE(6,*)'ELEMENT COURBE'
!-DEL       WRITE(6,*)'FRACTION EN FLEXION DANS LE PLAN :',EP(2)
!-DEL       WRITE(6,*)'FRACTION EN FLEXION HORS DU PLAN :',EP(3)
!-DEL    ELSE IF ( IFORM .EQ. 10 ) THEN
!-DEL       DO 905 K = 1 , NN
!-DEL          WRITE(6,*)'FRACTION SUR LE D.D.L. ',K,' :',EP(1+K)
!-DEL 905   CONTINUE
!-DEL    ENDIF
!-DEL ENDIF
!
end subroutine
