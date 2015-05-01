subroutine deflg3(gn, feta, xi, me, t,&
                  tl)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
    implicit none
!     CALCUL DES DEFORMATIONS LOGARITHMIQUES ET DES TERMES NECESSAIRES
!     AU POST TRAITEMENT DES CONTRAINTES ET A LA RIGIDITE TANGENTE
!     SUIVANT ARTICLE MIEHE APEL LAMBRECHT CMAME 2002
! ----------------------------------------------------------------------
!     IN GN    directions propres du tenseur F
!     IN FETA  utilitaires issus de DEFLG2  f_i=-2/lambda_i**2 puis eta
!     IN XI    utilitaires issus de DEFLG2  xi_ij
!     IN ME    utilitaires issus de DEFLG2  tenseur M d'ordre 4
!     IN T     tenseur des contraintesissu de NMCOMP (avec sqrt(2))
!     OUT TL   tenseur d'ordre 4 T:L
! ----------------------------------------------------------------------
#include "asterfort/r8inir.h"
#include "asterfort/tnsvec.h"
    real(kind=8) :: gn(3, 3), t(6), tl(3, 3, 3, 3)
    real(kind=8) :: dzeta(3, 3), t33(3, 3), me(3, 3, 3, 3), xi(3, 3), feta(4)
    integer :: i, j, k, a, b, c, d
! ----------------------------------------------------------------------
!
!     CALCUL DU TERME T.L
!
    call r8inir(81, 0.d0, tl, 1)
    call r8inir(9, 0.d0, dzeta, 1)
    call tnsvec(6, 3, t33, t, 1.d0/sqrt(2.d0))
!
!     A,B sont les composantes, J,I sont les modes propres
    do 40 i = 1, 3
        do 41 j = 1, 3
            do 42 a = 1, 3
                do 43 b = 1, 3
                    dzeta(i,j)=dzeta(i,j)+t33(a,b)*gn(a,i)*gn(b,j)
43              continue
42          continue
41      continue
40  end do
!
    do 44 i = 1, 3
        do 45 a = 1, 3
            do 46 b = 1, 3
                do 47 c = 1, 3
                    do 48 d = 1, 3
                        tl(a,b,c,d)=tl(a,b,c,d)+ 0.25d0*feta(i)*dzeta(&
                        i,i)*me(a,b,i,i)*me(c,d,i,i)
48                  continue
47              continue
46          continue
45      continue
44  end do
!
    do 49 i = 1, 3
        do 50 j = 1, 3
            do 51 k = 1, 3
                do 52 a = 1, 3
                    do 53 b = 1, 3
                        do 54 c = 1, 3
                            do 55 d = 1, 3
                                if ((j.ne.i) .and. (j.ne.k) .and. (k.ne.i)) then
                                    tl(a,b,c,d)=tl(a,b,c,d)+ 2.d0*&
                                    feta(4)*dzeta(i,j)*me(a,b,i,k)*me(&
                                    c,d,j,k)
                                endif
55                          continue
54                      continue
53                  continue
52              continue
51          continue
50      continue
49  end do
!
    do 56 i = 1, 3
        do 57 j = 1, 3
            do 58 a = 1, 3
                do 59 b = 1, 3
                    do 60 c = 1, 3
                        do 61 d = 1, 3
                            if (j .ne. i) then
                                tl(a,b,c,d)=tl(a,b,c,d)+ 2.d0*xi(i,j)*&
                                dzeta(i,j)*me(a,b,i,j)*me(c,d,j,j)
                            endif
61                      continue
60                  continue
59              continue
58          continue
57      continue
56  end do
!
    do 62 i = 1, 3
        do 63 j = 1, 3
            do 64 a = 1, 3
                do 65 b = 1, 3
                    do 66 c = 1, 3
                        do 67 d = 1, 3
                            if (j .ne. i) then
                                tl(a,b,c,d)=tl(a,b,c,d)+ 2.d0*xi(i,j)*&
                                dzeta(i,j)*me(a,b,j,j)*me(c,d,i,j)
                            endif
67                      continue
66                  continue
65              continue
64          continue
63      continue
62  end do
!
    do 68 i = 1, 3
        do 69 j = 1, 3
            do 70 a = 1, 3
                do 71 b = 1, 3
                    do 72 c = 1, 3
                        do 73 d = 1, 3
                            if (j .ne. i) then
                                tl(a,b,c,d)=tl(a,b,c,d)+ 2.d0*xi(i,j)*&
                                dzeta(j,j)*me(a,b,i,j)*me(c,d,i,j)
                            endif
73                      continue
72                  continue
71              continue
70          continue
69      continue
68  end do
!
end subroutine
