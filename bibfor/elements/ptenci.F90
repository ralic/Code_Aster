subroutine ptenci(neq, x, mat, omeg, en, itype, kanl, idis)
!
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
!
! --------------------------------------------------------------------------------------------------
!
!     CALCUL ENERGIE CINETIQUE POUR
!         - ELEMENT DE POUTRE (POU_D_T, POU_D_E,)
!         - ELEMENT DISCRET
!         - ELEMENT BARRE
!
! --------------------------------------------------------------------------------------------------
!
! IN  : NEQ    : DIMENSION DE LA MATRICE MAT
! IN  : X      : VECTEUR DE DEPLACEMENT
! IN  : MAT    : MATRICE DE MASSE
! IN  : OMEG   : PULSATION AU CARREE
! OUT : EN     : ENERGIE CINETIQUE
! IN  : ITYPE  : TYPE DE LA SECTION
! IN  : KANL   : TYPE DE LA MATRICE DE MASSE
! IN  : IDIS   : = 0 , PAS DE CALCUL DE LA REPARTITON DE L'ENERGIE
!                = 1 , CALCUL DE LA REPARTITON DE L'ENERGIE
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
    integer :: neq, itype, kanl, idis
    real(kind=8) :: x(*), mat(neq, neq), omeg, en(*)
!
#include "asterf_types.h"
#include "asterfort/vtmv.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jcft(8), ncft(3), icft(6, 3), na(4), ia(4, 4)
    real(kind=8) :: x2(12), mat2(144)
    aster_logical :: ltest
    integer :: i, iform, j, kk, l, nddl, nn
    real(kind=8) :: const, r, zero
!
! --------------------------------------------------------------------------------------------------
!
    data jcft/  2 ,  3 ,  5 ,  6 ,  8 ,  9 , 11 , 12 /
    data ncft/  2 ,  6 ,  6 /
    data icft/  1 ,  7 ,  0 ,  0 ,  0 ,  0 ,&
                2 ,  4 ,  6 ,  8 , 10 , 12 ,&
                3 ,  4 ,  5 ,  9 , 10 , 11 /
!
!   ELEMENT DROIT CLASSIQUE
    data na/  2 ,  2 ,  4 ,  4  /
    data ia/  1 ,  7 ,  0 ,  0 ,&
              4 , 10 ,  0 ,  0 ,&
              2 ,  6 ,  8 , 12 ,&
              3 ,  5 ,  9 , 11  /
!
! --------------------------------------------------------------------------------------------------
!
    const = omeg / 2.d0
    zero = 0.d0
    ltest = .false.
!
!   ENERGIE CINETIQUE GLOBALE
    call vtmv(neq, x, mat, r)
    en(1) = r * const
    if (idis.eq.0) goto 910
    if (abs(en(1)) .lt. 1.d-06) goto 910
    iform = 0
!
!                    -----------------------------
!                    --- REPARTITION D'ENERGIE ---
!                    -----------------------------
!
    nn=0
    if (kanl.eq.0) then
!
!        NEQ  : NOMBRE D'EQUATION DE LA MATRICE ( 12, 6, 3 )
!        NDDL : NOMBRE DE DDL ( 6, 3 )
!
!        ITYPE : 20 , MAILLE POI1 DE TRANSLATION
!        ITYPE : 21 , MAILLE POI1 DE TRANSLATION ET ROTATION
!        ITYPE : 40 , MAILLE SEG2 DE TRANSLATION
!        ITYPE : 41 , MAILLE SEG2 DE TRANSLATION ET ROTATION
!        ITYPE : 0, 1, 2, ELEMENT DE POUTRE
!
        nddl = neq / 2
        if (itype.eq.20 .or. itype.eq.21 .or. itype.eq.22 .or. itype.eq.23) nddl = neq
!
        nn = 1 + nddl
!       ON N'A QUE LA DIAGONALE ( PAS DE TERMES D'INERTIE )
        do i = 1, neq-1
            do j = i+1, neq
                if (mat(i,j) .ne. zero) goto 500
            enddo
        enddo
!
        if (itype.eq.20 .or. itype.eq.21 .or. itype.eq.22 .or. itype.eq.23) then
            do i = 1, nddl
                en(i+1) = (x(i)*mat(i,i)*x(i))*const
            enddo
        else
            do i = 1, nddl
                en(i+1) = (x(i+nddl) * mat(i+nddl,i+nddl) * x(i+nddl) + x(i) * mat(i,i) * x(i)&
                          ) * const
            enddo
        endif
        iform = 10
        if (itype.eq.40 .or. itype.eq.20) iform = 11
        if (itype.eq.42 .or. itype.eq.22) iform = 11
!
        goto 900
!
!       ON TIENT COMPTE DES TERMES D'INERTIE
500     continue
!
        if (nddl .ge. 6) then
            do i = 1, 3
                do j = i+1, 6
                    if (mat(i,j) .ne. zero) goto 600
                enddo
            enddo
        else
            goto 600
        endif
!       MASSE CONCENTREE + INERTIES
        if (itype.eq.21 .or. itype.eq.23) then
            do i = 1, nddl
                en(i+1) = (x(i)*mat(i,i)*x(i))*const
            enddo
        else
            do i = 1, nddl
                en(i+1) = (x(i+nddl)*mat(i+nddl,i+nddl)*x(i+nddl) + x(i)*mat(i,i)*x(i))*const
            enddo
        endif
        if (nddl.eq.6) then
            do i = 1, 3
                x2(i) = x(i+3)
                do j = 1, 3
                    mat2(3*(j-1)+i) = mat(i+3,j+3)
                enddo
            enddo
        else
            do i = 1, 3
                x2(i) = x(i+3)
                x2(i+3) = x(i+9)
                do j = 1, 3
                    mat2(6*(j-1)+i ) = mat(i+3,j+3)
                    mat2(6*(j+2)+i+3) = mat(i+9,j+9)
                enddo
            enddo
        endif
        call vtmv(nddl, x2, mat2, r)
        en(5) = r * const
        iform = 20
        nn = 5
        goto 900
!
600     continue
!
    else if (itype.eq.0 .or. itype.eq.1 .or. itype.eq.2) then
!       ELEMENT DROIT DE SECTION CONSTANTE OU VARIABLE
        do kk = 1, 8
            if (mat( 4,jcft(kk)) .ne. zero .or. mat(10,jcft(kk)) .ne. zero) then
!               COUPLAGE FLEXION-TORSION
                do l = 1, 3
                    do i = 1, ncft(l)
                        x2(i) = x(icft(i,l))
                        do j = 1, ncft(l)
                            mat2(ncft(l)*(j-1)+i) = mat(icft(i,l), icft(j,l) )
                        enddo
                    enddo
                    call vtmv(ncft(l), x2, mat2, r)
                    en(1+l) = r * const
                enddo
                iform = 101
                nn = 4
                goto 900
            endif
        enddo
!
!       --- ELEMENT DROIT CLASSIQUE ---
        iform = 101
        nn = 5
        do l = 1, 4
            do i = 1, na(l)
                x2(i) = x(ia(i,l))
                do j = 1, na(l)
                    mat2(na(l)*(j-1)+i) = mat ( ia(i,l) , ia(j,l) )
                enddo
            enddo
            call vtmv(na(l), x2, mat2, r)
            en(1+l) = r * const
        enddo
    endif
900 continue
!
!   POURCENTAGE
    do i = 2, nn
        en(i) = en(i)/en(1)
    enddo
!
910 continue
    if (ltest) then
        write(6,*)'--->> PTENCI     ITYPE = ',itype
        write(6,*)'                  KANL = ',kanl
        write(6,*)'                  OMEG = ',omeg
        write(6,*)'---------ENERGIE CINETIQUE--------'
        write(6,*)'ENERGIE CINETIQUE GLOBALE ',en(1)
        if (iform.eq.10) then
            write(6,*)'MASSE CONCENTREE STRICTEMENT DIAGONALE'
            write(6,*)'TRANSLATION X  ',en(2)
            write(6,*)'TRANSLATION Y  ',en(3)
            write(6,*)'TRANSLATION Z  ',en(4)
            write(6,*)'ROTATION /X    ',en(5)
            write(6,*)'ROTATION /Y    ',en(6)
            write(6,*)'ROTATION /Z    ',en(7)
        else if (iform.eq.11) then
            write(6,*)'MASSE CONCENTREE STRICTEMENT DIAGONALE'
            write(6,*)'TRANSLATION X  ',en(2)
            write(6,*)'TRANSLATION Y  ',en(3)
            write(6,*)'TRANSLATION Z  ',en(4)
        else if (iform.eq. 20) then
            write(6,*)'MASSE DIAGONALE + INERTIE '
            write(6,*)'TRANSLATION X  ',en(2)
            write(6,*)'TRANSLATION Y  ',en(3)
            write(6,*)'TRANSLATION Z  ',en(4)
            write(6,*)'ROTATION       ',en(5)
        else if (iform.eq.100) then
            write(6,*)'ELEMENT DROIT "ORDINAIRE"'
            write(6,*)'DE TRACTION-COMPRESSION ',en(2)
            write(6,*)'DE TORSION              ',en(3)
            write(6,*)'DE FLEXION Y            ',en(4)
            write(6,*)'DE FLEXION Z            ',en(5)
        else if (iform.eq.101) then
            write(6,*)'ELEMENT DROIT AVEC COUPLAGE FLEXION-TORSION'
            write(6,*)'DE TRACTION-COMPRESSION ',en(2)
            write(6,*)'DE FLEXION-TORSION Y    ',en(3)
            write(6,*)'DE FLEXION-TORSION Z    ',en(4)
        else if (iform.eq.110) then
            write(6,*)'ELEMENT COURBE'
            write(6,*)'DE FLEXION DANS LE PLAN ',en(2)
            write(6,*)'DE FLEXION HORS DU PLAN ',en(3)
        endif
    endif
!
end subroutine
