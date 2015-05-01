subroutine ordre1(numcle, nomnoe, ddl, coef, coefc,&
                  nbterm)
    implicit none
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
    integer, intent(in) :: nbterm
    integer, intent(inout) :: numcle(nbterm)
    real(kind=8), intent(inout) :: coef(nbterm)
    complex(kind=8), intent(inout) :: coefc(nbterm)
    character(len=8), intent(inout) :: nomnoe(nbterm)
    character(len=8), intent(inout) :: ddl(nbterm)
!
! ------------------------------------------------------------------
!     REARRANGEMENT DES TABLEAUX D'UNE RELATION LINEAIRE PAR ORDRE
!     DE NOEUD CROISSANT OU DE DDL CROISSANT
! ------------------------------------------------------------------
!  NUMCLE(NBTERM) - VAR    - I    - : NUMEROS DES NOEUDS OU DES DDLS
!                 -        -      -   DE LA RELATION
! ------------------------------------------------------------------
!  NOMNOE(NBTERM) - VAR    - K8   - : NOMS DES NOEUDS DE LA
!                 -        -      -   RELATION
! ------------------------------------------------------------------
!  DDL(NBTERM)    - VAR    - K8   - : NOMS DES DDLS DE LA
!                 -        -      -   RELATION
! ------------------------------------------------------------------
!  COEF(NBTERM)   - VAR    - R    - : COEFFICIENTS REELS DES TERMES
!                 -        -      -   DE LA RELATION
! ------------------------------------------------------------------
!  COEFC(NBTERM)  - VAR    - C    - : COEFFICIENTS COMPLEXES DES
!                 -        -      -   TERMES DE LA RELATION
! ------------------------------------------------------------------
!  NBTERM         - IN     - I    - : NOMBRE DE TERMES DE LA
!                 -        -      -   RELATION
! ------------------------------------------------------------------
!
    complex(kind=8) :: coec
    character(len=8) :: nono, nodl
    integer :: i, j, k
    real(kind=8) :: coe
!
! --------- FIN  DECLARATIONS  VARIABLES LOCALES -------------------
!
    do j = 2, nbterm
        k = numcle(j)
        nono = nomnoe(j)
        nodl = ddl(j)
        coe = coef(j)
        coec = coefc(j)
        do i = j-1, 1, -1
            if (numcle(i) .le. k) goto 30
            numcle(i+1) = numcle(i)
            nomnoe(i+1) = nomnoe(i)
            ddl(i+1) = ddl(i)
            coef(i+1) = coef(i)
            coefc(i+1) = coefc(i)
        enddo
        i = 0
30      continue
        numcle(i+1) = k
        nomnoe(i+1) = nono
        ddl(i+1) = nodl
        coef(i+1) = coe
        coefc(i+1) = coec
    end do
end subroutine
