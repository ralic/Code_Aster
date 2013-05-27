subroutine copma2(matr, mat1, mat2)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!
!     FONCTION : COPIE MATR_ASSE VERSION MULT_FRONT DANS MATRICE PLEINE
!
!-----------------------------------------------------------------------
!    IN : MATR  NOM DE LA MATRICE
!    OUT: MAT1/MAT2 VECTEUR CONTENANT LA MATRICE PLEINE
!     ------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       03/06/04 (OB): CREATION POUR FETI.
!-----------------------------------------------------------------------
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mesk.h'
    character(len=19) :: matr
    real(kind=8) :: mat1(*), mat2(*)
!
!
! DECLARATION VARIABLES LOCALES
    integer :: ivale, jrefa, jsmde, neq, ismdi, ismhc, i, j, iaux, iauxo, k, l
    character(len=8) :: kbid
    character(len=14) :: numddl
    real(kind=8) :: pij
!
!-----------------------------------------------------------------------
    data kbid /'        '/
!-----------------------------------------------------------------------
!
    call jemarq()
    call jeveuo(matr//'.REFA', 'L', jrefa)
    numddl=zk24(jrefa-1+2)(1:14)
!
    if (numddl(1:8) .eq. kbid) then
        call u2mesk('F', 'UTILITAI_43', 1, matr)
!
    else
        call jeveuo(numddl(1:14)//'.SMOS.SMDE', 'L', jsmde)
        neq=zi(jsmde-1+1)
        call jeveuo(numddl(1:14)//'.SMOS.SMDI', 'L', ismdi)
        call jeveuo(numddl(1:14)//'.SMOS.SMHC', 'L', ismhc)
!
        call jeveuo(jexnum(matr//'.VALM', 1), 'L', ivale)
!
        iauxo=0
        l=ivale-1
        do 50 j = 1, neq
            iaux=zi(ismdi+j-1)
            do 40 k = iauxo+1, iaux
                l=l+1
                i=zi4(ismhc+k-1)
                pij=zr(l)
                mat1(i+(j-1)*neq) = pij
                mat1(j+(i-1)*neq) = pij
                mat2(i+(j-1)*neq) = pij
                mat2(j+(i-1)*neq) = pij
40          continue
            iauxo=iaux
50      continue
    endif
!
    call jedema()
end subroutine
