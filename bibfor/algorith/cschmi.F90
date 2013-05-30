subroutine cschmi(ca, ndim, cvec, cbas, ndimax,&
                  nbbas)
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
    implicit none
!
!***********************************************************************
!    P. RICHARD                                 DATE 31/07/91
!-----------------------------------------------------------------------
!  BUT: < COMPLEXE ORTHOGONALISATION DE SCHMIDT >
!
! ORTHOGONALISER UN VECTEUR COMPLEXE A DES VECTEURS DE REFERENCE
!  PAR RAPPORT A UNE MATRICE COMPLEXES HERMITIENNE
!
!-----------------------------------------------------------------------
!
! CA       /I/: MATRICE CARRE COMPLEXE SERVANT DE NORME
! NDIM     /I/: DIMENSION DE LA MATRICE
! CVEC     /M/: VECTEUR A ORTHOGONALISER
! CBAS     /I/: MATRICE DES VECTEURS DE REFERENCE
! NBBAS    /I/: NOMBRE DES VECTEURS DE REFERENCE
!
!-----------------------------------------------------------------------
!
    include 'asterfort/cvnorm.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/zconju.h'
    complex(kind=8) :: ca(*)
    complex(kind=8) :: cbas(ndimax, nbbas)
    complex(kind=8) :: cvec(ndim), ctrav1, ctrav2, cprod, cconj, cmodu
    real(kind=8) :: prea, pima
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
! CAS OU IL N'Y A PAS DE VECTEUR DE REFERENCE
!
!-----------------------------------------------------------------------
    integer :: i, ic, icdiag, il, ildiag, iretou, j
    integer :: nbbas, ndim, ndimax
!-----------------------------------------------------------------------
    if (nbbas .eq. 0) then
!
!   NORMALISATION
!
        call cvnorm(ca, cvec, ndim, iretou)
        if (iretou .eq. 1) call u2mess('F', 'ALGORITH2_22')
!
        goto 9999
    endif
!
!       BOUCLE SUR LES VECTEURS DE REFERENCE
!
    do 10 j = 1, nbbas
!
!   MULTIPLICATION ET CALCUL PRODUIT ET NORMES
!
        cprod=dcmplx(0.d0,0.d0)
        cmodu=dcmplx(0.d0,0.d0)
        do 20 il = 1, ndim
            ctrav1=dcmplx(0.d0,0.d0)
            ctrav2=dcmplx(0.d0,0.d0)
            ildiag = il*(il-1)/2+1
            do 30 ic = 1, ndim
                icdiag=ic*(ic-1)/2+1
                if (ic .ge. il) then
                    ctrav1=ctrav1+(ca(icdiag+ic-il)*cvec(ic))
                    ctrav2=ctrav2+(ca(icdiag+ic-il)*cbas(ic,j))
                else
                    ctrav1=ctrav1+(dconjg(ca(ildiag+il-ic))*cvec(ic))
                    ctrav2=ctrav2+(dconjg(ca(ildiag+il-ic))*cbas(ic,j)&
                    )
                endif
30          continue
            call zconju(cbas(il, j), prea, pima)
            cconj=dcmplx(prea,-pima)
            cprod=cprod+(ctrav1*cconj)
            cmodu=cmodu+(ctrav2*cconj)
20      continue
!
!
!
!   ORTHOGONALISATION DE SCHMIT (LE POTE DE GRAM)
!
        do 40 i = 1, ndim
            cvec(i)=cvec(i)-(cprod*cbas(i,j)/cmodu)
40      continue
!
10  end do
!
    call cvnorm(ca, cvec, ndim, iretou)
    if (iretou .eq. 1) call u2mess('F', 'ALGORITH2_22')
!
!
!
9999  continue
end subroutine
