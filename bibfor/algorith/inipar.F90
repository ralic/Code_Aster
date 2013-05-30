subroutine inipar(np1, nbm, nbnl, testc, cmod0,&
                  cmodca, kmod0, kmodca, amor, amor0,&
                  puls, puls0, acc, vit, dep,&
                  acc0, vit0, dep0, accg, vitg,&
                  depg, accg0, vitg0, depg0, tconf1,&
                  ftest0, tconf2, ftest)
    implicit none
!-----------------------------------------------------------------------
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
! DESCRIPTION : INITIALISATION DES PARAMETRES POUR PAS DE TEMPS SUIVANT
! -----------
!               APPELANT : MDITM2
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: np1, nbm, nbnl, testc
    real(kind=8) :: cmod0(np1, *), cmodca(np1, *), kmod0(np1, *), kmodca(np1, *)
    real(kind=8) :: amor(*), amor0(*), puls(*), puls0(*), acc(3, *), vit(3, *)
    real(kind=8) :: dep(3, *), acc0(3, *), vit0(3, *), dep0(3, *), accg(*)
    real(kind=8) :: vitg(*), depg(*), accg0(*), vitg0(*), depg0(*), tconf1(4, *)
    real(kind=8) :: ftest0, tconf2(4, *), ftest
!
! VARIABLES LOCALES
! -----------------
    integer :: i, ic, j
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!-----------------------------------------------------------------------
! 1.  REINITIALISATION DES VECTEURS DES DEGRES DE LIBERTE GENERALISES
!-----------------------------------------------------------------------
!
! 1.1 SI LE SYSTEME EST EN VOL A L'INSTANT N+1
!
    if (testc .eq. 0) then
!
        do 10 i = 1, nbm
            depg0(i) = depg(i)
10      continue
        do 20 i = 1, nbm
            vitg0(i) = vitg(i)
20      continue
        do 30 i = 1, nbm
            accg0(i) = accg(i)
30      continue
!
        do 70 i = 1, nbm
            amor0(i) = amor(i)
70      continue
        do 80 i = 1, nbm
            puls0(i) = puls(i)
80      continue
!
! 1.2 SINON (LE SYSTEME EST EN CHOC A L'INSTANT N+1)
!
    else
!
        do 110 i = 1, nbm
            depg0(i) = depg(i)
110      continue
        do 120 i = 1, nbm
            vitg0(i) = vitg(i)
120      continue
        do 130 i = 1, nbm
            accg0(i) = accg(i)
130      continue
!
        do 170 i = 1, nbm
            amor0(i) = amor(i)
170      continue
        do 180 i = 1, nbm
            puls0(i) = puls(i)
180      continue
!
    endif
!
!-----------------------------------------------------------------------
! 2.  REINITIALISATION DES MATRICES DE RAIDEURS ET D'AMORTISSEMENTS
!     GENERALISES DU SYSTEME
!-----------------------------------------------------------------------
!
    do 200 j = 1, nbm
        do 201 i = 1, nbm
            kmod0(i,j) = kmodca(i,j)
201      continue
200  end do
!
    do 210 j = 1, nbm
        do 211 i = 1, nbm
            cmod0(i,j) = cmodca(i,j)
211      continue
210  end do
!
!-----------------------------------------------------------------------
! 3.  REINITIALISATION DES VECTEURS ACCELERATIONS, VITESSES ET
!     DEPLACEMENTS PHYSIQUES AUX NOEUDS DE CHOC
!-----------------------------------------------------------------------
!
    do 300 ic = 1, nbnl
        do 301 i = 1, 3
            acc0(i,ic) = acc(i,ic)
301      continue
300  end do
!
    do 310 ic = 1, nbnl
        do 311 i = 1, 3
            vit0(i,ic) = vit(i,ic)
311      continue
310  end do
!
    do 320 ic = 1, nbnl
        do 321 i = 1, 3
            dep0(i,ic) = dep(i,ic)
321      continue
320  end do
!
    do 330 ic = 1, nbnl
        do 331 i = 1, 4
            tconf1(i,ic) = tconf2(i,ic)
331      continue
330  end do
    ftest0 = ftest
!
! --- FIN DE INIPAR.
end subroutine
