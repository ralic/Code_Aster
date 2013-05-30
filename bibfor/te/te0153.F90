subroutine te0153(option, nomte)
    implicit none
    include 'jeveux.h'
    include 'asterfort/jevech.h'
    include 'asterfort/lonele.h'
    include 'asterfort/matrot.h'
    include 'asterfort/pmavec.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/utpslg.h'
    include 'asterfort/vecma.h'
    character(len=*) :: option, nomte
! ----------------------------------------------------------------------
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
!     CALCULE LES MATRICES ELEMENTAIRES DES ELEMENTS DE BARRE
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        'RIGI_MECA'      : CALCUL DE LA MATRICE DE RAIDEUR
!        'MASS_MECA'      : CALCUL DE LA MATRICE DE MASSE
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!        'MECA_BARRE' : ELEMENT BARRE
!        'MECA_2D_BARRE' : ELEMENT BARRE
!
!
    integer :: codres
    character(len=8) :: nomail
    character(len=16) :: ch16
    real(kind=8) :: e, rho, pgl(3, 3), mat(21), matr(21)
    real(kind=8) :: a, xl, xrig, xmas, matp(6, 6), mat2dm(4, 4), mat2dv(10)
    integer :: iadzi, iazk24
!     ------------------------------------------------------------------
!
!C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
!-----------------------------------------------------------------------
    integer :: i, iacce, imate, j, lmat, lorien, lsect
    integer :: lvec, lx, nc, nno
    real(kind=8) :: r8b
!-----------------------------------------------------------------------
    call jevech('PCAGNBA', 'L', lsect)
    a = zr(lsect)
    nno = 2
    nc = 3
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
    if (nomte .eq. 'MECA_BARRE') then
        call lonele(zr(lx), 3, xl)
!
    else if (nomte.eq.'MECA_2D_BARRE') then
        call lonele(zr(lx), 2, xl)
!
    endif
!
    if (xl .eq. 0.d0) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call u2mesk('F', 'ELEMENTS2_43', 1, nomail)
    endif
!
!     --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA ---
    call jevech('PCAORIE', 'L', lorien)
!
    if (option .eq. 'M_GAMMA') then
        call jevech('PVECTUR', 'E', lvec)
        call jevech('PACCELR', 'L', iacce)
    else
        call jevech('PMATUUR', 'E', lmat)
    endif
!
    do 20 i = 1, 21
        mat(i) = 0.d0
20  end do
!
!     --- CALCUL DES MATRICES ELEMENTAIRES ----
    call jevech('PMATERC', 'L', imate)
    if (option .eq. 'RIGI_MECA') then
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', 'ELAS', 0, ' ', r8b,&
                    1, 'E', e, codres, 1)
        xrig = e * a / xl
        mat( 1) = xrig
        mat( 7) = -xrig
        mat(10) = xrig
!
    else if (option.eq.'MASS_MECA' .or. option.eq.'M_GAMMA') then
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', 'ELAS', 0, ' ', r8b,&
                    1, 'RHO', rho, codres, 1)
        do 40 i = 1, 21
            matr(i) = 0.d0
40      continue
!
        xmas = rho * a * xl / 6.d0
        mat( 1) = xmas * 2.d0
        mat( 3) = xmas * 2.d0
        mat( 6) = xmas * 2.d0
        mat( 10) = xmas * 2.d0
        mat( 15) = xmas * 2.d0
        mat( 21) = xmas * 2.d0
!
        mat( 7) = xmas
        mat( 12) = xmas
        mat( 18) = xmas
!
        else if ( (option.eq.'MASS_MECA_DIAG') .or. (&
    option.eq.'MASS_MECA_EXPLI')) then
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', 'ELAS', 0, ' ', r8b,&
                    1, 'RHO', rho, codres, 1)
        xmas = rho * a * xl / 2.d0
        mat( 1) = xmas
        mat( 3) = xmas
        mat( 6) = xmas
        mat(10) = xmas
        mat(15) = xmas
        mat(21) = xmas
!
    else
        ch16 = option
        call u2mesk('F', 'ELEMENTS2_47', 1, ch16)
    endif
!
!     --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
!
    call matrot(zr(lorien), pgl)
    call utpslg(nno, nc, pgl, mat, matr)
!
    if (option .eq. 'M_GAMMA') then
        if (nomte .eq. 'MECA_BARRE') then
            do 45 i = 1, 6
                do 46 j = 1, 6
                    matp(i,j)=0.d+0
46              continue
45          continue
            call vecma(matr, 21, matp, 6)
            call pmavec('ZERO', 6, matp, zr(iacce), zr(lvec))
        else
            mat2dv(1) = matr(1)
            mat2dv(2) = matr(2)
            mat2dv(3) = matr(3)
            mat2dv(4) = matr(7)
            mat2dv(5) = matr(8)
            mat2dv(6) = matr(10)
            mat2dv(7) = matr(11)
            mat2dv(8) = matr(12)
            mat2dv(9) = matr(14)
            mat2dv(10) = matr(15)
            do 47 i = 1, 4
                do 48 j = 1, 4
                    mat2dm(i,j)=0.d+0
48              continue
47          continue
            call vecma(mat2dv, 10, mat2dm, 4)
            call pmavec('ZERO', 4, mat2dm, zr(iacce), zr(lvec))
        endif
    else
!
! ECRITURE DANS LE VECTEUR PMATTUR SUIVANT L'ELEMENT
!
        if (nomte .eq. 'MECA_BARRE') then
            do 30 i = 1, 21
                zr(lmat+i-1) = matr(i)
30          continue
        else if (nomte.eq.'MECA_2D_BARRE') then
            zr(lmat) = matr(1)
            zr(lmat+1) = matr(2)
            zr(lmat+2) = matr(3)
            zr(lmat+3) = matr(7)
            zr(lmat+4) = matr(8)
            zr(lmat+5) = matr(10)
            zr(lmat+6) = matr(11)
            zr(lmat+7) = matr(12)
            zr(lmat+8) = matr(14)
            zr(lmat+9) = matr(15)
        endif
    endif
!
!
!
!
!
end subroutine
