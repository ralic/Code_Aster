subroutine te0245(option, nomte)
    implicit none
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/jevech.h'
    include 'asterfort/lonele.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    character(len=*) :: option, nomte
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCULE DES TERMES PROPRES A UNE STRUCTURE  (ELEMENT DE BARRE)
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        'MASS_INER      : CALCUL DES CARACTERISTIQUES DE STRUCTURES
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!        'MECA_BARRE'       : BARRE
!        'MECA_2D_BARRE'    : BARRE 2D
!
!
    integer :: codres
    character(len=16) :: ch16, phenom
    character(len=8) :: nomail
    real(kind=8) :: rho, a, xl, r8b
    integer :: iadzi, iazk24
!     ------------------------------------------------------------------
!
!     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
!-----------------------------------------------------------------------
    integer :: lcastr, lmater, lsect, lx
!-----------------------------------------------------------------------
    call jevech('PMATERC', 'L', lmater)
!
    call rccoma(zi(lmater), 'ELAS', 1, phenom, codres)
!
    if (phenom .eq. 'ELAS' .or. phenom .eq. 'ELAS_ISTR' .or. phenom .eq. 'ELAS_ORTH') then
        call rcvalb('FPG1', 1, 1, '+', zi(lmater),&
                    ' ', phenom, 0, ' ', r8b,&
                    1, 'RHO', rho, codres, 1)
        if (rho .le. r8prem()) then
            call u2mess('F', 'ELEMENTS5_45')
        endif
    else
        call u2mess('F', 'ELEMENTS_50')
    endif
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
    call jevech('PCAGNBA', 'L', lsect)
    a = zr(lsect)
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
!
    if (nomte .eq. 'MECA_BARRE') then
        call lonele(zr(lx), 3, xl)
    else if (nomte.eq.'MECA_2D_BARRE') then
        call lonele(zr(lx), 2, xl)
!
    endif
    if (xl .eq. 0.d0) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call u2mesk('F', 'ELEMENTS2_43', 1, nomail)
    endif
!
!     --- CALCUL DES CARACTERISTIQUES ELEMENTAIRES ----
    if (option .eq. 'MASS_INER') then
        call jevech('PMASSINE', 'E', lcastr)
!
!        --- MASSE ET CDG DE L'ELEMENT ---
        if (nomte .eq. 'MECA_BARRE') then
            zr(lcastr) = rho * a * xl
            zr(lcastr+1) =( zr(lx+4) + zr(lx+1) ) / 2.d0
            zr(lcastr+2) =( zr(lx+5) + zr(lx+2) ) / 2.d0
            zr(lcastr+3) =( zr(lx+6) + zr(lx+3) ) / 2.d0
        else if (nomte.eq.'MECA_2D_BARRE') then
            zr(lcastr) = rho * a * xl
            zr(lcastr+1) =( zr(lx+3) + zr(lx+1) ) / 2.d0
            zr(lcastr+2) =( zr(lx+4) + zr(lx+2) ) / 2.d0
        endif
!
!        --- INERTIE DE L'ELEMENT ---
        zr(lcastr+4) = 0.d0
        zr(lcastr+5) = 0.d0
        zr(lcastr+6) = 0.d0
        zr(lcastr+7) = 0.d0
        zr(lcastr+8) = 0.d0
        zr(lcastr+9) = 0.d0
!
    else
        ch16 = option
        call u2mesk('F', 'ELEMENTS2_47', 1, ch16)
    endif
!
end subroutine
