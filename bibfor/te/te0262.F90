subroutine te0262(option, nomte)
    implicit none
!     ------------------------------------------------------------------
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
! TOLE CRS_512
! ======================================================================
    include 'jeveux.h'
!
    include 'asterfort/jevech.h'
    include 'asterfort/matrot.h'
    include 'asterfort/porigy.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/utpplg.h'
    character(len=16) :: option, nomte
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!       'MECA_GYRO': CALCUL DE LA MATRICE DE RAIDEUR GYROSCOPIQUE
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!       'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!       'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!       'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
!       'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!                         MULTI-FIBRES (SECTION CONSTANTE)
!
!
    integer :: nbres, nl
    parameter (nbres=3,nl=144)
    real(kind=8) :: valres(nbres)
    integer :: codres(nbres)
    character(len=8) :: nompar, nomres(nbres), fami, poum
    real(kind=8) :: pgl(3, 3), klv(nl)
    real(kind=8) :: e, rho
    real(kind=8) :: valpar, xnu, zero
    integer :: imate, lmat, lorien, lsect
    integer :: nbpar, nno, kpg, spt
!     ------------------------------------------------------------------
    data nomres/'E','RHO','NU'/
!     ------------------------------------------------------------------
    zero = 0.d0
!     ------------------------------------------------------------------
!
!     --- CARACTERISTIQUES DES ELEMENTS
!
    if (nomte .eq. 'MECA_POU_D_E' .or. nomte .eq. 'MECA_POU_D_T' .or. nomte .eq.&
        'MECA_POU_D_EM') then
        nno = 2
    else
        call u2mesk('F', 'ELEMENTS2_42', 1, nomte)
    endif
!
!     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
!
    call jevech('PMATERC', 'L', imate)
!
    nbpar = 0
    nompar = ' '
    valpar = zero
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'ELAS', nbpar, nompar, valpar,&
                nbres, nomres, valres, codres, 1)
    e = valres(1)
    rho = valres(2)
    xnu = valres(3)
!
    call jevech('PCAGNPO', 'L', lsect)
!
    call jevech('PMATUNS', 'E', lmat)
!
!     --- RECUPERATION DES ORIENTATIONS ---
!
    call jevech('PCAORIE', 'L', lorien)
!
!     --- CALCUL DE LA MATRICE GYROSCOPIQUE LOCALE ---
    call porigy(nomte, e, rho, xnu, zi(imate),&
                klv, nl)
!
    call matrot(zr(lorien), pgl)
!  CHANGEMENT DE BASE : LOCAL -> GLOBAL
    call utpplg(nno, 6, pgl, klv, zr(lmat))
!
end subroutine
