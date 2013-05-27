subroutine te0240(option, nomte)
    implicit none
    include 'jeveux.h'
    include 'asterfort/jevech.h'
    include 'asterfort/matrot.h'
    include 'asterfort/ptktfv.h'
    include 'asterfort/ptktuf.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/utpslg.h'
    character(len=*) :: option, nomte
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     CALCULE LA MATRICE DE RIGIDITE ELEMENTAIRE DES ELEMENTS DE TUYAU
!     DROIT DE TIMOSHENKO
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        'RIGI_MECA '   : CALCUL DE LA MATRICE DE RIGIDITE
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!        'MEFS_POU_D_T' : POUTRE DROITE DE TIMOSHENKO
!                         (SECTION CONSTANTE OU NON)
!
!
!-----------------------------------------------------------------------
    integer :: i, imate, itype, lmat, lorien, lsect, lsect2
    integer :: lx, nbpar, nbres, nc, nno
    real(kind=8) :: rho
!-----------------------------------------------------------------------
    parameter                 (nbres=2)
    real(kind=8) :: valpar, valres(nbres)
    integer :: codres(nbres), kpg, spt
    character(len=8) :: nompar, nomres(nbres), nomail, fami, poum
    character(len=16) :: ch16
    real(kind=8) :: zero, c1, c2, pgl(3, 3), mat(136)
    real(kind=8) :: e, nu, g, celer
    real(kind=8) :: a, ai, xiy, xiz, alfay, alfaz, xjx, xl
    real(kind=8) :: a2, ai2, xiy2, xiz2, alfay2, alfaz2, xjx2, ey, ez
    integer :: iadzi, iazk24
!     ------------------------------------------------------------------
    zero = 0.d0
    c1 = 1.d0
    c2 = 2.d0
!     ------------------------------------------------------------------
!
!     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
    call jevech('PMATERC', 'L', imate)
    nbpar = 0
    nompar = '  '
    valpar = zero
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    do 10 i = 1, nbres
        valres(i) = zero
10  end do
    nomres(1) = 'E'
    nomres(2) = 'NU'
!
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'ELAS', nbpar, nompar, valpar,&
                nbres, nomres, valres, codres, 1)
    e = valres(1)
    nu = valres(2)
    g = e / (c2 *(c1+nu))
!
    valres(1) = zero
    valres(2) = zero
    nomres(1) = 'RHO'
    nomres(2) = 'CELE_R'
!
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'FLUIDE', nbpar, nompar, valpar,&
                nbres, nomres, valres, codres, 1)
    rho = valres(1)
    celer = valres(2)
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
    call jevech('PCAGNPO', 'L', lsect)
    lsect = lsect-1
    itype = nint(zr(lsect+25))
    nno = 2
    nc = 8
!
!     --- SECTION INITIALE ---
    a = zr(lsect+1)
    xiy = zr(lsect+2)
    xiz = zr(lsect+3)
    alfay = zr(lsect+4)
    alfaz = zr(lsect+5)
    xjx = zr(lsect+8)
    ai = zr(lsect+12)
!
!     --- SECTION FINALE ---
    lsect2 = lsect + 12
    a2 = zr(lsect2+1)
    xiy2 = zr(lsect2+2)
    xiz2 = zr(lsect2+3)
    alfay2 = zr(lsect2+4)
    alfaz2 = zr(lsect2+5)
    xjx2 = zr(lsect2+8)
    ai2 = zr(lsect2+12)
    ey = -(zr(lsect+6)+zr(lsect2+6))/c2
    ez = -(zr(lsect+7)+zr(lsect2+7))/c2
!
    if (nomte .ne. 'MEFS_POU_D_T') then
        ch16 = nomte
        call u2mesk('F', 'ELEMENTS2_42', 1, ch16)
    endif
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
    xl = sqrt( (zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2))**2 + (zr(lx+6)-zr(lx+3) )**2 )
    if (xl .eq. zero) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call u2mesk('F', 'ELEMENTS2_43', 1, nomail)
    endif
!
    do 30 i = 1, 136
        mat(i) = 0.d0
30  end do
!
!     --- CALCUL DES MATRICES ELEMENTAIRES ----
    if (option .eq. 'RIGI_MECA') then
!
        if (itype .eq. 0) then
!           --- POUTRE DROITE A SECTION CONSTANTE ---
            call ptktuf(mat, e, rho, celer, a,&
                        ai, xl, xiy, xiz, xjx,&
                        g, alfay, alfaz, ey, ez)
        else if (itype .eq. 1 .or. itype .eq. 2) then
!           --- POUTRE DROITE A SECTION VARIABLE (TYPE 1 OU 2) ---
            call ptktfv(itype, mat, e, rho, celer,&
                        a, ai, a2, ai2, xl,&
                        xiy, xiy2, xiz, xiz2, xjx,&
                        xjx2, g, alfay, alfay2, alfaz,&
                        alfaz2, ey, ez)
        endif
!
!        --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA ---
        call jevech('PCAORIE', 'L', lorien)
!
!        --- PASSAGE DU REPERE LOCAL AU REPER GLOBAL ---
        call matrot(zr(lorien), pgl)
        call jevech('PMATUUR', 'E', lmat)
        call utpslg(nno, nc, pgl, mat, zr(lmat))
!
    else
        ch16 = option
        call u2mesk('F', 'ELEMENTS2_47', 1, ch16)
    endif
!
end subroutine
