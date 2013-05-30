subroutine te0235(option, nomte)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jevech.h'
    include 'asterfort/masstg.h'
    include 'asterfort/matrot.h'
    include 'asterfort/pmfitg.h'
    include 'asterfort/pmfitx.h'
    include 'asterfort/poriro.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/utpslg.h'
    character(len=*) :: option, nomte
!     ------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
!     CALCULE LA MATRICE DE RAIDEUR CENTRIFUGE DES ELEMENTS DE POUTRE
!     AVEC GAUCHISSEMENT (MULTIFIBRE ON NON)
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!       'RIGI_MECA_RO'      : CALCUL DE LA MATRICE DE RAIDEUR CENTRIFUGE
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!       'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!                         MULTI-FIBRES SECTION CONSTANTE
!     ------------------------------------------------------------------
!
    integer :: nbres
    parameter (nbres=6)
    real(kind=8) :: valres(nbres), r8b
    integer :: codres(nbres)
    character(len=8) :: nomres(nbres), nomail, fami, poum
    character(len=24) :: mator
    integer :: i, lmater, j, iret
    integer :: lorien, lmat
    integer :: nno, nc, kpg, spt, nbgfmx
    integer :: itype, lsect, lx, iadzi, iazk24, irota
    integer :: inbfib, nbfib, jacf, icompo, isicom, npg, isdcom
    real(kind=8) :: omega(3), omegl(3), s
    real(kind=8) :: xl
    real(kind=8) :: zero, un, deux
    real(kind=8) :: e, g, xnu, rho
    real(kind=8) :: a, xiy, xiz, alfay, alfaz
    real(kind=8) :: pgl(3, 3), mlv(105), matp1(105)
    real(kind=8) :: carsec(6), temp, rbid, casrho(6), casece(6)
!     ------------------------------------------------------------------
    data nomres/'E','NU','RHO','RHO_F_IN','RHO_F_EX','CM'/
!     ------------------------------------------------------------------
    zero = 0.d0
    un = 1.d0
    deux = 2.d0
!     ------------------------------------------------------------------
!
!     --- CARACTERISTIQUES DES ELEMENTS
!
    nno = 2
    nc = 7
    itype = 0
!
!     --- CARACTERISTIQUES GENERALES DES SECTIONS ---
    call jevech('PCAGNPO', 'L', lsect)
    lsect = lsect - 1
!
!     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
    call jevech('PMATERC', 'L', lmater)
!
    if (nomte .eq. 'MECA_POU_D_TG') then
        do 10 i = 1, nbres
            valres(i) = zero
10      continue
!
!
        fami='FPG1'
        kpg=1
        spt=1
        poum='+'
        call rcvalb(fami, kpg, spt, poum, zi(lmater),&
                    ' ', 'ELAS', 0, ' ', r8b,&
                    3, nomres, valres, codres, 1)
        e = valres(1)
        xnu = valres(2)
        rho = valres(3)
        g = e / ( deux * ( un + xnu ) )
!
        a = zr(lsect+1)
        xiy = zr(lsect+2)
        xiz = zr(lsect+3)
!
    else if (nomte.eq.'MECA_POU_D_TGM') then
!       CALCUL DE E ET G
        call pmfitx(zi(lmater), 1, casece, g)
!
!       CALCUL DE RHO MOYEN
        call pmfitx(zi(lmater), 2, casrho, rbid)
!
!
        call jevech('PNBSP_I', 'L', inbfib)
        nbfib = zi(inbfib)
        call jevech('PFIBRES', 'L', jacf)
        call pmfitg(nbfib, 3, zr(jacf), carsec)
        a = carsec(1)
        xiy = carsec(5)
        xiz = carsec(4)
!
        rho = casrho(1)/a
        e = casece(1)/a
!
    endif
!
    alfay = zr(lsect+4)
    alfaz = zr(lsect+5)
!     --- COORDONNEES DES NOEUDS ---
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
    xl = sqrt( (zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2))**2 + (zr(lx+6)-zr(lx+3) )**2 )
    if (xl .eq. zero) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call u2mesk('F', 'ELEMENTS2_43', 1, nomail)
    endif
!
!     --- RECUPERATION DES ORIENTATIONS ---
    call jevech('PCAORIE', 'L', lorien)
!     --- RECUPERATION DU VECTEUR ROTATION ---
    call jevech('PROTATR', 'L', irota)
    omega(1) = zr(irota+1)*zr(irota)
    omega(2) = zr(irota+2)*zr(irota)
    omega(3) = zr(irota+3)*zr(irota)
    call matrot(zr(lorien), pgl)
    do 1 i = 1, 3
        s=0.d0
        do 2 j = 1, 3
            s=s+pgl(i,j)*omega(j)
 2      continue
        omegl(i)=s
 1  end do
!
!CC     --- CALCUL DE LA MATRICE DE MASSE LOCALE ---
    do 20 i = 1, 105
        matp1(i) = 0.0d0
20  continue
!        --- POUTRE DROITE SECTION CONSTANTE OU VARIABLE (1 OU 2)
    call poriro(itype, matp1, rho, omegl, e,&
                a, a, xl, xiy, xiy,&
                xiz, xiz, g, alfay, alfay,&
                alfaz, alfaz)
    call masstg(matp1, mlv)
!
!
    call jevech('PMATUUR', 'E', lmat)
!
    call utpslg(nno, nc, pgl, mlv, zr(lmat))
!
end subroutine
