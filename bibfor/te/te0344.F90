subroutine te0344(option, nomte)
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
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jevech.h'
    include 'asterfort/matrot.h'
    include 'asterfort/moytem.h'
    include 'asterfort/pmavec.h'
    include 'asterfort/posigr.h'
    include 'asterfort/posipr.h'
    include 'asterfort/ptforp.h'
    include 'asterfort/ptka21.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/tecach.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/utpvgl.h'
    include 'asterfort/vecma.h'
    include 'asterfort/verift.h'
    character(len=*) :: option, nomte
! --- ------------------------------------------------------------------
! INSPI TE0144
!     CALCUL DU VECTEUR ELEMENTAIRE EFFORT GENERALISE,
!     POUR LES ELEMENTS DE POUTRE DE TIMOSHENKO AVEC GAUCHISSEMENT.
! --- ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        SIGM_ELNO
!        SIPO_ELNO
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!        MECA_POU_D_TG : POUTRE DROITE DE TIMOSHENKO AVEC GAUCHISSEMENT
!
!
    integer :: nbres
    parameter     (nbres=2)
    integer :: lmater, jmat, nbmat, imat, icomp, nbpar, i, j, npg, nno, nc
    integer :: ncc, jeffo, iadzi, iazk24, iret, lsect, itype, lx
    integer :: lorien, jdepl, iret1, lforcr, lforcf
    real(kind=8) :: valres(nbres)
    integer :: codres(nbres)
    character(len=8) :: nompar, nomres(nbres), nomail
    character(len=16) :: messk(2)
    real(kind=8) :: valpar, zero, angs2, rad, e, g, a, xiz, alfaz, alfay, ey, ez
    real(kind=8) :: xjx, xjg, xiy, xl, epsith
    real(kind=8) :: nu, fe(12), fi(12), flr(14), klv(105)
    real(kind=8) :: ulr(14), ugr(14), pgl(14, 14), klc(14, 14)
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3)
    logical :: okopt
!     ------------------------------------------------------------------
    data nomres/'E','NU'/
!     ------------------------------------------------------------------
!
    okopt = (option.eq.'SIPM_ELNO') .or. (option.eq.'SIPO_ELNO')
    call assert(okopt)
!
! --- ------------------------------------------------------------------
! --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
    call jevech('PMATERC', 'L', lmater)
! --- ------------------------------------------------------------------
!     BLINDAGE : OPTION VALIDE AVEC UN SEUL PHENOMENE : ELAS
    jmat = zi(lmater)
    nbmat= zi(jmat)
!     UN SEUL MATERIAU
    if (nbmat .ne. 1) then
        messk(1) = option
        call u2mesk('F', 'ELEMENTS4_59', 1, messk)
    endif
!     LE 1ER MATERIAU
    imat = jmat+zi(jmat+nbmat+1)
!     SEUL ELAS EST AUTORISE
    do 152 icomp = 1, zi(imat+1)
        if (zk16(zi(imat)+icomp-1)(1:4) .ne. 'ELAS') then
            messk(1) = option
            messk(2) = zk16(zi(imat)+icomp-1)(1:10)
            call u2mesk('F', 'ELEMENTS4_64', 2, messk)
        endif
152  end do
! --- ------------------------------------------------------------------
    nbpar = 0
    nompar = '  '
    valpar = 0.d0
    zero = 0.d0
    angs2 = zero
    rad = zero
    do 10 i = 1, nbres
        valres(i) = zero
10  end do
!
    do 11 i = 1, 3
        do 11 j = 1, 3
            pgl1(i,j) = zero
            pgl2(i,j) = zero
11      continue
!
    npg = 3
    call moytem('RIGI', npg, 1, '+', valpar,&
                iret)
!
    nbpar = 1
    nompar = 'TEMP'
!
    call rcvalb('RIGI', 1, 1, '+', zi(lmater),&
                ' ', 'ELAS', nbpar, nompar, valpar,&
                2, nomres, valres, codres, 1)
!
    e = valres(1)
    nu = valres(2)
    g = e / ( 2.d0 * ( 1.d0 + nu ) )
! --- ------------------------------------------------------------------
! --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS
    call jevech('PCAGNPO', 'L', lsect)
    lsect = lsect - 1
    a = zr(lsect+1)
    xiy = zr(lsect+2)
    xiz = zr(lsect+3)
    alfay = zr(lsect+4)
    alfaz = zr(lsect+5)
    ey = -zr(lsect+6)
    ez = -zr(lsect+7)
    xjx = zr(lsect+8)
    xjg = zr(lsect+12)
    itype = nint(zr(lsect+23))
    nno = 2
    nc = 7
    ncc = 6
! --- ------------------------------------------------------------------
! --- RECUPERATION DES COORDONNEES DES NOEUDS
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
    xl = sqrt( (zr(lx+4)-zr(lx+1))**2+ (zr(lx+5)-zr(lx+2))**2+ (zr(lx+6)-zr(lx+3))**2 )
    if (xl .eq. 0.d0) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call u2mesk('F', 'ELEMENTS2_43', 1, nomail)
    endif
! --- ------------------------------------------------------------------
! --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA
    call jevech('PCAORIE', 'L', lorien)
    call matrot(zr(lorien), pgl)
! --- ------------------------------------------------------------------
! --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE
    call ptka21(klv, e, a, xl, xiy,&
                xiz, xjx, xjg, g, alfay,&
                alfaz, ey, ez)
! --- ------------------------------------------------------------------
! --- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
    call vecma(klv, 105, klc, 14)
!
    call jevech('PDEPLAR', 'L', jdepl)
    do 510 i = 1, 14
        ugr(i) = zr(jdepl+i-1)
510  end do
! --- VECTEUR DEPLACEMENT LOCAL  ULR = PGL * UGR
    call utpvgl(nno, nc, pgl, ugr, ulr)
! --- VECTEUR EFFORT       LOCAL  FLR = KLC * ULR
    call pmavec('ZERO', 14, klc, ulr, flr)
! --- ------------------------------------------------------------------
! --- TENIR COMPTE DES EFFORTS DUS A LA DILATATION
    call verift('RIGI', npg, 1, '+', zi(lmater),&
                'ELAS', 1, epsith, iret1)
    do 20 i = 1, 14
        ugr(i) = 0.d0
20  end do
    ugr(1) = -epsith*xl
    ugr(8) = -ugr(1)
! --- ------------------------------------------------------------------
! --- CALCUL DES FORCES INDUITES
    do 35 i = 1, 7
        flr(i) = flr(i) - klc(i,1)*ugr(1)
        flr(i+7) = flr(i+7) - klc(i+7,1+7)*ugr(1+7)
35  end do
! --- ------------------------------------------------------------------
! --- PRISE EN COMPTE DES EFFORTS REPARTIS
    call tecach('ONN', 'PFR1D1D', 'L', 1, lforcr,&
                iret)
    if (lforcr .ne. 0) then
        call ptforp(itype, 'CHAR_MECA_FR1D1D', nomte, a, a,&
                    xl, rad, angs2, 1, nno,&
                    ncc, pgl, pgl1, pgl2, fe,&
                    fi)
        do 100 i = 1, 6
            flr(i) = flr(i) - fe(i)
            flr(i+7) = flr(i+7) - fe(i+6)
100      continue
    endif
! --- ------------------------------------------------------------------
! --- PRISE EN COMPTE DES EFFORTS REPARTIS (SOUS FORME DE FONCTION)
    call tecach('ONN', 'PFF1D1D', 'L', 1, lforcf,&
                iret)
    if (lforcf .ne. 0) then
        call ptforp(itype, 'CHAR_MECA_FF1D1D', nomte, a, a,&
                    xl, rad, angs2, 1, nno,&
                    ncc, pgl, pgl1, pgl2, fe,&
                    fi)
        do 110 i = 1, 6
            flr(i) = flr(i) - fe(i)
            flr(i+7) = flr(i+7) - fe(i+6)
110      continue
    endif
!
! --- ------------------------------------------------------------------
! --- ARCHIVAGE
!
    if (option .eq. 'SIPM_ELNO') then
        call jevech('PCONTRR', 'E', jeffo)
        do 210 i = 1, 6
            fe(i) = flr(i)
            fe(i+6) = flr(i+7)
210      continue
        call posigr(nomte, fe, zr(jeffo))
!
    else if (option.eq.'SIPO_ELNO') then
        call jevech('PCONTPO', 'E', jeffo)
        do 220 i = 1, 6
            fe(i) = flr(i)
            fe(i+6) = flr(i+7)
220      continue
        call posipr(nomte, fe, zr(jeffo))
!
    endif
!
end subroutine
