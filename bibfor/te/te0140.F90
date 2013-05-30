subroutine te0140(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/chgrep.h'
    include 'asterfort/jevech.h'
    include 'asterfort/matro2.h'
    include 'asterfort/matrot.h'
    include 'asterfort/moytem.h'
    include 'asterfort/pmfrig.h'
    include 'asterfort/porigi.h'
    include 'asterfort/ptka21.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/tecael.h'
    include 'asterfort/trigom.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/utpslg.h'
    character(len=16) :: option, nomte
!     ------------------------------------------------------------------
!     CALCULE LA MATRICE DE RIGIDITE ELEMENTAIRE DES ELEMENTS DE POUTRE
!     D'EULER ET DE TIMOSHENKO
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!      'RIGI_MECA '     : CALCUL DE LA MATRICE DE RIGIDITE
!      'RIGI_FLUI_STRU' : CALCUL DE LA MATRICE DE RIGIDITE (ABS_CURV)
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!      'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!      'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!      'MECA_POU_C_T'  : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
!      'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
!      'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!      'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!                         MULTI-FIBRES SECTION CONSTANTE
!     ------------------------------------------------------------------
!
!
    integer :: iadzi, iazk24
    integer :: i, imate, lmat, lorien, lrcou
    integer :: lx, nbpar, nbres, nc, nno, lsect, iret
    parameter (nbres=2)
    real(kind=8) :: valres(nbres)
    real(kind=8) :: angarc, angs2, deux, e, rad
    real(kind=8) :: valpar, xl, xnu, g, un, zero
    integer :: codres(nbres), kpg, spt
    character(len=8) :: nompar, nomres(nbres), nomail, fami, poum
    character(len=16) :: opti
    real(kind=8) :: pgl(3, 3), pgl1(3, 3), pgl2(3, 3), klv(105)
    real(kind=8) :: a, xiy, xiz, alfay, alfaz, xjx, xjg, ez, ey, mat(105)
    integer :: inbfib, nbfib, jacf
    real(kind=8) :: carsec(6)
!     ------------------------------------------------------------------
    data nomres/'E','NU'/
!     ------------------------------------------------------------------
    zero = 0.d0
    un = 1.d0
    deux = 2.d0
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!     ------------------------------------------------------------------
!
!     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
!
    if (option(1:9) .eq. 'RIGI_MECA') then
        opti = 'ELAS'
    else if (option(1:14).eq.'RIGI_FLUI_STRU') then
        opti = 'ELAS_FLUI'
    else
! OPTION NON PROGRAMMEE
        call assert(.false.)
    endif
    if (nomte .ne. 'MECA_POU_D_TG' .and. nomte .ne. 'MECA_POU_D_TGM') then
        call moytem('NOEU', 2, 1, '+', valpar,&
                    iret)
    else
        call moytem('RIGI', 3, 1, '+', valpar,&
                    iret)
    endif
    nbpar = 1
    nompar = 'TEMP'
!
    call jevech('PMATERC', 'L', imate)
    if ((nomte.ne.'MECA_POU_D_EM') .and. (nomte.ne.'MECA_POU_D_TGM')) then
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', opti, nbpar, nompar, valpar,&
                    nbres, nomres, valres, codres, 1)
        e = valres(1)
        xnu = valres(2)
        g = e/ (deux* (un+xnu))
    endif
!
!     --- RECUPERATION DES ORIENTATIONS ---
    call jevech('PCAORIE', 'L', lorien)
!
    do 25 i = 1, 105
        klv(i) = 0.d0
25  end do
!
!     --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE ---
    if ((nomte.eq.'MECA_POU_D_EM') .or. (nomte.eq.'MECA_POU_D_TGM')) then
        call pmfrig(nomte, zi(imate), klv)
    else if (nomte.ne.'MECA_POU_D_TG') then
        call porigi(nomte, e, xnu, klv)
    endif
!
    call jevech('PMATUUR', 'E', lmat)
!
    if (nomte .eq. 'MECA_POU_D_EM' .or. nomte .eq. 'MECA_POU_D_E' .or. nomte .eq.&
        'MECA_POU_D_T') then
        nno = 2
        nc = 6
        call matrot(zr(lorien), pgl)
        call utpslg(nno, nc, pgl, klv, zr(lmat))
!
    else if (nomte.eq.'MECA_POU_C_T') then
        call jevech('PGEOMER', 'L', lx)
        lx = lx - 1
        xl = sqrt( ( zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2))**2 + (zr(lx+6)-zr(lx+3) )**2 )
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        angarc = zr(lrcou+1)
        angs2 = trigom('ASIN',xl/ (deux*rad))
        call matro2(zr(lorien), angarc, angs2, pgl1, pgl2)
        call chgrep('LG', pgl1, pgl2, klv, zr(lmat))
!
    else if (nomte.eq.'MECA_POU_D_TGM') then
        nno = 2
        nc = 7
        call matrot(zr(lorien), pgl)
        call utpslg(nno, nc, pgl, klv, zr(lmat))
    else if (nomte.eq.'MECA_POU_D_TG') then
!     --- CARACTERISTIQUES GENERALES DES SECTIONS ---
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
        nno = 2
        nc = 7
!     --- COORDONNEES DES NOEUDS ---
        call jevech('PGEOMER', 'L', lx)
        lx = lx - 1
        xl = sqrt( ( zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2))**2 + (zr(lx+6)-zr(lx+3) )**2 )
        if (xl .eq. zero) then
            call tecael(iadzi, iazk24)
            nomail = zk24(iazk24-1+3)(1:8)
            call u2mesk('F', 'ELEMENTS2_43', 1, nomail)
        endif
!     --- CALCUL DES MATRICES ELEMENTAIRES ----
        if (option(1:9) .eq. 'RIGI_MECA' .or. option(1:9) .eq. 'RIGI_FLUI') then
            call ptka21(klv, e, a, xl, xiy,&
                        xiz, xjx, xjg, g, alfay,&
                        alfaz, ey, ez)
        endif
!        --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
        call matrot(zr(lorien), pgl)
        call utpslg(nno, nc, pgl, klv, zr(lmat))
    else
        call u2mesk('F', 'ELEMENTS2_42', 1, nomte)
    endif
!
end subroutine
