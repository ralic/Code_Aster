subroutine te0151(option, nomte)
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/jevech.h'
    include 'asterfort/matro2.h'
    include 'asterfort/matrot.h'
    include 'asterfort/moytem.h'
    include 'asterfort/pmfrig.h'
    include 'asterfort/ptenci.h'
    include 'asterfort/ptenpo.h'
    include 'asterfort/ptenth.h'
    include 'asterfort/ptka01.h'
    include 'asterfort/ptka02.h'
    include 'asterfort/ptka10.h'
    include 'asterfort/ptka21.h'
    include 'asterfort/ptma01.h'
    include 'asterfort/ptma10.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/tecach.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utpvgl.h'
    include 'asterfort/vecma.h'
    include 'asterfort/verifm.h'
    character(len=*) :: option, nomte
!     ------------------------------------------------------------------
! TOLE CRP_6
!     CALCUL
!       - ENERGIE DE DEFORMATION
!       - ENERGIE CINETIQUE
!     POUR LES ELEMENTS DE POUTRE D'EULER ET DE TIMOSHENKO.
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        'EPOT_ELEM' : ENERGIE DE DEFORMATION
!        'ECIN_ELEM' : ENERGIE CINETIQUE
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!     'MECA_POU_D_E'   : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!     'MECA_POU_D_T'   : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!     'MECA_POU_C_T'   : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
!     'MECA_POU_D_TG'  : POUTRE DROITE DE TIMOSHENKO(SECTION CONSTANTE)
!                        AVEC GAUCHISSEMENT
!     'MECA_POU_D_EM'  : POUTRE DROITE D'EULER MULTI-FIBRE
!                        (SECTION CONSTANTE)
!     'MECA_POU_D_TGM' :POUTRE DROITE DE TIMOSHENKO(SECTION CONSTANTE)
!                        MULTIFIBRE AVEC GAUCHISSEMENT
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, if, iret, istruc, itype, jdepl, jende
    integer :: jfreq, jmasd, jvite, kanl, lmater, lorien, lrcou
    integer :: lsect, lsect2, lx, nbpar, nbres, nc, nno
    integer :: npg
    real(kind=8) :: a, a2, alfay, alfay2, alfaz, alfaz2, ang
    real(kind=8) :: angarc, angs2, deux, e, enerth, ey, ez
    real(kind=8) :: g, rad, rho, un, valpar, x2iy, x2iz
    real(kind=8) :: xfl, xfly, xflz, xiy, xiy2, xiz, xiz2
    real(kind=8) :: xjx, xjx2, xl, xnu, zero, xig
!-----------------------------------------------------------------------
    parameter    (             nbres = 3 )
    real(kind=8) :: valres(nbres)
    integer :: codres(nbres)
    character(len=1) :: stopz(3)
    character(len=4) :: fami
    character(len=8) :: nompar, nomres(nbres), nomail, famil, poum
    character(len=16) :: ch16
    real(kind=8) :: ul(14), ug(14), pgl(3, 3), klc(14, 14), klv(105)
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), epsthe
    integer :: iadzi, iazk24, kpg, spt, nklv
!     ------------------------------------------------------------------
    data nomres / 'E' , 'NU' , 'RHO' /
!     ------------------------------------------------------------------
    zero = 0.d0
    un = 1.d0
    deux = 2.d0
!     ------------------------------------------------------------------
!
!     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
    call jevech('PMATERC', 'L', lmater)
    do 10 i = 1, nbres
        valres(i) = zero
10  end do
!
    fami = 'RIGI'
    npg = 3
    if ((nomte.eq.'MECA_POU_C_T') .or. (nomte.eq.'MECA_POU_D_EM')) npg = 2
!
    call moytem(fami, npg, 1, '+', valpar,&
                iret)
    call verifm(fami, npg, 1, '+', zi(lmater),&
                'ELAS', 1, epsthe, iret)
    nbpar = 1
    nompar = 'TEMP'
    famil='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call rcvalb(famil, kpg, spt, poum, zi(lmater),&
                ' ', 'ELAS', nbpar, nompar, valpar,&
                2, nomres, valres, codres, 1)
    call rcvalb(famil, kpg, spt, poum, zi(lmater),&
                ' ', 'ELAS', nbpar, nompar, valpar,&
                1, nomres(3), valres(3), codres(3), 1)
!
    e = valres(1)
    xnu = valres(2)
    rho = valres(3)
    g = e / ( deux * ( un + xnu ) )
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
    call jevech('PCAGNPO', 'L', lsect)
    lsect = lsect-1
    itype = nint(zr(lsect+23))
!
!
!       --- RECUPERATION DES ORIENTATIONS ---
    call jevech('PCAORIE', 'L', lorien)
!
    if ((nomte.ne.'MECA_POU_D_EM') .and. (nomte.ne.'MECA_POU_D_TGM')) then
!
!       --- SECTION INITIALE ---
        a = zr(lsect+ 1)
        xiy = zr(lsect+ 2)
        xiz = zr(lsect+ 3)
        alfay = zr(lsect+ 4)
        alfaz = zr(lsect+ 5)
!       EY    = -ZR(LSECT+ 6)
!       EZ    = -ZR(LSECT+ 7)
        xjx = zr(lsect+ 8)
        xig = zr(lsect+12)
!
!       --- SECTION FINALE ---
        lsect2 = lsect + 11
        a2 = zr(lsect2+ 1)
        xiy2 = zr(lsect2+ 2)
        xiz2 = zr(lsect2+ 3)
        alfay2 = zr(lsect2+ 4)
        alfaz2 = zr(lsect2+ 5)
        ey = -(zr(lsect+6)+zr(lsect2+6))/deux
        ez = -(zr(lsect+7)+zr(lsect2+7))/deux
        xjx2 = zr(lsect2+ 8)
!
!
!       --- RECUPERATION DES COORDONNEES DES NOEUDS ---
        call jevech('PGEOMER', 'L', lx)
        lx = lx - 1
        xl = sqrt( ( zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2))**2 + ( zr(lx+6)-zr(lx+3) )**2 )
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        if (xl .eq. zero) then
            call u2mesk('F', 'ELEMENTS2_43', 1, nomail)
        endif
!
    endif
!
    if (nomte .eq. 'MECA_POU_D_E') then
!        --- POUTRE DROITE D'EULER A 6 DDL ---
        istruc = 1
        nno = 2
        nc = 6
        alfay = zero
        alfaz = zero
        alfay2 = zero
        alfaz2 = zero
        call matrot(zr(lorien), pgl)
    else if (nomte.eq.'MECA_POU_D_EM') then
        istruc = 1
        nno = 2
        nc = 6
        call matrot(zr(lorien), pgl)
    else if (nomte .eq. 'MECA_POU_D_T') then
!        --- POUTRE DROITE DE TIMOSKENKO A 6 DDL ---
        istruc = 1
        nno = 2
        nc = 6
        call matrot(zr(lorien), pgl)
        elseif ( nomte .eq. 'MECA_POU_D_TG' .or. nomte .eq.&
    'MECA_POU_D_TGM' ) then
!        --- POUTRE DROITE DE TIMOSKENKO A 7 DDL ---
        istruc = 1
        nno = 2
        nc = 7
        call matrot(zr(lorien), pgl)
    else if (nomte .eq. 'MECA_POU_C_T') then
!        --- POUTRE COURBE DE TIMOSKENKO A 6 DDL ---
        istruc = 1
        nno = 1
        nc = 6
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        angarc = zr(lrcou+1)
        xfl = zr(lrcou+2)
        xfly = xfl
        xflz = xfl
        if (xfl .eq. zero) then
            xfly = zr(lrcou+4)
            xflz = zr(lrcou+6)
        endif
        angs2 = asin( xl / ( deux * rad ) )
        ang = angs2 * deux
        xl = rad * ang
        x2iy = xiy
        x2iz = xiz
        xiy = xiy / xfly
        xiz = xiz / xflz
        xiy2 = xiy2 / xfly
        xiz2 = xiz2 / xflz
        call matro2(zr(lorien), angarc, angs2, pgl1, pgl2)
    else
        ch16 = nomte
        call u2mesk('F', 'ELEMENTS2_42', 1, ch16)
    endif
!
    nklv = 2*nc*(2*nc+1)/2
!
    if (option .ne. 'ECIN_ELEM') then
        call jevech('PDEPLAR', 'L', jdepl)
        do 20 i = 1, 2*nc
            ug(i) = zr(jdepl+i-1)
20      continue
    else
        stopz(1)='O'
        stopz(2)='N'
        stopz(3)='O'
        call tecach(stopz, 'PVITESR', 'L', 1, jvite,&
                    iret)
! IRET NE PEUT VALOIR QUE 0 (TOUT EST OK) OU 2 (CHAMP NON FOURNI)
        if (iret .eq. 0) then
            do 21 i = 1, 2*nc
                ug(i) = zr(jvite+i-1)
21          continue
        else
            call tecach(stopz, 'PDEPLAR', 'L', 1, jdepl,&
                        iret)
            if (iret .eq. 0) then
                do 22 i = 1, 2*nc
                    ug(i) = zr(jdepl+i-1)
22              continue
            else
                call u2mesk('F', 'ELEMENTS2_1', 1, option)
            endif
        endif
    endif
!
!     --- MATRICE DE ROTATION PGL
!     --- VECTEUR DEPLACEMENT OU VITESSE LOCAL  UL = PGL * UG
    if (itype .eq. 10) then
        call utpvgl(nno, nc, pgl1, ug, ul)
        call utpvgl(nno, nc, pgl2, ug(7), ul(7))
    else
        call utpvgl(nno, nc, pgl, ug, ul)
    endif
!
!                    --- ENERGIE DE DEFORMATION ----
!
    if (option .eq. 'EPOT_ELEM') then
        call jevech('PENERDR', 'E', jende)
!
!        --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE
        if (itype .eq. 0) then
!           --- POUTRE DROITE A SECTION CONSTANTE ---
            if ((nomte.eq.'MECA_POU_D_EM') .or. ( nomte.eq.'MECA_POU_D_TGM')) then
                call pmfrig(nomte, zi(lmater), klv)
            else if (nomte.eq.'MECA_POU_D_TG') then
                call ptka21(klv, e, a, xl, xiy,&
                            xiz, xjx, xig, g, alfay,&
                            alfaz, ey, ez)
            else
                call ptka01(klv, e, a, xl, xiy,&
                            xiz, xjx, g, alfay, alfaz,&
                            ey, ez, istruc)
            endif
        else if (itype .eq. 1 .or. itype .eq. 2) then
!           --- POUTRE DROITE A SECTION VARIABLE (TYPE 1 OU 2) ---
            call ptka02(itype, klv, e, a, a2,&
                        xl, xiy, xiy2, xiz, xiz2,&
                        xjx, xjx2, g, alfay, alfay2,&
                        alfaz, alfaz2, ey, ez, istruc)
        else if (itype .eq. 10) then
!           --- POUTRE COURBE A SECTION CONSTANTE ---
            call ptka10(klv, e, a, xiy, xiz,&
                        xjx, g, alfay, alfaz, rad,&
                        ang, istruc)
        endif
!
!        ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
        call vecma(klv, nklv, klc, 2*nc)
!        --- ENERGIE DE DEFORMATION
        if = 1
        call ptenpo(nc*2, ul, klc, zr(jende), itype,&
                    if)
        if (epsthe .ne. zero) then
            call ptenth(ul, xl, epsthe, 2*nc, klc,&
                        itype, enerth)
            zr(jende) = zr(jende) - enerth
        endif
!
!                     --- ENERGIE CINETIQUE ----
!
    else if (option .eq. 'ECIN_ELEM') then
        call jevech('PENERCR', 'E', jende)
        call jevech('PMASDIA', 'L', jmasd)
        call jevech('POMEGA2', 'L', jfreq)
        kanl = zi(jmasd)
!
!        --- CALCUL DE LA MATRICE DE MASSE LOCALE
        if (rho .ne. zero) then
!           --- KANL = 0 MASSES CONCENTREES
!           --- KANL = 1 MASSES COHERENTES
            if (itype .lt. 10) then
!              --- POUTRE DROITE SECTION CONSTANTE OU VARIABLE (1 OU 2)
                call ptma01(kanl, itype, klv, istruc, rho,&
                            e, a, a2, xl, xiy,&
                            xiy2, xiz, xiz2, g, alfay,&
                            alfay2, alfaz, alfaz2, ey, ez)
            else if (itype.eq.10) then
!              --- POUTRE COURBE SECTION CONSTANTE ---
                call ptma10(klv, rho, a, xl, x2iy,&
                            x2iz)
            endif
!
!           ---- MATRICE MASSE LIGNE > MATRICE MASSE CARRE
            call vecma(klv, 78, klc, 12)
!           --- ENERGIE CINETIQUE
            if = 1
            call ptenci(12, ul, klc, zr(jfreq), zr(jende),&
                        itype, kanl, if)
!
        else if (codres(3).ne.0) then
            call u2mess('F', 'ELEMENTS3_31')
        endif
!
    else
        ch16 = option
        call u2mesk('F', 'ELEMENTS2_47', 1, ch16)
    endif
!
end subroutine
