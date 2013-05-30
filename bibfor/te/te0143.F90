subroutine te0143(option, nomte)
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/chgrep.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/jspgno.h'
    include 'asterfort/matro2.h'
    include 'asterfort/matrot.h'
    include 'asterfort/pmfitg.h'
    include 'asterfort/ptkg00.h'
    include 'asterfort/ptkg20.h'
    include 'asterfort/tecael.h'
    include 'asterfort/trigom.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE  CRP_6
!-----------------------------------------------------------------------
!     CALCULE LA MATRICE DE RIGIDITE GEOMETRIQUE ELEMENTAIRE DES
!     ELEMENTS DE POUTRE D'EULER ET DE TIMOSHENKO
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        'RIGI_MECA_GE' : CALCUL DE LA MATRICE DE RIGIDITE
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!       'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!       'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!       'MECA_POU_C_T'  : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
!       'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
!       'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!                         MULTI-FIBRES SECTION CONSTANTE
!     ------------------------------------------------------------------
!
!
    character(len=8) :: nomail
    character(len=16) :: ch16
    real(kind=8) :: a, xiy, xiz, ez, ey
    real(kind=8) :: a2, xiy2, xiz2, xl
    real(kind=8) :: rad, ang, angarc, angs2, xfly, xflz
    real(kind=8) :: pgl(3, 3), pgl1(3, 3), pgl2(3, 3), mat(105)
    real(kind=8) :: itype, iyr2, izr2, xfl, b(14), ksi1, d1b3(2, 3)
    real(kind=8) :: zero, sigma(14), carsec(6)
    integer :: lsect, lsect2, lorien, nno, nc, i, lmat, ncomp
    integer :: lrcou, ldep, kp, adr, lx, iadzi, iazk24, npg, iplouf, istrxr
    integer :: inbfib, nbfib, jacf
!     ------------------------------------------------------------------
!
    zero = 0.0d0
    itype= 0
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
    call jevech('PCAGNPO', 'L', lsect)
    lsect = lsect-1
    if (nomte .eq. 'MECA_POU_D_TG' .and. nomte .eq. 'MECA_POU_D_TGM') then
        itype = nint(zr(lsect+23))
    endif
!
!     --- SECTION INITIALE ---
    if (nomte .eq. 'MECA_POU_D_TGM') then
        call jevech('PNBSP_I', 'L', inbfib)
        nbfib = zi(inbfib)
        call jevech('PFIBRES', 'L', jacf)
        call pmfitg(nbfib, 3, zr(jacf), carsec)
        a = carsec(1)
        xiy = carsec(5)
        xiz = carsec(4)
    else
        a = zr(lsect+1)
        xiy = zr(lsect+2)
        xiz = zr(lsect+3)
    endif
!
    if (nomte .eq. 'MECA_POU_D_TG' .or. nomte .eq. 'MECA_POU_D_TGM') then
        ey = -zr(lsect+6)
        ez = -zr(lsect+7)
        iyr2= zr(lsect+10)
        izr2= zr(lsect+11)
    else
!     --- SECTION FINALE ---
        lsect2 = lsect + 11
        a2 = zr(lsect2+1)
        xiy2 = zr(lsect2+2)
        xiz2 = zr(lsect2+3)
        ey = -(zr(lsect+6)+zr(lsect2+6))/2.d0
        ez = -(zr(lsect+7)+zr(lsect2+7))/2.d0
    endif
!
!     --- RECUPERATION DES ORIENTATIONS ---
    call jevech('PCAORIE', 'L', lorien)
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
    xl = sqrt( (zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2))**2 + (zr(lx+6)-zr(lx+3) )**2 )
    if (xl .eq. 0.d0) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call u2mesk('F', 'ELEMENTS2_43', 1, nomail)
    endif
!
    if (nomte .eq. 'MECA_POU_D_E') then
!        --- POUTRE DROITE D'EULER A 6 DDL ---
        nno = 2
        nc = 6
        call matrot(zr(lorien), pgl)
    else if (nomte .eq. 'MECA_POU_D_T') then
!        --- POUTRE DROITE DE TIMOSKENKO A 6 DDL ---
        nno = 2
        nc = 6
        call matrot(zr(lorien), pgl)
    else if (nomte .eq. 'MECA_POU_C_T') then
        nno = 2
        nc = 6
!        --- POUTRE COURBE DE TIMOSHENKO A 6 DDL ---
        call u2mess('F', 'ELEMENTS3_28')
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
        angs2 = trigom('ASIN', xl / ( 2.d0 * rad ) )
        ang = angs2 * 2.d0
        xl = rad * ang
        xiy = xiy / xfly
        xiz = xiz / xflz
        xiy2 = xiy2 / xfly
        xiz2 = xiz2 / xflz
        call matro2(zr(lorien), angarc, angs2, pgl1, pgl2)
!
        else if(nomte.eq.'MECA_POU_D_TG'.or. nomte.eq.'MECA_POU_D_TGM'&
    ) then
        nno = 2
        nc = 7
        call matrot(zr(lorien), pgl)
    else
        ch16 = nomte
        call u2mesk('F', 'ELEMENTS2_42', 1, ch16)
    endif
!
    do 10 i = 1, 105
        mat(i) = 0.d0
10  end do
!
!     --- CALCUL DES MATRICES ELEMENTAIRES ----
    if (option .eq. 'RIGI_MECA_GE') then
!
!        --- CALCUL DE LA MATRICE DE RIGIDITE GEOMETRIQUE ---
        call jevech('PEFFORR', 'L', ldep)
        call jevech('PMATUUR', 'E', lmat)
        if (nomte .eq. 'MECA_POU_D_T' .or. nomte .eq. 'MECA_POU_D_E' .or. nomte .eq.&
            'MECA_POU_C_T') then
!           NOMBRE DE POINTS DE GAUSS
            call elref4(' ', 'RIGI', iplouf, iplouf, iplouf,&
                        npg, iplouf, iplouf, iplouf, iplouf)
            call assert((npg.eq.2).or.(npg.eq.3))
            if (npg .eq. 2) then
                do 15 i = 1, nc
                    sigma(i) = zr(ldep+i-1)
                    sigma(i+nc) = zr(ldep+nc+i-1)
15              continue
            else
                do 17 i = 1, nc
                    sigma(i) = zr(ldep+i-1)
                    sigma(i+nc) = zr(ldep+nc+nc+i-1)
17              continue
            endif
            if (itype .ne. 10) then
                call ptkg00(sigma, a, a2, xiz, xiz2,&
                            xiy, xiy2, xl, ey, ez,&
                            mat)
            else
                call u2mess('A', 'ELEMENTS3_28')
            endif
!
        else if (nomte.eq.'MECA_POU_D_TG') then
            call jspgno(xl, zr(ldep), b)
            do 20 i = 1, 7
                b(i) = -b(i)
20          continue
            call ptkg20(b, a, xiz, xiy, iyr2,&
                        izr2, xl, ey, ez, mat)
!
        else if (nomte.eq.'MECA_POU_D_TGM') then
!     --- RECUPERATION DES CARACTERISTIQUES DES FIBRES
            call jevech('PNBSP_I', 'L', i)
!
!     ON PROJETTE AVEC LES FCTS DE FORME
!     SUR LES NOEUDS DEBUT ET FIN DE L'ELEMENT
!     POUR LE POINT 1
            ksi1 = -sqrt( 5.d0 / 3.d0 )
            d1b3(1,1) = ksi1*(ksi1-1.d0)/2.0d0
            d1b3(1,2) = 1.d0-ksi1*ksi1
            d1b3(1,3) = ksi1*(ksi1+1.d0)/2.0d0
!     POUR LE POINT 2
            ksi1 = sqrt( 5.d0 / 3.d0 )
            d1b3(2,1) = ksi1*(ksi1-1.d0)/2.0d0
            d1b3(2,2) = 1.d0-ksi1*ksi1
            d1b3(2,3) = ksi1*(ksi1+1.d0)/2.0d0
!
!     ON RECUPERE LES EFFORTS GENERALISE CF OPTION SIEF_ELNO
            call jevech('PSTRXRR', 'L', istrxr)
!     NOMBRE DE COMPOSANTE PAR PDG DANS LE CHAMPS PSTRXRR
            ncomp = 18
!       AU NOEUD 1 ON RECUPERE   -EFFORT STOCKE
!       AU NOEUD 2 ON RECUPERE   +EFFORT STOCKE
            do 210 i = 1, nc
                b(i) = zero
                b(i+nc) = zero
                do 212 kp = 1, 3
                    adr = istrxr+ncomp*(kp-1)+i-1
                    b(i) = b(i) -zr(adr)*d1b3(1,kp)
                    b(i+nc)= b(i+nc)+zr(adr)*d1b3(2,kp)
212              continue
210          continue
            call ptkg20(b, a, xiz, xiy, iyr2,&
                        izr2, xl, ey, ez, mat)
        endif
!
!        --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
        if (itype .eq. 10) then
            call chgrep('LG', pgl1, pgl2, mat, zr(lmat))
        else
            call utpslg(nno, nc, pgl, mat, zr(lmat))
        endif
!
    else
        ch16 = option
        call u2mesk('F', 'ELEMENTS2_47', 1, ch16)
    endif
!
end subroutine
