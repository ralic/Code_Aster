subroutine te0143(option, nomte)
! aslint: disable=
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/chgrep.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/jspgno.h"
#include "asterfort/lonele.h"
#include "asterfort/matro2.h"
#include "asterfort/matrot.h"
#include "asterfort/pmfitg.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/ptkg00.h"
#include "asterfort/ptkg20.h"
#include "asterfort/trigom.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
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
    character(len=16) :: ch16
    real(kind=8) :: a, xiy, xiz, ez, ey
    real(kind=8) :: a2, xiy2, xiz2, xl
    real(kind=8) :: rad, ang, angarc, angs2, xfly, xflz
    real(kind=8) :: pgl(3, 3), pgl1(3, 3), pgl2(3, 3), mat(105)
    real(kind=8) :: itype, iyr2, izr2, xfl, b(14), ksi1, d1b3(2, 3)
    real(kind=8) :: zero, sigma(14), carsec(6)
    integer :: lorien, nno, nc, i, lmat, ncomp
    integer :: lrcou, ldep, kp, adr, lx, npg, istrxr
    integer :: inbfib, nbfib, jacf
!     ------------------------------------------------------------------
    integer, parameter :: nb_cara = 11
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','IY1','IZ1','EY1','EZ1',&
                    'A2','IY2','IZ2','EY2','EZ2','TVAR'/
    integer, parameter :: nb_cara1 = 7
    real(kind=8) :: vale_cara1(nb_cara1)
    character(len=8) :: noms_cara1(nb_cara1)
    data noms_cara1 /'A1','IY1','IZ1','EY1','EZ1','IYR21','IZR21'/
!-----------------------------------------------------------------------
!
    zero = 0.0d0
    nno = 2
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
    if (nomte(1:13).eq.'MECA_POU_D_TG') then
        itype = 0
        nc = 7
        call poutre_modloc('CAGNP1', noms_cara1, nb_cara1, lvaleur=vale_cara1)
        a    = vale_cara1(1)
        xiy  = vale_cara1(2)
        xiz  = vale_cara1(3)
        ey   = vale_cara1(4)
        ez   = vale_cara1(5)
        iyr2 = vale_cara1(6)
        izr2 = vale_cara1(7)
    else
        call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
!
        nc = 6
        a      = vale_cara(1)
        xiy    = vale_cara(2)
        xiz    = vale_cara(3)
        a2     = vale_cara(6)
        xiy2   = vale_cara(7)
        xiz2   = vale_cara(8)
        ey = (vale_cara(4) +vale_cara(9))/2.d0
        ez = (vale_cara(5) +vale_cara(10))/2.d0
        itype = nint(vale_cara(11))
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
    endif
!
!     --- RECUPERATION DES ORIENTATIONS ---
    call jevech('PCAORIE', 'L', lorien)
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call lonele(3, lx, xl)
!
    if (itype .ne. 10) then
!        --- POUTRE DROITE ---
        call matrot(zr(lorien), pgl)
    else
!        --- POUTRE COURBE DE TIMOSHENKO A 6 DDL ---
        call utmess('F', 'ELEMENTS3_28')
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
    endif
!
    mat(:) = 0.d0
!
!     --- CALCUL DES MATRICES ELEMENTAIRES ----
    if (option .eq. 'RIGI_MECA_GE') then
!
!        --- CALCUL DE LA MATRICE DE RIGIDITE GEOMETRIQUE ---
        call jevech('PEFFORR', 'L', ldep)
        call jevech('PMATUUR', 'E', lmat)
        if (nc .eq. 6) then
!           NOMBRE DE POINTS DE GAUSS
            call elrefe_info(fami='RIGI',npg=npg)
            ASSERT((npg.eq.2).or.(npg.eq.3))
            if (npg .eq. 2) then
                do i = 1, nc
                    sigma(i) = zr(ldep+i-1)
                    sigma(i+nc) = zr(ldep+nc+i-1)
                enddo
            else
                do i = 1, nc
                    sigma(i) = zr(ldep+i-1)
                    sigma(i+nc) = zr(ldep+nc+nc+i-1)
                enddo
            endif
            if (itype .ne. 10) then
                call ptkg00(sigma, a, a2, xiz, xiz2,&
                            xiy, xiy2, xl, ey, ez,&
                            mat)
            else
                call utmess('A', 'ELEMENTS3_28')
            endif
!
        else if (nomte.eq.'MECA_POU_D_TG') then
            call jspgno(xl, zr(ldep), b)
            b(1:7) = -b(1:7)
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
            do i = 1, nc
                b(i) = zero
                b(i+nc) = zero
                do kp = 1, 3
                    adr = istrxr+ncomp*(kp-1)+i-1
                    b(i) = b(i) -zr(adr)*d1b3(1,kp)
                    b(i+nc)= b(i+nc)+zr(adr)*d1b3(2,kp)
                enddo
            enddo
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
        call utmess('F', 'ELEMENTS2_47', sk=ch16)
    endif
!
end subroutine
