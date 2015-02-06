subroutine te0143(option, nomte)
! aslint: disable=
!
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
! --------------------------------------------------------------------------------------------------
!
!     CALCULE LA MATRICE DE RIGIDITE GEOMETRIQUE ELEMENTAIRE DES
!               ELEMENTS DE POUTRE D'EULER ET DE TIMOSHENKO
!
! --------------------------------------------------------------------------------------------------
!
!   IN
!       OPTION  : NOM DE L'OPTION A CALCULER
!           'RIGI_MECA_GE' : CALCUL DE LA MATRICE DE RIGIDITE
!       NOMTE   :   NOM DU TYPE ELEMENT
!           'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!           'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!           'MECA_POU_C_T'  : POUTRE COURBE DE TIMOSHENKO (SECTION CONSTANTE)
!           'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D'EULER (SECT. CONST)
!           'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!           'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT) MULTIFIBRES
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
    character(len=*) :: option, nomte
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/chgrep.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/jspgno.h"
#include "asterfort/lonele.h"
#include "asterfort/matro2.h"
#include "asterfort/matrot.h"
#include "asterfort/pmfinfo.h"
#include "asterfort/pmfitg.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/ptkg00.h"
#include "asterfort/ptkg20.h"
#include "asterfort/trigom.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: lorien, nno, nc, i, lmat, ncomp, itype
    integer :: lrcou, ldep, kp, adr, npg, istrxr, jacf
    real(kind=8) :: a, xiy, xiz, ez, ey, a2, xiy2, xiz2, xl
    real(kind=8) :: rad, ang, angarc, angs2, xfly, xflz
    real(kind=8) :: pgl(3, 3), pgl1(3, 3), pgl2(3, 3), mat(105)
    real(kind=8) :: iyr2, izr2, xfl, b(14), ksi1, d1b3(2, 3)
    real(kind=8) :: sigma(14), carsec(6)
    character(len=16) :: ch16
!
    integer :: nbfibr, nbgrfi, tygrfi, nbcarm, nug(10)
!
! --------------------------------------------------------------------------------------------------
    integer, parameter :: nb_cara1 = 11
    real(kind=8) :: vale_cara1(nb_cara1)
    character(len=8) :: noms_cara1(nb_cara1)
    data noms_cara1 /'A1','IY1','IZ1','EY1','EZ1','A2','IY2','IZ2','EY2','EZ2','TVAR'/
!
    integer, parameter :: nb_cara2 = 7
    real(kind=8) :: vale_cara2(nb_cara2)
    character(len=8) :: noms_cara2(nb_cara2)
    data noms_cara2 /'A1','IY1','IZ1','EY1','EZ1','IYR21','IZR21'/
! --------------------------------------------------------------------------------------------------
!
    nno = 2
!   Récupération des caractéristiques générales des sections
    if (nomte(1:13).eq.'MECA_POU_D_TG') then
        itype = 0
        nc = 7
        call poutre_modloc('CAGNP1', noms_cara2, nb_cara2, lvaleur=vale_cara2)
        a    = vale_cara2(1)
        xiy  = vale_cara2(2)
        xiz  = vale_cara2(3)
        ey   = vale_cara2(4)
        ez   = vale_cara2(5)
        iyr2 = vale_cara2(6)
        izr2 = vale_cara2(7)
    else
        call poutre_modloc('CAGNPO', noms_cara1, nb_cara1, lvaleur=vale_cara1)
        nc = 6
        a      = vale_cara1(1)
        xiy    = vale_cara1(2)
        xiz    = vale_cara1(3)
        a2     = vale_cara1(6)
        xiy2   = vale_cara1(7)
        xiz2   = vale_cara1(8)
        ey = (vale_cara1(4) +vale_cara1(9))/2.d0
        ez = (vale_cara1(5) +vale_cara1(10))/2.d0
        itype = nint(vale_cara1(11))
    endif
! --------------------------------------------------------------------------------------------------
!   section initiale
    if (nomte .eq. 'MECA_POU_D_TGM') then
!       Récupération des caractéristiques des fibres
        call pmfinfo(nbfibr,nbgrfi,tygrfi,nbcarm,nug)
        call jevech('PFIBRES', 'L', jacf)
!       calcul des caractéristiques
        call pmfitg(tygrfi, nbfibr, nbcarm, zr(jacf), carsec)
        a   = carsec(1)
        xiy = carsec(5)
        xiz = carsec(4)
    endif
! --------------------------------------------------------------------------------------------------
!   Recuperation des orientations
    call jevech('PCAORIE', 'L', lorien)
!   Recuperation des coordonnees des noeuds
    xl =  lonele()
    if (itype .ne. 10) then
!       poutre droite
        call matrot(zr(lorien), pgl)
    else
!       poutre courbe de timoshenko a 6 ddl
        call utmess('F', 'ELEMENTS3_28')
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        angarc = zr(lrcou+1)
        xfl = zr(lrcou+2)
        xfly = xfl
        xflz = xfl
        if ( xfl .le. r8prem() ) then
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
    endif
!
! --------------------------------------------------------------------------------------------------
!   Calcul des matrices elementaires
    mat(:) = 0.0d+0
    if (option .eq. 'RIGI_MECA_GE') then
!       Calcul de la matrice de rigidite geometrique
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
                call ptkg00(sigma, a, a2, xiz, xiz2, xiy, xiy2, xl, ey, ez, mat)
            else
                call utmess('A', 'ELEMENTS3_28')
            endif
!
        else if (nomte.eq.'MECA_POU_D_TG') then
            call jspgno(xl, zr(ldep), b)
            b(1:7) = -b(1:7)
            call ptkg20(b, a, xiz, xiy, iyr2, izr2, xl, ey, ez, mat)
!
        else if (nomte.eq.'MECA_POU_D_TGM') then
!           on projette avec les fcts de forme sur les noeuds debut et fin de l'élément
!           pour le point 1
            ksi1 = -sqrt( 5.d0 / 3.d0 )
            d1b3(1,1) = ksi1*(ksi1-1.d0)/2.0d0
            d1b3(1,2) = 1.d0-ksi1*ksi1
            d1b3(1,3) = ksi1*(ksi1+1.d0)/2.0d0
!           pour le point 2
            ksi1 = sqrt( 5.d0 / 3.d0 )
            d1b3(2,1) = ksi1*(ksi1-1.d0)/2.0d0
            d1b3(2,2) = 1.d0-ksi1*ksi1
            d1b3(2,3) = ksi1*(ksi1+1.d0)/2.0d0
!           On recupere les efforts généralisé cf option SIEF_ELNO
            call jevech('PSTRXRR', 'L', istrxr)
!           nombre de composante par pdg dans le champs PSTRXRR
            ncomp = 18
!           au noeud 1 on récupère   -effort stocke
!           au noeud 2 on récupère   +effort stocke
            do i = 1, nc
                b(i) = 0.0d+0
                b(i+nc) = 0.0d+0
                do kp = 1, 3
                    adr = istrxr+ncomp*(kp-1)+i-1
                    b(i) = b(i) -zr(adr)*d1b3(1,kp)
                    b(i+nc)= b(i+nc)+zr(adr)*d1b3(2,kp)
                enddo
            enddo
            call ptkg20(b, a, xiz, xiy, iyr2, izr2, xl, ey, ez, mat)
        endif
!       Passage du repère local au repère global
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
