subroutine te0144(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/matro2.h"
#include "asterfort/matrot.h"
#include "asterfort/moytem.h"
#include "asterfort/pmavec.h"
#include "asterfort/porigi.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/rcvalb.h"
#include "asterfort/trigom.h"
#include "asterfort/utpvgl.h"
#include "asterfort/vecma.h"
#include "asterfort/verifm.h"
    character(len=*) :: option, nomte
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     CALCUL
!       - DU VECTEUR ELEMENTAIRE EFFORT GENERALISE,
!       - DU VECTEUR ELEMENTAIRE CONTRAINTE
!     POUR LES ELEMENTS DE POUTRE D'EULER ET DE TIMOSHENKO.
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        'SIEF_ELGA'
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!        'MECA_POU_D_E' : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!        'MECA_POU_D_T' : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!        'MECA_POU_C_T' : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
!     ------------------------------------------------------------------
!
    real(kind=8) :: zero, deux
    parameter  (zero = 0.d0,deux = 2.d0)
!
    integer :: nbres, npg, nno, nc, nnoc, ncc, jeffo, lmater, iret
    integer :: iret1, itype, lx, lrcou, lorien, jdepl, i, j
    parameter    (nbres=2)
    real(kind=8) :: valres(nbres)
    integer :: codres(nbres)
    character(len=16) :: nomres(nbres)
!
    real(kind=8) :: ul(12), ug(12), pgl(3, 3), klc(12, 12), klv(78)
    real(kind=8) :: fl(12), pgl1(3, 3), pgl2(3, 3), epsith(1)
    real(kind=8) :: x, temp, tvar
    real(kind=8) :: e, xnu, xl, rad, angarc, angs2, along
!
!
!     ------------------------------------------------------------------
    data nomres / 'E', 'NU'/
!     ------------------------------------------------------------------
    nno = 2
    nc = 6
    nnoc = 1
    ncc = 6
!     ------------------------------------------------------------------
!
    if (option .eq. 'SIEF_ELGA') then
        call jevech('PCONTRR', 'E', jeffo)
    else
! OPTION NON PROGRAMMEE
        ASSERT(.false.)
    endif
!
! --- POINT DE GAUSS DE L'ELEMENT
    call elrefe_info(fami='RIGI',npg=npg)
    ASSERT((npg.eq.2).or.(npg.eq.3))
!
! --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
    call jevech('PMATERC', 'L', lmater)
!
! --- RECUPERATION DE LA TEMPERATURE :
    call verifm('RIGI', npg, 1, '+', zi(lmater),&
                'ELAS', 1, epsith, iret)
    call moytem('RIGI', npg, 1, '+', temp,&
                iret1)
!
    call rcvalb('RIGI', 1, 1, '+', zi(lmater),&
                ' ', 'ELAS', 1, 'TEMP', [temp],&
                2, nomres, valres, codres, 1)
    e = valres(1)
    xnu = valres(2)
!
!     --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE ---
!
    call porigi(nomte, e, xnu, -1.d0, klv)
!
!     ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
!
    call vecma(klv, 78, klc, 12)
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
!
    call poutre_modloc('CAGNPO', ['TVAR'], 1, valeur=tvar)
    itype = nint(tvar)
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
!
    call lonele(3, lx, xl)
    if (itype .eq. 10) then
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        angarc = zr(lrcou+1)
        x = xl / ( deux * rad )
        angs2 = trigom('ASIN', x )
        xl = rad * angs2 * deux
    endif
!
!     --- MATRICE DE ROTATION PGL
!
    call jevech('PCAORIE', 'L', lorien)
    if (itype .eq. 10) then
        call matro2(zr(lorien), angarc, angs2, pgl1, pgl2)
    else
        call matrot(zr(lorien), pgl)
    endif
!
    call jevech('PDEPLAR', 'L', jdepl)
    do i = 1, 12
        ug(i) = zr(jdepl+i-1)
    end do
!
!      --- VECTEUR DEPLACEMENT LOCAL  UL = PGL * UG
    if (itype .eq. 10) then
        call utpvgl(nnoc, ncc, pgl1, ug, ul)
        call utpvgl(nnoc, ncc, pgl2, ug(7), ul(7))
    else
        call utpvgl(nno, nc, pgl, ug, ul)
    endif
!
!     --- VECTEUR EFFORT       LOCAL  FL = KLC * UL
    call pmavec('ZERO', 12, klc, ul, fl)
!
!     --- TENIR COMPTE DES EFFORTS DUS A LA DILATATION ---
    if (epsith(1) .ne. zero) then
        do i = 1, 12
            ug(i) = zero
        enddo
!
        if (itype .ne. 10) then
            ug(1) = -epsith(1) * xl
            ug(7) = -ug(1)
        else
            along = 2.d0 * rad * epsith(1) * sin(angs2)
            ug(1) = -along * cos(angs2)
            ug(2) = along * sin(angs2)
            ug(7) = -ug(1)
            ug(8) = ug(2)
        endif
!
!              --- CALCUL DES FORCES INDUITES ---
        do i = 1, 6
            do j = 1, 6
                fl(i) = fl(i) - klc(i,j) * ug(j)
                fl(i+6) = fl(i+6) - klc(i+6,j+6) * ug(j+6)
            enddo
        enddo
    endif
!
! --- ARCHIVAGE ---
    if (npg .eq. 2) then
        do i = 1, 6
            zr(jeffo+i-1) = -fl(i)
            zr(jeffo+i+6-1) = fl(i+6)
        enddo
    else
!        DANS LE CAS 3 POINTS DE GAUSS
!        IL NE FAUT PAS METTRE 0 SUR UN DES POINTS DE GAUSS
!        CF DOC ASTER POUR LA NUMEROTATION OU ELREGA
!              NOEUD        N1       N2       N3
!              POSITION   (-0.7777 , 0.0000 , 0.7777)
!        C'EST LINEAIRE : (N2) = ( (N1) + (N3) )/2
        do i = 1, 6
            zr(jeffo+i-1) = -fl(i)
            zr(jeffo+i+6-1) = (-fl(i) + fl(i+6))*0.5d0
            zr(jeffo+i+12-1) = fl(i+6)
        enddo
    endif
!
end subroutine
