subroutine te0151(option, nomte)
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
! aslint: disable=
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/matrot.h"
#include "asterfort/matro2.h"
#include "asterfort/moytem.h"
#include "asterfort/pmfrig.h"
#include "asterfort/pomass.h"
#include "asterfort/porigi.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/ptenci.h"
#include "asterfort/ptenpo.h"
#include "asterfort/ptenth.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/trigom.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "asterfort/vecma.h"
#include "asterfort/verifm.h"
    character(len=*) :: option, nomte
!     ------------------------------------------------------------------
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
    integer :: nbpar, nbres, nc, nno
    integer :: npg, lx
    real(kind=8) :: ang, tvar
    real(kind=8) :: angarc, angs2, deux, e, enerth
    real(kind=8) :: g, rad, rho, un, valpar
    real(kind=8) :: xl, xnu, zero
!-----------------------------------------------------------------------
    parameter    (             nbres = 3 )
    real(kind=8) :: valres(nbres)
    integer :: codres(nbres)
    character(len=3) :: stopz
    character(len=4) :: fami
    character(len=8) :: nompar, famil, poum
    character(len=16) :: ch16, nomres(nbres)
    real(kind=8) :: ul(14), ug(14), pgl(3, 3), klc(14, 14), klv(105)
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), epsthe(1)
    integer :: kpg, spt, nklv
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
    do i = 1, nbres
        valres(i) = zero
    end do
!
    fami = 'RIGI'
    npg = 3
    istruc = 1
    nno = 2
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
                ' ', 'ELAS', nbpar, nompar, [valpar],&
                2, nomres, valres, codres, 1)
    call rcvalb(famil, kpg, spt, poum, zi(lmater),&
                ' ', 'ELAS', nbpar, nompar, [valpar],&
                1, nomres(3), valres(3), codres(3), 1)
!
    e = valres(1)
    xnu = valres(2)
    rho = valres(3)
    g = e / ( deux * ( un + xnu ) )
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
    call jevech('PCAORIE', 'L', lorien)
    call matrot(zr(lorien), pgl)
    call lonele(3, lx, xl)
    call poutre_modloc('CAGNPO', ['TVAR'], 1, valeur=tvar)
    itype = nint(tvar)
!
    nc = 6
    if ( nomte(1:13) .eq. 'MECA_POU_D_TG') nc = 7

    if (nomte .eq. 'MECA_POU_C_T') then
!        --- POUTRE COURBE DE TIMOSKENKO A 6 DDL ---
        nno = 1
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        angarc = zr(lrcou+1)
        angs2 = trigom('ASIN',xl/ (deux*rad))
        ang = angs2 * deux
        xl = rad * ang
        call matro2(zr(lorien), angarc, angs2, pgl1, pgl2)
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
        stopz='ONO'
        call tecach(stopz, 'PVITESR', 'L', iret, iad=jvite)
! IRET NE PEUT VALOIR QUE 0 (TOUT EST OK) OU 2 (CHAMP NON FOURNI)
        if (iret .eq. 0) then
            do 21 i = 1, 2*nc
                ug(i) = zr(jvite+i-1)
21          continue
        else
            call tecach(stopz, 'PDEPLAR', 'L', iret, iad=jdepl)
            if (iret .eq. 0) then
                do 22 i = 1, 2*nc
                    ug(i) = zr(jdepl+i-1)
22              continue
            else
                call utmess('F', 'ELEMENTS2_1', sk=option)
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
        if ((nomte.eq.'MECA_POU_D_EM') .or. ( nomte.eq.'MECA_POU_D_TGM')) then
            call pmfrig(nomte, zi(lmater), klv)
        else
            call porigi(nomte, e, xnu, xl, klv)
        endif
!
!        ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
        call vecma(klv, nklv, klc, 2*nc)
!        --- ENERGIE DE DEFORMATION
        if = 1
        call ptenpo(nc*2, ul, klc, zr(jende), itype,&
                    if)
        if (epsthe(1) .ne. zero) then
            call ptenth(ul, xl, epsthe(1), 2*nc, klc,&
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
            call pomass(nomte, e, xnu, rho, kanl,&
                  klv)
!
!           ---- MATRICE MASSE LIGNE > MATRICE MASSE CARRE
            call vecma(klv, 78, klc, 12)
!           --- ENERGIE CINETIQUE
            if = 1
            call ptenci(12, ul, klc, zr(jfreq), zr(jende),&
                        itype, kanl, if)
!
        else if (codres(3).ne.0) then
            call utmess('F', 'ELEMENTS3_31')
        endif
!
    else
        ch16 = option
        call utmess('F', 'ELEMENTS2_47', sk=ch16)
    endif
!
end subroutine
