subroutine te0151(option, nomte)
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
!     CALCUL
!       - ENERGIE DE DEFORMATION
!       - ENERGIE CINETIQUE
!     POUR LES ELEMENTS DE POUTRE D'EULER ET DE TIMOSHENKO.
!
! --------------------------------------------------------------------------------------------------
!
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
!
! --------------------------------------------------------------------------------------------------
!
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
!
    character(len=*) :: option, nomte
! --------------------------------------------------------------------------------------------------
    integer :: i, idis, iret, istruc, itype, jdepl, jende
    integer :: jfreq, jmasd, jvite, kanl, lmater, lorien, lrcou
    integer :: nbpar, nbres, nc, nno
    integer :: npg
    real(kind=8) :: ang, tvar
    real(kind=8) :: angarc, angs2, e, enerth
    real(kind=8) :: g, rad, rho, valpar
    real(kind=8) :: xl, xnu
! --------------------------------------------------------------------------------------------------
    character(len=3) :: stopz
    character(len=4) :: fami
    character(len=8) :: nompar, famil, poum
    character(len=16) :: ch16
    real(kind=8) :: ul(14), ug(14), pgl(3, 3), klc(14, 14), klv(105)
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), epsthe(1)
    integer :: kpg, spt, nklv
! --------------------------------------------------------------------------------------------------
    parameter    (nbres = 3 )
    integer :: codres(nbres)
    real(kind=8) :: valres(nbres)
    character(len=16) :: nomres(nbres)
    data nomres / 'E' , 'NU' , 'RHO' /
! --------------------------------------------------------------------------------------------------
!
    call jevech('PMATERC', 'L', lmater)
    valres(:) = 0.0d0
!
    fami = 'RIGI'
    npg = 3
    istruc = 1
    nno = 2
    if ((nomte.eq.'MECA_POU_C_T') .or. (nomte.eq.'MECA_POU_D_EM')) npg = 2
!
    call moytem(fami, npg, 1, '+', valpar, iret)
    call verifm(fami, npg, 1, '+', zi(lmater), 'ELAS', 1, epsthe, iret)
    nbpar = 1
    nompar = 'TEMP'
    famil='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call rcvalb(famil, kpg, spt, poum, zi(lmater), ' ', 'ELAS', nbpar, nompar, [valpar],&
                2, nomres, valres, codres, 1)
    call rcvalb(famil, kpg, spt, poum, zi(lmater), ' ', 'ELAS', nbpar, nompar, [valpar],&
                1, nomres(3), valres(3), codres(3), 1)
!
    e = valres(1)
    xnu = valres(2)
    rho = valres(3)
    g = e / ( 2.0d0 * ( 1.0d0 + xnu ) )
!
!   recuperation des caracteristiques generales des sections
    call jevech('PCAORIE', 'L', lorien)
    call matrot(zr(lorien), pgl)
    xl =  lonele()
    call poutre_modloc('CAGNPO', ['TVAR'], 1, valeur=tvar)
    itype = nint(tvar)
!
    nc = 6
    if ( nomte(1:13) .eq. 'MECA_POU_D_TG') nc = 7
    if (nomte .eq. 'MECA_POU_C_T') then
!       poutre courbe de timoskenko a 6 DDL
        nno = 1
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        angarc = zr(lrcou+1)
        angs2 = trigom('ASIN',xl/ (2.0d0*rad))
        ang = angs2 * 2.0d0
        xl = rad * ang
        call matro2(zr(lorien), angarc, angs2, pgl1, pgl2)
    endif
!
    nklv = 2*nc*(2*nc+1)/2
    if (option .ne. 'ECIN_ELEM') then
        call jevech('PDEPLAR', 'L', jdepl)
        do i = 1, 2*nc
            ug(i) = zr(jdepl+i-1)
        enddo
    else
        stopz='ONO'
        call tecach(stopz, 'PVITESR', 'L', iret, iad=jvite)
!       iret ne peut valoir que 0 (tout est ok) ou 2 (champ non fourni)
        if (iret .eq. 0) then
            do i = 1, 2*nc
                ug(i) = zr(jvite+i-1)
            enddo
        else
            call tecach(stopz, 'PDEPLAR', 'L', iret, iad=jdepl)
            if (iret .eq. 0) then
                do i = 1, 2*nc
                    ug(i) = zr(jdepl+i-1)
                enddo
            else
                call utmess('F', 'ELEMENTS2_1', sk=option)
            endif
        endif
    endif
!
!   matrice de rotation PGL. vecteur deplacement ou vitesse local  ul = pgl * ug
    if (itype .eq. 10) then
        call utpvgl(nno, nc, pgl1, ug, ul)
        call utpvgl(nno, nc, pgl2, ug(7), ul(7))
    else
        call utpvgl(nno, nc, pgl, ug, ul)
    endif
!
!   energie de deformation
    if (option .eq. 'EPOT_ELEM') then
        call jevech('PENERDR', 'E', jende)
!       calcul de la matrice de rigidite locale
        if ((nomte.eq.'MECA_POU_D_EM') .or. ( nomte.eq.'MECA_POU_D_TGM')) then
            call pmfrig(nomte, zi(lmater), klv)
        else
            call porigi(nomte, e, xnu, xl, klv)
        endif
!       matrice rigidite ligne > matrice rigidite carre
        call vecma(klv, nklv, klc, 2*nc)
!        energie de deformation
        idis = 1
        call ptenpo(nc*2, ul, klc, zr(jende), itype, idis)
        if (epsthe(1) .ne. 0.0d0) then
            call ptenth(ul, xl, epsthe(1), 2*nc, klc, itype, enerth)
            zr(jende) = zr(jende) - enerth
        endif
!
    else if (option .eq. 'ECIN_ELEM') then
!       energie cinetique
        call jevech('PENERCR', 'E', jende)
        call jevech('PMASDIA', 'L', jmasd)
        call jevech('POMEGA2', 'L', jfreq)
        kanl = zi(jmasd)
!        calcul de la matrice de masse locale
        if (rho .ne. 0.0d0) then
!           KANL = 0 ou 1. masses concentrees ou coherentes
            call pomass(nomte, e, xnu, rho, kanl, klv)
!           matrice masse ligne > matrice masse carre
            call vecma(klv, 78, klc, 12)
!           energie cinetique
            idis = 1
            call ptenci(12, ul, klc, zr(jfreq), zr(jende), itype, kanl, idis)
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
