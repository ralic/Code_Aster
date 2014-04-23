subroutine te0154(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/matrot.h"
#include "asterfort/pmavec.h"
#include "asterfort/ptenci.h"
#include "asterfort/ptenpo.h"
#include "asterfort/ptenth.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "asterfort/verift.h"
    character(len=*) :: option, nomte
! ----------------------------------------------------------------------
!     CALCUL
!       - DU VECTEUR ELEMENTAIRE EFFORT GENERALISE,
!       - DU VECTEUR ELEMENTAIRE CONTRAINTE
!       - DE L'ENERGIE DE DEFORMATION
!       - DE L'ENERGIE CINETIQUE
!     POUR LES ELEMENTS DE BARRE
! ----------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        'SIEF_ELGA'   : CALCUL DU VECTEUR EFFORT GENERALISE
!        'EPSI_ELGA'   : CALCUL DU VECTEUR DEFORMATION
!        'EPOT_ELEM'   : CALCUL DE L'ENERGIE DE DEFORMATION
!        'ECIN_ELEM'   : CALCUL DE L'ENERGIE CINETIQUE
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!        'MECA_BARRE'   : BARRE
!        'MECA_2D_BARRE'   : BARRE
!
!
    real(kind=8) :: pgl(3, 3), klc(6, 6), enerth
    real(kind=8) :: ugr(6), ulr(12), flr(6)
    integer :: codres(1)
    character(len=3) :: stopz
    character(len=4) :: fami
    character(len=8) :: nomail
    character(len=16) :: ch16
    logical :: lteimp
    real(kind=8) :: a, epsth, e(1), r8bid=0.d0, rho(1), xfl1, xfl4, xl, xmas, xrig
    integer :: i, if, itype, j, jdepl, jeffo, jende, jfreq, jdefo, kanl
    integer :: lmater, lorien, lsect, iret, lx, nc, nno, iadzi, iazk24
    integer :: jvite
!     ------------------------------------------------------------------
!
    lteimp = .false.
    nno = 2
    nc = 3
    fami = 'RIGI'
!
    if ((nomte .ne. 'MECA_BARRE') .and. (nomte .ne. 'MECA_2D_BARRE')) then
        ch16 = nomte
        call utmess('F', 'ELEMENTS2_42', sk=ch16)
    endif
!
!     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
    call jevech('PMATERC', 'L', lmater)
!
    call verift(fami, 1, 1, '+', zi(lmater),&
                epsth=epsth)
!
    r8bid = 0.0d0
    call rcvalb(fami, 1, 1, '+', zi(lmater),&
                ' ', 'ELAS', 0, ' ', [r8bid],&
                1, 'E', e, codres, 1)
    if (epsth .ne. 0.d0) lteimp =.true.
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
!
    if (nomte .eq. 'MECA_BARRE') then
!
        call lonele(zr(lx), 3, xl)
!
    else if (nomte.eq.'MECA_2D_BARRE') then
        call lonele(zr(lx), 2, xl)
!
    endif
!
    if (xl .eq. 0.d0) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call utmess('F', 'ELEMENTS2_43', sk=nomail)
    endif
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
    if (option .ne. 'EPSI_ELGA') then
        call jevech('PCAGNBA', 'L', lsect)
        a = zr(lsect)
    endif
!
!     --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA ---
    call jevech('PCAORIE', 'L', lorien)
!     --- MATRICE DE ROTATION PGL
    call matrot(zr(lorien), pgl)
!
!     --- RECUPERATION DES DEPLACEMENTS OU DES VITESSES ----
    do i = 1, 6
        ugr(i) = 0.d0
    end do
!
    if (option .ne. 'ECIN_ELEM') then
!
! ON RECUPERE DES DEPLACEMENTS
!
        call jevech('PDEPLAR', 'L', jdepl)
        if (nomte .eq. 'MECA_BARRE') then
            do i = 1, 6
                ugr(i) = zr(jdepl+i-1)
            end do
        else if (nomte.eq.'MECA_2D_BARRE') then
            ugr(1) = zr(jdepl+1-1)
            ugr(2) = zr(jdepl+2-1)
            ugr(4) = zr(jdepl+3-1)
            ugr(5) = zr(jdepl+4-1)
        endif
!
    else
!
        stopz='ONO'
        call tecach(stopz, 'PVITESR', 'L', iret, iad=jvite)
! IRET NE PEUT VALOIR QUE 0 (TOUT EST OK) OU 2 (CHAMP NON FOURNI)
        if (iret .eq. 0) then
!
! ON RECUPERE DES VITESSES
!
            if (nomte .eq. 'MECA_BARRE') then
                do i = 1, 6
                    ugr(i) = zr(jvite+i-1)
                end do
            else if (nomte.eq.'MECA_2D_BARRE') then
                ugr(1) = zr(jvite+1-1)
                ugr(2) = zr(jvite+2-1)
                ugr(4) = zr(jvite+3-1)
                ugr(5) = zr(jvite+4-1)
            endif
!
        else
!
! ON RECUPERE DES DEPLACEMENTS
!
            call tecach(stopz, 'PDEPLAR', 'L', iret, iad=jdepl)
            if (iret .eq. 0) then
                if (nomte .eq. 'MECA_BARRE') then
                    do i = 1, 6
                        ugr(i) = zr(jdepl+i-1)
                    end do
                else if (nomte.eq.'MECA_2D_BARRE') then
                    ugr(1) = zr(jdepl+1-1)
                    ugr(2) = zr(jdepl+2-1)
                    ugr(4) = zr(jdepl+3-1)
                    ugr(5) = zr(jdepl+4-1)
                endif
            else
                call utmess('F', 'ELEMENTS2_1', sk=option)
            endif
!
        endif
!
    endif
!
!     --- VECTEUR DANS REPERE LOCAL  ULR = PGL * UGR
!
    call utpvgl(nno, nc, pgl, ugr, ulr)
!
!     --- RIGIDITE ELEMENTAIRE ---
    do i = 1, 6
        do j = 1, 6
            klc(i,j) = 0.d0
        end do
    end do
!
!     --- ENERGIE DE DEFORMATION ----
    if (option .eq. 'EPOT_ELEM') then
        call jevech('PENERDR', 'E', jende)
        xrig = e(1) * a / xl
        klc(1,1) = xrig
        klc(1,4) = -xrig
        klc(4,1) = -xrig
        klc(4,4) = xrig
        if = 0
        call ptenpo(6, ulr, klc, zr(jende), if,&
                    if)
!
        if (lteimp) then
            call ptenth(ulr, xl, epsth, 6, klc,&
                        if, enerth)
            zr(jende) = zr(jende) - enerth
        endif
!
    else if (option .eq. 'ECIN_ELEM') then
        call rcvalb(fami, 1, 1, '+', zi(lmater),&
                    ' ', 'ELAS', 0, ' ', [r8bid],&
                    1, 'RHO', rho, codres, 1)
        call jevech('PENERCR', 'E', jende)
        call jevech('POMEGA2', 'L', jfreq)
        xmas = rho(1) * a * xl / 6.d0
        klc(1,1) = xmas * 2.d0
        klc(2,2) = xmas * 2.d0
        klc(3,3) = xmas * 2.d0
        klc(4,4) = xmas * 2.d0
        klc(5,5) = xmas * 2.d0
        klc(6,6) = xmas * 2.d0
        klc(1,4) = xmas
        klc(4,1) = xmas
        klc(2,5) = xmas
        klc(5,2) = xmas
        klc(3,6) = xmas
        klc(6,3) = xmas
        if = 0
        itype = 50
        kanl = 1
        call ptenci(6, ulr, klc, zr(jfreq), zr(jende),&
                    itype, kanl, if)
!
!
    else if (option .eq. 'EPSI_ELGA') then
        call jevech('PDEFOPG', 'E', jdefo)
        zr(jdefo-1+1)=(ulr(4)-ulr(1))/xl
    else
        xrig = e(1) * a / xl
        klc(1,1) = xrig
        klc(1,4) = -xrig
        klc(4,1) = -xrig
        klc(4,4) = xrig
!
!
!        --- VECTEUR EFFORT LOCAL  FLR = KLC * ULR
        call pmavec('ZERO', 6, klc, ulr, flr)
!
!        --- TENIR COMPTE DES EFFORTS DUS A LA DILATATION ---
        if (lteimp) then
!
!              --- CALCUL DES FORCES INDUITES ---
            xfl1 = -epsth * e(1) * a
            xfl4 = -xfl1
            flr(1) = flr(1) - xfl1
            flr(4) = flr(4) - xfl4
        endif
!
        if (option .eq. 'SIEF_ELGA') then
            call jevech('PCONTRR', 'E', jeffo)
            zr(jeffo ) = -flr(1)
!
        else
! OPTION NON PROGRAMMEE
            ASSERT(.false.)
        endif
    endif
!
end subroutine
