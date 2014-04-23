subroutine te0155(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/angvx.h"
#include "asterfort/assert.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/provec.h"
#include "asterfort/pscvec.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
#include "asterfort/verift.h"
#include "blas/ddot.h"
    character(len=*) :: option, nomte
! ----------------------------------------------------------------------
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
!     1- CALCUL FORCES ELEMENTAIRES LINEIQUES
!     2- CALCULE LE CHARGEMENT INDUIT PAR UNE ELEVATION UNIFORME DE
!        TEMPERATURE
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        'CHAR_MECA_PESA_R' : CHARGES DE PESANTEUR
!        'CHAR_MECA_FR1D1D' : FORCES LINEIQUES (REEL)
!        'CHAR_MECA_FF1D1D' : FORCES LINEIQUES (FONCTION)
!        'CHAR_MECA_SR1D1D' : FORCES LINEIQUES SUIVEUSES (FONCTION)
!        'CHAR_MECA_TEMP_R' : ELEVATION DE TEMPERATURE
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!        'MECA_BARRE'       : BARRE
!        'MECA_2D_BARRE'       : BARRE
!
!
    integer :: codres(1)
    character(len=4) :: fami
    character(len=8) :: nompar(4), poum
    real(kind=8) :: a, e(1), rho(1), xl, temp, xdep, xrig, w(6), w2(3)
    real(kind=8) :: pgl(3, 3), fl(6), qg(6), ql(6), valpa1(4), valpa2(4)
    real(kind=8) :: r8min, s, s2, s3, s4, s5, r1, vect(6)
    integer :: nno, nc, lx, lorien, idepla, ideplp, i, lvect, lsect
    integer :: lmater, lpesa, lforc, itemps, nbpar, iret
    integer :: ifcx, iadzi, iazk24, kpg, spt
    character(len=8) :: nompav(1), nomail
    real(kind=8) :: valpav(1), fcx, vite2, vp(3), ang1(3), u(3), v(3), instan
    logical :: normal, global, okvent
!
    real(kind=8) :: kendog(1), kdessi(1), sech, hydr
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iret1
    real(kind=8) :: epsth, sref
!-----------------------------------------------------------------------
    data         nompar / 'X' , 'Y' , 'Z' , 'INST' /
    data         nompav /'VITE'/
!     ------------------------------------------------------------------
    r8min = r8miem()
!
    nno = 2
    nc = 3
    fami = 'RIGI'
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
!
!     --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA ---
    call jevech('PCAORIE', 'L', lorien)
!     --- MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE LOCAL: PGL ---
!
    if (option .eq. 'CHAR_MECA_SR1D1D' .or. option .eq. 'CHAR_MECA_SF1D1D') then
!          ------------------------------
        call jevech('PDEPLMR', 'L', idepla)
        call jevech('PDEPLPR', 'L', ideplp)
        if (nomte .eq. 'MECA_BARRE') then
            do i = 1, 3
                w(i) = zr(lx+i) + zr(idepla-1+i) + zr(ideplp-1+i)
                w(i+3) = zr(lx+i+3) + zr(idepla+2+i) + zr(ideplp+2+i)
                w2(i) = w(i+3) - w(i)
            end do
        else if (nomte.eq.'MECA_2D_BARRE') then
            do i = 1, 2
                w(i) = zr(lx+i) + zr(idepla-1+i) + zr(ideplp-1+i)
                w(i+2) = zr(lx+i+2) + zr(idepla+1+i) + zr(ideplp+1+i)
                w2(i) = w(i+2) - w(i)
            end do
        endif
        call angvx(w2, ang1(1), ang1(2))
        ang1(3) = zr(lorien+2)
        call matrot(ang1, pgl)
    else
        if (nomte .eq. 'MECA_BARRE') then
            do i = 1, 3
                w(i) = zr(lx+i)
                w(i+3) = zr(lx+i+3)
                w2(i) = w(i+3) - w(i)
            end do
        else if (nomte.eq.'MECA_2D_BARRE') then
            do i = 1, 2
                w(i) = zr(lx+i)
                w(i+2) = zr(lx+i+2)
                w2(i) = w(i+2) - w(i)
            end do
        endif
        call matrot(zr(lorien), pgl)
    endif
    if (nomte .eq. 'MECA_BARRE') then
        s=ddot(3,w2,1,w2,1)
    else if (nomte.eq.'MECA_2D_BARRE') then
        s=ddot(2,w2,1,w2,1)
    endif
    xl = sqrt(s)
    if (xl .eq. 0.d0) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call utmess('F', 'ELEMENTS2_43', sk=nomail)
    endif
!
!     --- INITIALISATION DE FL ---
    do i = 1, 6
        fl(i) = 0.d0
    end do
!
    call jevech('PVECTUR', 'E', lvect)
!
    if (option .eq. 'CHAR_MECA_PESA_R') then
!
!     --- CAS DE CHARGE DE PESANTEUR ---
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
        call jevech('PCAGNBA', 'L', lsect)
        a = zr(lsect)
!        --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
        call jevech('PMATERC', 'L', lmater)
        kpg=1
        spt=1
        poum='+'
        call rcvalb('FPG1', kpg, spt, poum, zi(lmater),&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    1, 'RHO', rho, codres, 1)
!
        call jevech('PPESANR', 'L', lpesa)
        do i = 1, 3
            qg(i) = rho(1) * zr(lpesa) * zr(lpesa+i)
            qg(i+3) = qg(i)
        end do
!
!        --- PASSAGE REPERE LOCAL DU VECTEUR FORCE ---
        call utpvgl(nno, nc, pgl, qg(1), ql(1))
!
!        --- CALCUL DES FORCES NODALES EQUIVALENTES EN REPERE LOCAL ---
!         FL(1) = QL(1) * A * XL / 2.D0
!         FL(4) = QL(4) * A * XL / 2.D0
        do i = 1, 6
            fl(i)= ql(i) * a * xl / 2.d0
        end do
        call utpvlg(nno, nc, pgl, fl(1), vect)
    endif
!
! ECRITURE DANS LE VECTEUR PVECTUR SUIVANT L'ELEMENT
!
    if (nomte .eq. 'MECA_BARRE') then
        do i = 1, 6
            zr(lvect+i-1) = vect(i)
        end do
    else if (nomte.eq.'MECA_2D_BARRE') then
        zr(lvect) = vect(1)
        zr(lvect+1) = vect(2)
        zr(lvect+2) = vect(4)
        zr(lvect+3) = vect(5)
    endif
!
    okvent = .false.
    if (option .eq. 'CHAR_MECA_FR1D1D' .or. option .eq. 'CHAR_MECA_SR1D1D') then
!          ------------------------------
!        POUR LE CAS DU VENT
        call tecach('NNN', 'PVITER', 'L', iret, iad=lforc)
        if (lforc .ne. 0) then
            if (nomte .eq. 'MECA_2D_BARRE') then
! OPTION NON PROGRAMMEE
                ASSERT(.false.)
            endif
            normal = .true.
            okvent = .true.
            global = .true.
        else
            call jevech('PFR1D1D', 'L', lforc)
            r1 = abs(zr(lforc+3))
            global = r1 .lt. 1.d-3
            normal = r1 .gt. 1.001d0
        endif
        elseif ( option .eq. 'CHAR_MECA_FF1D1D' .or. option .eq.&
    'CHAR_MECA_SF1D1D' ) then
!              ------------------------------
        call jevech('PFF1D1D', 'L', lforc)
        normal = zk8(lforc+3) .eq. 'VENT'
        global = zk8(lforc+3) .eq. 'GLOBAL'
        call tecach('NNN', 'PTEMPSR', 'L', iret, iad=itemps)
        if (itemps .ne. 0) then
            valpa1(4) = zr(itemps)
            valpa2(4) = zr(itemps)
            nbpar = 4
        else
            nbpar = 3
        endif
    endif
!
    if (option .eq. 'CHAR_MECA_FR1D1D') then
!     --- FORCES REPARTIES PAR VALEURS REELLES---
        do i = 1, 3
            qg(i) = zr(lforc+i-1)
            qg(i+3) = qg(i)
        end do
        if (normal) then
            s=ddot(3,w2,1,w2,1)
            s2=1.d0/s
            s=ddot(3,qg(1),1,qg(1),1)
            s4 = sqrt(s)
            if (s4 .gt. r8min) then
                call provec(w2, qg(1), u)
                s=ddot(3,u,1,u,1)
                s3 = sqrt(s)
                s5 = s3*sqrt(s2)/s4
                call provec(u, w2, v)
                call pscvec(3, s2, v, u)
                call pscvec(3, s5, u, qg(1))
            endif
            s=ddot(3,qg(4),1,qg(4),1)
            s4 = sqrt(s)
            if (s4 .gt. r8min) then
                call provec(w2, qg(4), u)
                s=ddot(3,u,1,u,1)
                s3 = sqrt(s)
                s5 = s3*sqrt(s2)/s4
                call provec(u, w2, v)
                call pscvec(3, s2, v, u)
                call pscvec(3, s5, u, qg(4))
            endif
        endif
        if (global .or. normal) then
            call utpvgl(nno, nc, pgl, qg(1), ql(1))
        else
            do i = 1, 6
                ql(i) = qg(i)
            end do
        endif
!
!        --- CALCUL DES FORCES NODALES EQUIVALENTES EN REPERE LOCAL ---
!         FL(1) = QL(1) * XL / 2.D0
!         FL(4) = QL(4) * XL / 2.D0
        do i = 1, 6
            fl(i)= ql(i) * xl / 2.d0
        end do
        call utpvlg(nno, nc, pgl, fl(1), vect)
!
! ECRITURE DANS LE VECTEUR PVECTUR SUIVANT L'ELEMENT
!
        if (nomte .eq. 'MECA_BARRE') then
            do i = 1, 6
                zr(lvect+i-1) = vect(i)
            end do
        else if (nomte.eq.'MECA_2D_BARRE') then
            zr(lvect) = vect(1)
            zr(lvect+1) = vect(2)
            zr(lvect+2) = vect(4)
            zr(lvect+3) = vect(5)
        endif
!
        elseif ( option .eq.'CHAR_MECA_FF1D1D' .or. option&
    .eq.'CHAR_MECA_SF1D1D' ) then
!     --- FORCES REPARTIES PAR FONCTIONS ---
        do i = 1, 3
            valpa1(i) = w(i)
            valpa2(i) = w(i+3)
        end do
        do i = 1, 3
            call fointe('FM', zk8(lforc+i-1), nbpar, nompar, valpa1,&
                        qg(i), iret)
            call fointe('FM', zk8(lforc+i-1), nbpar, nompar, valpa2,&
                        qg(i+ 3), iret)
        end do
!
        if (normal) then
            s=ddot(3,w2,1,w2,1)
            s2=1.d0/s
            s=ddot(3,qg(1),1,qg(1),1)
            s4 = sqrt(s)
            if (s4 .gt. r8min) then
                call provec(w2, qg(1), u)
                s=ddot(3,u,1,u,1)
                s3 = sqrt(s)
                s5 = s3*sqrt(s2)/s4
                call provec(u, w2, v)
                call pscvec(3, s2, v, u)
                call pscvec(3, s5, u, qg(1))
            endif
!
            s=ddot(3,qg(4),1,qg(4),1)
            s4 = sqrt(s)
            if (s4 .gt. r8min) then
                call provec(w2, qg(4), u)
                s=ddot(3,u,1,u,1)
                s3 = sqrt(s)
                s5 = s3*sqrt(s2)/s4
                call provec(u, w2, v)
                call pscvec(3, s2, v, u)
                call pscvec(3, s5, u, qg(4))
            endif
        endif
        if (global .or. normal) then
            call utpvgl(nno, nc, pgl, qg(1), ql(1))
        else
            do i = 1, 6
                ql(i) = qg(i)
            end do
        endif
!
!        --- CALCUL DES FORCES NODALES EQUIVALENTES EN REPERE LOCAL ---
!         FL(1) = QL(1) * XL / 2.D0
!         FL(4) = QL(4) * XL / 2.D0
        do i = 1, 6
            fl(i)= ql(i) * xl / 2.d0
        end do
        call utpvlg(nno, nc, pgl, fl(1), vect)
!
! ECRITURE DANS LE VECTEUR PVECTUR SUIVANT L'ELEMENT
!
        if (nomte .eq. 'MECA_BARRE') then
            do i = 1, 6
                zr(lvect+i-1) = vect(i)
            end do
        else if (nomte.eq.'MECA_2D_BARRE') then
            zr(lvect) = vect(1)
            zr(lvect+1) = vect(2)
            zr(lvect+2) = vect(4)
            zr(lvect+3) = vect(5)
        endif
!
!
    else if (option.eq.'CHAR_MECA_SR1D1D') then
!     --- FORCES SUIVEUSES REPARTIES PAR VALEURS REELLES---
!
!        SEUL LE CAS DU VENT DONNE PAR 'PVITER' EST ACCEPTE
        if (.not. okvent) goto 998
!        RECUPERATION DE LA VITESSE DE VENT RELATIVE AUX NOEUDS
        do i = 1, 6
            qg(i)=zr(lforc-1+i)
        end do
        do i = 1, 3
            vp(i)=0.d0
        end do
!
!        CALCUL DU VECTEUR VITESSE PERPENDICULAIRE
        s=ddot(3,w2,1,w2,1)
        s2=1.d0/s
!
        s=ddot(3,qg(1),1,qg(1),1)
        s4 = sqrt(s)
        fcx = 0.0d0
        if (s4 .gt. r8min) then
            call provec(w2, qg(1), u)
            call provec(u, w2, v)
            call pscvec(3, s2, v, vp)
!          NORME DE LA VITESSE PERPENDICULAIRE
            vite2=ddot(3,vp,1,vp,1)
            valpav(1) = sqrt( vite2 )
            if (valpav(1) .gt. r8min) then
!            RECUPERATION DE L'EFFORT EN FONCTION DE LA VITESSE
                call tecach('ONN', 'PVENTCX', 'L', iret, iad=ifcx)
                if (iret .ne. 0) goto 999
                if (zk8(ifcx)(1:1) .eq. '.') goto 999
                call fointe('FM', zk8(ifcx), 1, nompav, valpav,&
                            fcx, iret)
                fcx = fcx / valpav(1)
            endif
        endif
        call pscvec(3, fcx, vp, qg(1))
!
        s=ddot(3,qg(4),1,qg(4),1)
        s4 = sqrt(s)
        fcx = 0.0d0
        if (s4 .gt. r8min) then
            call provec(w2, qg(4), u)
            call provec(u, w2, v)
            call pscvec(3, s2, v, vp)
!          NORME DE LA VITESSE PERPENDICULAIRE
            vite2=ddot(3,vp,1,vp,1)
            valpav(1) = sqrt( vite2 )
            if (valpav(1) .gt. r8min) then
!            RECUPERATION DE L'EFFORT EN FONCTION DE LA VITESSE
                call tecach('ONN', 'PVENTCX', 'L', iret, iad=ifcx)
                if (iret .ne. 0) goto 999
                if (zk8(ifcx)(1:1) .eq. '.') goto 999
                call fointe('FM', zk8(ifcx), 1, nompav, valpav,&
                            fcx, iret)
                fcx = fcx / valpav(1)
            endif
        endif
        call pscvec(3, fcx, vp, qg(4))
!
        call utpvgl(nno, nc, pgl, qg(1), ql(1))
!        --- CALCUL DES FORCES NODALES EQUIVALENTES EN REPERE LOCAL ---
        do i = 1, 6
            fl(i)= ql(i) * xl / 2.d0
        end do
        call utpvlg(nno, nc, pgl, fl(1), vect)
!
! ECRITURE DANS LE VECTEUR PVECTUR SUIVANT L'ELEMENT
!
        if (nomte .eq. 'MECA_BARRE') then
            do i = 1, 6
                zr(lvect+i-1) = vect(i)
            end do
        else if (nomte.eq.'MECA_2D_BARRE') then
            zr(lvect) = vect(1)
            zr(lvect+1) = vect(2)
            zr(lvect+2) = vect(4)
            zr(lvect+3) = vect(5)
        endif
!
!
    else if (option.eq.'CHAR_MECA_TEMP_R') then
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
        call jevech('PCAGNBA', 'L', lsect)
        a = zr(lsect)
!        --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
        call jevech('PMATERC', 'L', lmater)
        call rcvalb(fami, 1, 1, '+', zi(lmater),&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    1, 'E', e, codres, 1)
!
!        TEMPERATURE DE REFERENCE
        call verift(fami, 1, 1, '+', zi(lmater),&
                    epsth=epsth)
!
!        TERME DE LA MATRICE ELEMENTAIRE
        xrig = e(1) * a / xl
!
!        DEPLACEMENT INDUIT PAR LA TEMPERATURE
        xdep = epsth * xl
!
!        --- CALCUL DES FORCES INDUITES ---
        fl(1) = -xrig * xdep
        fl(4) = xrig * xdep
        call utpvlg(nno, nc, pgl, fl(1), vect)
!
! ECRITURE DANS LE VECTEUR PVECTUR SUIVANT L'ELEMENT
!
        if (nomte .eq. 'MECA_BARRE') then
            do i = 1, 6
                zr(lvect+i-1) = vect(i)
            end do
        else if (nomte.eq.'MECA_2D_BARRE') then
            zr(lvect) = vect(1)
            zr(lvect+1) = vect(2)
            zr(lvect+2) = vect(4)
            zr(lvect+3) = vect(5)
        endif
!
!
    else if (option.eq.'CHAR_MECA_SECH_R') then
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
        call jevech('PCAGNBA', 'L', lsect)
        a = zr(lsect)
!        --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
        call jevech('PMATERC', 'L', lmater)
        call rcvalb(fami, 1, 1, '+', zi(lmater),&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    1, 'E', e, codres, 1)
!
        call tecach('ONN', 'PTEMPSR', 'L', iret, iad=itemps)
        if (itemps .ne. 0) then
            instan = zr(itemps)
        else
            instan = 0.d0
        endif
!
!        TEMPERATURE EFFECTIVE
        call rcvarc(' ', 'TEMP', '+', fami, 1,&
                    1, temp, iret)
!
        call rcvarc(' ', 'SECH', '+', 'RIGI', 1,&
                    1, sech, iret)
        if (iret .ne. 0) sech=0.d0
        call rcvarc(' ', 'SECH', 'REF', 'RIGI', 1,&
                    1, sref, iret)
        if (iret .ne. 0) sref=0.d0
!
        nompar(1) = 'TEMP'
        valpa2(1) = temp
        nompar(2) = 'INST'
        valpa2(2) = instan
        nompar(3) = 'SECH'
        valpa2(3) = sech
!
!        TERME DE LA MATRICE ELEMENTAIRE
        xrig = e(1) * a / xl
!
! ----      INTERPOLATION DE K_DESSICCA EN FONCTION DE LA TEMPERATURE
!           DU SECHAGE
!           ----------------------------------------------------------
        call rcvalb('RIGI', 1, 1, '+', zi(lmater),&
                    ' ', 'ELAS', 3, nompar, valpa2,&
                    1, 'K_DESSIC', kdessi, codres, 0)
!
        if (codres(1) .ne. 0) kdessi(1)=0.d0
!
!C        DEPLACEMENT INDUIT PAR LE SECHAGE
        xdep = -kdessi(1)*(sref-sech) * xl
!
!        --- CALCUL DES FORCES INDUITES ---
        fl(1) = -xrig * xdep
        fl(4) = xrig * xdep
        call utpvlg(nno, nc, pgl, fl(1), vect)
!
! ECRITURE DANS LE VECTEUR PVECTUR SUIVANT L'ELEMENT
!
        if (nomte .eq. 'MECA_BARRE') then
            do i = 1, 6
                zr(lvect+i-1) = vect(i)
            end do
        else if (nomte.eq.'MECA_2D_BARRE') then
            zr(lvect) = vect(1)
            zr(lvect+1) = vect(2)
            zr(lvect+2) = vect(4)
            zr(lvect+3) = vect(5)
        endif
!
!
    else if (option.eq.'CHAR_MECA_HYDR_R') then
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
        call jevech('PCAGNBA', 'L', lsect)
        a = zr(lsect)
!        --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
        call jevech('PMATERC', 'L', lmater)
        call rcvalb('RIGI', 1, 1, '+', zi(lmater),&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    1, 'E', e, codres, 1)
!
! ---- RECUPERATION DE L'INSTANT
!      -------------------------
        call tecach('ONN', 'PTEMPSR', 'L', iret, iad=itemps)
        if (itemps .ne. 0) then
            instan = zr(itemps)
        else
            instan = 0.d0
        endif
!
!        TEMPERATURE EFFECTIVE
        call rcvarc(' ', 'TEMP', '+', fami, 1,&
                    1, temp, iret1)
!
!        HYDRATATION EFFECTIVE
        call rcvarc(' ', 'HYDR', '+', 'RIGI', 1,&
                    1, hydr, iret)
        if (iret .ne. 0) hydr=0.d0
!
        nompar(1) = 'TEMP'
        valpa2(1) = temp
        nompar(2) = 'INST'
        valpa2(2) = instan
        nompar(3) = 'HYDR'
        valpa2(3) = hydr
!
!        TERME DE LA MATRICE ELEMENTAIRE
        xrig = e(1) * a / xl
!
! ----      INTERPOLATION DE K_DESSICCA EN FONCTION DE LA TEMPERATURE
!           OU DE L HYDRATATION
!           ----------------------------------------------------------
        call rcvalb('RIGI', 1, 1, '+', zi(lmater),&
                    ' ', 'ELAS', 3, nompar, valpa2,&
                    1, 'B_ENDOGE', kendog, codres, 0)
!
        if (codres(1) .ne. 0) kendog(1)=0.d0
!
!C        DEPLACEMENT INDUIT PAR LE SECHAGE
        xdep = -kendog(1)*hydr * xl
!
!        --- CALCUL DES FORCES INDUITES ---
        fl(1) = -xrig * xdep
        fl(4) = xrig * xdep
        call utpvlg(nno, nc, pgl, fl(1), vect)
!
! ECRITURE DANS LE VECTEUR PVECTUR SUIVANT L'ELEMENT
!
        if (nomte .eq. 'MECA_BARRE') then
            do i = 1, 6
                zr(lvect+i-1) = vect(i)
            end do
        else if (nomte.eq.'MECA_2D_BARRE') then
            zr(lvect) = vect(1)
            zr(lvect+1) = vect(2)
            zr(lvect+2) = vect(4)
            zr(lvect+3) = vect(5)
        endif
!
    endif
!
    goto 100
998 continue
    call utmess('F', 'ELEMENTS3_34')
!
999 continue
    call utmess('F', 'ELEMENTS3_35')
!
100 continue
end subroutine
