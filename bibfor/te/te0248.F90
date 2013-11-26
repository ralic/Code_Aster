subroutine te0248(optioz, nomtez)
    implicit none
#include "jeveux.h"
#include "asterc/r8nnem.h"
#include "asterfort/angvx.h"
#include "asterfort/comp1d.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/nmasym.h"
#include "asterfort/nmiclb.h"
#include "asterfort/nmmaba.h"
#include "asterfort/nmpime.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
    character(len=16) :: option, nomte
    character(len=*) :: optioz, nomtez
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
!
!     CALCUL DES OPTIONS FULL_MECA, RAPH_MECA, RIGI_MECA_TANG
!     ET RIGI_MECA_IMPLEX POUR COMPORTEMENTS LINEAIRES ET NON LINEAIRES
!     DES ELEMENTS DE BARRE 'MECA_BARRE'
!
! ----------------------------------------------------------------------
! IN  : OPTION : NOM DE L'OPTION A CALCULER (K16)
! IN  : NOMTE  : NOM DU TYPE_ELEMENT (K16)
! ----------------------------------------------------------------------
!
!
!
!
!
! *************** DECLARATION DES VARIABLES LOCALES ********************
!
    integer :: neq, nbt, nvamax, imate, igeom, iorie, isect, iinstm, ivarmp
    integer :: iinstp, ideplm, ideplp, icontm, ivarim, icompo
    integer :: icarcr, imatuu, ivectu, icontp, nno, nc, ivarip, jcret, nbvari
    integer :: jtab(7), iret
    parameter (neq=6,nbt=21,nvamax=8)
    character(len=4) :: fami
    character(len=16) :: valkm(3)
!
!   CONSTANTES POUR INTO MENEGOTTO
!
    integer :: ncstpm, codret
    parameter (ncstpm=13)
    real(kind=8) :: cstpm(ncstpm)
!
    real(kind=8) :: e, epsm
    real(kind=8) :: a, xlong0, xlongm, sigy, dsde
    real(kind=8) :: pgl(3, 3)
    real(kind=8) :: dul(neq), uml(neq), dlong
    real(kind=8) :: klv(nbt), vip(nvamax), vim(nvamax)
    real(kind=8) :: effnom, effnop, fono(neq)
    real(kind=8) :: w(6), ang1(3), xd(3), matuu(21), vectu(6)
    real(kind=8) :: deplm(6), deplp(6), sigx, epsx, depx, sigxp
    real(kind=8) :: etan
    real(kind=8) :: angmas(3)
    integer :: i
!
!
!
    logical :: vecteu
!
! *********** FIN DES DECLARATIONS DES VARIABLES LOCALES ***************
!
!
    option = optioz
    nomte = nomtez
    codret=0
    fami = 'RIGI'
!
! --- PARAMETRES EN ENTREE
!
    call jevech('PMATERC', 'L', imate)
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCAORIE', 'L', iorie)
    call jevech('PCAGNBA', 'L', isect)
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
!
!
! ---- LA PRESENCE DU CHAMP DE DEPLACEMENT A L INSTANT T+
! ---- DEVRAIT ETRE CONDITIONNE  PAR L OPTION (AVEC RIGI_MECA_TANG
! ---- CA N A PAS DE SENS).
! ---- CEPENDANT CE CHAMP EST INITIALISE A 0 PAR LA ROUTINE NMMATR.
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PDEPLPR', 'L', ideplp)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PCARCRI', 'L', icarcr)
!
    if ((option.eq. 'FULL_MECA_ELAS' .or. option.eq.'RIGI_MECA_ELAS')&
        .and. zk16(icompo).ne. 'ELAS')then
        valkm(1) = option
        valkm(2) = zk16(icompo)
        valkm(3) = nomte
        call utmess('F', 'ELEMENTS3_2',nk=3,valk=valkm)
    endif
!
! --- ANGLE DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! --- INITIALISE A R8NNEM (ON NE S'EN SERT PAS)
    call r8inir(3, r8nnem(), angmas, 1)
!
! --- PARAMETRES EN SORTIE
!
!
    if (option(1:10) .eq. 'RIGI_MECA_') then
        call jevech('PMATUUR', 'E', imatuu)
        ivarip = ivarim
        icontp = icontm
    else if (option(1:9).eq.'FULL_MECA') then
        call jevech('PMATUUR', 'E', imatuu)
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
    else if (option.eq.'RAPH_MECA') then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
    endif
!
    if (option(1:16) .eq. 'RIGI_MECA_IMPLEX') then
        call jevech('PCONTXR', 'E', icontp)
    endif
!
! --- RECUPERATION DE LA SECTION DE LA BARRE
!
    a = zr(isect)
    nno = 2
    nc = 3
!
! --- RECUPERATION DES ORIENTATIONS BETA,GAMMA
! --- ET CALCUL DES MATRICES DE CHANGEMENT DE REPERE
!
    if (zk16(icompo+2) (6:10) .eq. '_REAC') then
        if (nomte .eq. 'MECA_BARRE') then
            do 10 i = 1, 3
                w(i) = zr(igeom-1+i) + zr(ideplm-1+i) + zr(ideplp-1+i)
                w(i+3) = zr(igeom+2+i) + zr(ideplm+2+i) + zr(ideplp+2+ i)
                xd(i) = w(i+3) - w(i)
10          continue
        else if (nomte.eq.'MECA_2D_BARRE') then
            w(1) = zr(igeom-1+1) + zr(ideplm-1+1) + zr(ideplp-1+1)
            w(2) = zr(igeom-1+2) + zr(ideplm-1+2) + zr(ideplp-1+2)
            w(3) = 0.d0
            w(4) = zr(igeom-1+3) + zr(ideplm-1+3) + zr(ideplp-1+3)
            w(5) = zr(igeom-1+4) + zr(ideplm-1+4) + zr(ideplp-1+4)
            w(6) = 0.d0
            xd(1) = w(4) - w(1)
            xd(2) = w(5) - w(2)
            xd(3) = 0.d0
        endif
        call angvx(xd, ang1(1), ang1(2))
        ang1(3) = zr(iorie+2)
        call matrot(ang1, pgl)
    else
        if (nomte .eq. 'MECA_BARRE') then
            do 20 i = 1, 3
                w(i) = zr(igeom-1+i)
                w(i+3) = zr(igeom+2+i)
                xd(i) = w(i+3) - w(i)
20          continue
        else if (nomte.eq.'MECA_2D_BARRE') then
            w(1) = zr(igeom-1+1)
            w(2) = zr(igeom-1+2)
            w(3) = 0.d0
            w(4) = zr(igeom-1+3)
            w(5) = zr(igeom-1+4)
            w(6) = 0.d0
            xd(1) = w(4) - w(1)
            xd(2) = w(5) - w(2)
            xd(3) = 0.d0
        endif
        call matrot(zr(iorie), pgl)
    endif
!
    xlong0=ddot(3,xd,1,xd,1)
    xlong0 = sqrt(xlong0)
!
    if (xlong0 .eq. 0.d0) then
        call utmess('F', 'ELEMENTS3_62')
    endif
!
!
! --- INCREMENT DE DEPLACEMENT EN REPERE LOCAL
! CORRECTION CHAVANT : DUL = INCREMENT ENTRE INSTANT
! PLUS ET INSTANT MOINS
! ---    DUL  ENTRE LE REPOS ET LE DERNIER ETAT CONVERGE
! --- INCREMENT D'ALLONGEMENT DLONG
!
    if (nomte .eq. 'MECA_BARRE') then
        do 30 i = 1, 6
            deplm(i) = zr(ideplm+i-1)
            deplp(i) = zr(ideplp+i-1)
30      continue
    else if (nomte.eq.'MECA_2D_BARRE') then
        deplm(1) = zr(ideplm)
        deplm(2) = zr(ideplm+1)
        deplm(3) = 0.d0
        deplm(4) = zr(ideplm+2)
        deplm(5) = zr(ideplm+3)
        deplm(6) = 0.d0
!
        deplp(1) = zr(ideplp)
        deplp(2) = zr(ideplp+1)
        deplp(3) = 0.d0
        deplp(4) = zr(ideplp+2)
        deplp(5) = zr(ideplp+3)
        deplp(6) = 0.d0
!
    endif
!
    call utpvgl(nno, nc, pgl, deplm, uml)
    call utpvgl(nno, nc, pgl, deplp, dul)
!
    dlong = dul(4) - dul(1)
!
    xlongm = xlong0 + uml(4) - uml(1)
!
!
!
! --- RECUPERATION
! ---     DE L'EFFORT NORMAL PRECEDENT MOYEN EFFNOM POUR L'ELEMENT
    effnom = zr(icontm)
!
!
! --- RELATION DE COMPORTEMENT
!
!     ---------------------------------------------------
    if (zk16(icompo) .eq. 'ELAS' .or. zk16(icompo) .eq. 'VMIS_ISOT_LINE' .or. zk16(icompo)&
        .eq. 'VMIS_ISOT_TRAC' .or. zk16(icompo) .eq. 'CORR_ACIER' .or. zk16(icompo) .eq.&
        'VMIS_CINE_LINE') then
!     ---------------------------------------------------
!
! --- RECUPERATION DES CARACTERISTIQUES DU MATERIAU
!
        epsm = (uml(4)-uml(1))/xlong0
        call nmiclb(fami, 1, 1, option, zk16(icompo),&
                    zi(imate), xlong0, a, zr(iinstm), zr(iinstp),&
                    dlong, effnom, zr(ivarim), effnop, zr(ivarip),&
                    klv, fono, epsm, zr(icarcr), codret)
!
        if (option(1:16) .eq. 'RIGI_MECA_IMPLEX') then
            zr(icontp) = effnop
        endif
!
        if (option(1:10) .eq. 'RIGI_MECA_') then
            call utpslg(nno, nc, pgl, klv, matuu)
        else
            zr(icontp) = effnop
            if (option(1:9) .eq. 'FULL_MECA') then
                call utpslg(nno, nc, pgl, klv, matuu)
            endif
            call utpvlg(nno, nc, pgl, fono, vectu)
        endif
!
!     ---------------------------------------------------
    else if (zk16(icompo).eq.'VMIS_ASYM_LINE') then
!     ---------------------------------------------------
!
!        RECUPERATION DES CARACTERISTIQUES DU MATERIAU
!
        call nmmaba(zi(imate), zk16(icompo), e, dsde, sigy,&
                    ncstpm, cstpm)
!
        call nmasym(fami, 1, 1, zi(imate), option,&
                    xlong0, a, zr(iinstm), zr( iinstp), dlong,&
                    effnom, zr(ivarim), zr(icontp), zr(ivarip), klv,&
                    fono)
!
        if (option(1:10) .eq. 'RIGI_MECA_') then
            call utpslg(nno, nc, pgl, klv, matuu)
        else
            if (option(1:9) .eq. 'FULL_MECA') then
                call utpslg(nno, nc, pgl, klv, matuu)
            endif
            call utpvlg(nno, nc, pgl, fono, vectu)
        endif
!
!     ---------------------------------------------------
    else if (zk16(icompo).eq.'PINTO_MENEGOTTO') then
!     ---------------------------------------------------
!
!        RECUPERATION DES CARACTERISTIQUES DU MATERIAU
!
        call nmmaba(zi(imate), zk16(icompo), e, dsde, sigy,&
                    ncstpm, cstpm)
!
        vim(1) = zr(ivarim)
        vim(2) = zr(ivarim+1)
        vim(3) = zr(ivarim+2)
        vim(4) = zr(ivarim+3)
        vim(5) = zr(ivarim+4)
        vim(6) = zr(ivarim+5)
        vim(7) = zr(ivarim+6)
        vim(8) = zr(ivarim+7)
        call nmpime(fami, 1, 1, zi(imate), option,&
                    xlong0, a, xlongm, dlong, ncstpm,&
                    cstpm, vim, effnom, vip, effnop,&
                    klv, fono)
!
        if (option(1:10) .eq. 'RIGI_MECA_') then
            call utpslg(nno, nc, pgl, klv, matuu)
        else
            zr(icontp) = effnop
            if (option(1:9) .eq. 'FULL_MECA') then
                call utpslg(nno, nc, pgl, klv, matuu)
            endif
            zr(ivarip) = vip(1)
            zr(ivarip+1) = vip(2)
            zr(ivarip+2) = vip(3)
            zr(ivarip+3) = vip(4)
            zr(ivarip+4) = vip(5)
            zr(ivarip+5) = vip(6)
            zr(ivarip+6) = vip(7)
            zr(ivarip+7) = vip(8)
            call utpvlg(nno, nc, pgl, fono, vectu)
        endif
!
!     ------------
    else
!     ------------
!
        call r8inir(neq, 0.d0, fono, 1)
        call r8inir(nbt, 0.d0, klv, 1)
        vecteu = ( (option(1:9).eq.'FULL_MECA') .or. (option(1:9) .eq.'RAPH_MECA') )
!
!
        call jevech('PCOMPOR', 'L', icompo)
        if ((zk16(icompo-1+5)(1:7).ne.'DEBORST') .and. (zk16(icompo)(1: 4).ne.'SANS')) then
            valkm(1) = zk16(icompo)
            valkm(2) = ' '
            call utmess('F', 'ALGORITH6_81', nk=2, valk=valkm)
        else
!
            sigx=effnom/a
            epsx=(uml(4)-uml(1))/xlong0
            depx=dlong/xlong0
!
            if (vecteu) then
                call tecach('OON', 'PVARIMP', 'L', iret, nval=7,&
                            itab=jtab)
                nbvari = max(jtab(6),1)*jtab(7)
                ivarmp=jtab(1)
                call dcopy(nbvari, zr(ivarmp), 1, zr(ivarip), 1)
            endif
!
            call comp1d(fami, 1, 1, option, sigx,&
                        epsx, depx, angmas, zr(ivarim), zr(ivarip),&
                        sigxp, etan, codret)
!
            if (vecteu) then
!
! ---          STOCKAGE DE L'EFFORT NORMAL
                zr(icontp) = sigxp*a
!
!
! ---          CALCUL DES FORCES NODALES
!
                fono(1) = -sigxp*a
                fono(4) = sigxp*a
!
            endif
!
! ---       CALCUL DE LA MATRICE TANGENTE
!
            klv(1) = etan
            klv(7) = -etan
            klv(10) = etan
!
        endif
!
! ---  PASSAGE DE KLV ET FONO DU REPERE LOCAL AU REPERE GLOBAL
!
        if (option(1:10) .eq. 'RIGI_MECA_') then
            call utpslg(nno, nc, pgl, klv, matuu)
        else
            if (option(1:9) .eq. 'FULL_MECA') then
                call utpslg(nno, nc, pgl, klv, matuu)
            endif
            call utpvlg(nno, nc, pgl, fono, vectu)
        endif
!
!     ----------
    endif
!     ----------
!
    if (nomte .eq. 'MECA_BARRE') then
        if ((option(1:10).eq.'RIGI_MECA_') .or. (option(1:9) .eq.'FULL_MECA')) then
            do 70 i = 1, 21
                zr(imatuu+i-1) = matuu(i)
70          continue
        endif
        if (option(1:10) .ne. 'RIGI_MECA_') then
            do 80 i = 1, 6
                zr(ivectu+i-1) = vectu(i)
80          continue
        endif
!
    else if (nomte.eq.'MECA_2D_BARRE') then
        if ((option(1:10).eq.'RIGI_MECA_') .or. (option(1:9) .eq.'FULL_MECA')) then
            zr(imatuu) = matuu(1)
            zr(imatuu+1) = matuu(2)
            zr(imatuu+2) = matuu(3)
            zr(imatuu+3) = matuu(7)
            zr(imatuu+4) = matuu(8)
            zr(imatuu+5) = matuu(10)
            zr(imatuu+6) = matuu(11)
            zr(imatuu+7) = matuu(12)
            zr(imatuu+8) = matuu(14)
            zr(imatuu+9) = matuu(15)
        endif
        if (option(1:10) .ne. 'RIGI_MECA_') then
!
            zr(ivectu) = vectu(1)
            zr(ivectu+1) = vectu(2)
            zr(ivectu+2) = vectu(4)
            zr(ivectu+3) = vectu(5)
        endif
!
    endif
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
!
end subroutine
