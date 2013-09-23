subroutine te0044(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/infdis.h"
#include "asterfort/infted.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/ptenci.h"
#include "asterfort/ptenpo.h"
#include "asterfort/tecach.h"
#include "asterfort/ut2mgl.h"
#include "asterfort/ut2pgl.h"
#include "asterfort/ut2vgl.h"
#include "asterfort/utmess.h"
#include "asterfort/utppgl.h"
#include "asterfort/utpsgl.h"
#include "asterfort/utpvgl.h"
#include "asterfort/vecma.h"
#include "asterfort/vecmap.h"
!
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
!     ------------------------------------------------------------------
!     CALCUL DE L'ENERGIE DE DEFORMATION, ET CINETIQUE
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        'EPOT_ELEM' : CALCUL DE L'ENERGIE DE DEFORMATION
!        'ECIN_ELEM' : CALCUL DE L'ENERGIE CINETIQUE
! IN  NOMTE  : K16 : NOM DU TYPE D'ELEMENT DISCRET :
!         MECA_DIS_T_N
!         MECA_DIS_T_L
!         MECA_DIS_TR_N
!         MECA_DIS_TR_L
!
!      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
    real(kind=8) :: r8bid, ul(12), pgl(3, 3), klc(12, 12), mat(144)
    integer :: infodi, nbterm, nno, nc, ndim, itype, neq, kanl, irep, iiff
    integer :: i, lorie, ldepl, lvite, jende, ldis, jfreq, ibid, iret
    character(len=3) :: stopz
    character(len=8) :: k8bid
!
!     ------------------------------------------------------------------
!
    infodi = 1
!
!     ON VERIFIE QUE LES CARACTERISTIQUES ONT ETE AFFECTEES
!     LE CODE DU DISCRET
    call infdis('CODE', ibid, r8bid, nomte)
!     LE CODE STOKE DANS LA CARTE
    call infdis('TYDI', infodi, r8bid, k8bid)
    if (infodi .ne. ibid) then
        call utmess('F+', 'DISCRETS_25', sk=nomte)
        call infdis('DUMP', ibid, r8bid, 'F+')
    endif
!
    if (option .eq. 'EPOT_ELEM') then
!        DISCRET DE TYPE RAIDEUR
        call infdis('DISK', infodi, r8bid, k8bid)
        if (infodi .eq. 0) then
            call utmess('A+', 'DISCRETS_27', sk=nomte)
            call infdis('DUMP', ibid, r8bid, 'A+')
        endif
        call infdis('SYMK', infodi, r8bid, k8bid)
    else if (option.eq.'ECIN_ELEM') then
!        DISCRET DE TYPE MASSE
        call infdis('DISM', infodi, r8bid, k8bid)
        if (infodi .eq. 0) then
            call utmess('A+', 'DISCRETS_26', sk=nomte)
            call infdis('DUMP', ibid, r8bid, 'A+')
        endif
        call infdis('SYMM', infodi, r8bid, k8bid)
    else
        call utmess('F', 'ELEMENTS2_47', sk=option)
    endif
!
! --- INFORMATIONS SUR LES DISCRETS :
!        NBTERM   = NOMBRE DE COEFFICIENTS DANS K
!        NNO      = NOMBRE DE NOEUDS
!        NC       = NOMBRE DE COMPOSANTE PAR NOEUD
!        NDIM     = DIMENSION DE L'ELEMENT
!        ITYPE    = TYPE DE L'ELEMENT
    call infted(nomte, infodi, nbterm, nno, nc,&
                ndim, itype)
    neq = nno*nc
!
!     TYPE DE LA MATRICE DE MASSE
    kanl = 0
!     --- MATRICE DE ROTATION PGL ---
    call jevech('PCAORIE', 'L', lorie)
    call matrot(zr(lorie), pgl)
!     --- RECUPERATION DES DEPLACEMENTS OU DES VITESSES ET PASSAGE
!     --- AU REPERE LOCAL
    if (option .ne. 'ECIN_ELEM') then
        call jevech('PDEPLAR', 'L', ldepl)
        if (ndim .eq. 3) then
            call utpvgl(nno, nc, pgl, zr(ldepl), ul)
        else if (ndim.eq.2) then
            call ut2vgl(nno, nc, pgl, zr(ldepl), ul)
        endif
    else
        stopz='ONO'
        call tecach(stopz, 'PVITESR', 'L', iret, iad=lvite)
! IRET NE PEUT VALOIR QUE 0 (TOUT EST OK) OU 2 (CHAMP NON FOURNI)
        if (iret .eq. 0) then
            if (ndim .eq. 3) then
                call utpvgl(nno, nc, pgl, zr(lvite), ul)
            else if (ndim.eq.2) then
                call ut2vgl(nno, nc, pgl, zr(lvite), ul)
            endif
        else
            call tecach(stopz, 'PDEPLAR', 'L', iret, iad=ldepl)
            if (iret .eq. 0) then
                if (ndim .eq. 3) then
                    call utpvgl(nno, nc, pgl, zr(ldepl), ul)
                else if (ndim.eq.2) then
                    call ut2vgl(nno, nc, pgl, zr(ldepl), ul)
                endif
            else
                call utmess('F', 'ELEMENTS2_1', sk=option)
            endif
        endif
    endif
!
    if (option .eq. 'EPOT_ELEM') then
        call jevech('PENERDR', 'E', jende)
!
!        --- MATRICE DE RIGIDITE ---
        call jevech('PCADISK', 'L', ldis)
!        --- GLOBAL VERS LOCAL ? ---
!        --- IREP EQ 1 : MATRICE EN REPERE GLOBAL
!        --- IREP NE 1 : MATRICE EN REPERE LOCAL
        call infdis('REPK', irep, r8bid, k8bid)
        if (irep .eq. 1) then
            if (ndim .eq. 3) then
                if (infodi .eq. 1) then
                    call utpsgl(nno, nc, pgl, zr(ldis), mat)
                else if (infodi.eq.2) then
                    call utppgl(nno, nc, pgl, zr(ldis), mat)
                endif
            else if (ndim.eq.2) then
                if (infodi .eq. 1) then
                    call ut2mgl(nno, nc, pgl, zr(ldis), mat)
                else if (infodi.eq.2) then
                    call ut2pgl(nno, nc, pgl, zr(ldis), mat)
                endif
            endif
        else
            do 10 i = 1, nbterm
                mat(i) = zr(ldis+i-1)
10          continue
        endif
!
!        ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
        if (infodi .eq. 1) then
            call vecma(mat, nbterm, klc, neq)
        else if (infodi.eq.2) then
            call vecmap(mat, nbterm, klc, neq)
        endif
!
!        --- ENERGIE DE DEFORMATION ---
        iiff = 1
        call ptenpo(neq, ul, klc, zr(jende), itype,&
                    iiff)
!
    else if (option.eq.'ECIN_ELEM') then
        call jevech('PENERCR', 'E', jende)
!
!        --- MATRICE DE MASSE ---
        call jevech('PCADISM', 'L', ldis)
!        --- GLOBAL VERS LOCAL ? ---
!        --- IREP EQ 1 : MATRICE EN REPERE GLOBAL
!        --- IREP NE 1 : MATRICE EN REPERE LOCAL
        call infdis('REPM', irep, r8bid, k8bid)
        if (irep .eq. 1) then
            if (ndim .eq. 3) then
                if (infodi .eq. 1) then
                    call utpsgl(nno, nc, pgl, zr(ldis), mat)
                else if (infodi.eq.2) then
                    call utppgl(nno, nc, pgl, zr(ldis), mat)
                endif
            else if (ndim.eq.2) then
                if (infodi .eq. 1) then
                    call ut2mgl(nno, nc, pgl, zr(ldis), mat)
                else if (infodi.eq.2) then
                    call ut2pgl(nno, nc, pgl, zr(ldis), mat)
                endif
            endif
        else
            do 20 i = 1, nbterm
                mat(i) = zr(ldis+i-1)
20          continue
        endif
!
!        ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
        if (infodi .eq. 1) then
            call vecma(mat, nbterm, klc, neq)
        else if (infodi.eq.2) then
            call vecmap(mat, nbterm, klc, neq)
        endif
!
!        --- FREQUENCE ---
        call jevech('POMEGA2', 'L', jfreq)
!
!        --- ENERGIE CINETIQUE  ---
        iiff = 1
        call ptenci(neq, ul, klc, zr(jfreq), zr(jende),&
                    itype, kanl, iiff)
!
    else
        call utmess('F', 'ELEMENTS2_47', sk=option)
    endif
end subroutine
