subroutine te0042(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/tpsivp.h"
#include "asterfort/infdis.h"
#include "asterfort/infted.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/pmavec.h"
#include "asterfort/ut2mgl.h"
#include "asterfort/ut2pgl.h"
#include "asterfort/ut2vgl.h"
#include "asterfort/utmess.h"
#include "asterfort/utppgl.h"
#include "asterfort/utpsgl.h"
#include "asterfort/utpvgl.h"
#include "asterfort/vecma.h"
#include "asterfort/vecmap.h"
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
!     ------------------------------------------------------------------
!     CALCUL DU VECTEUR ELEMENTAIRE EFFORT GENERALISE
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!                   'SIEF_ELGA'
! IN  NOMTE  : K16 : NOM DU TYPE D'ELEMENT DISCRET :
!         MECA_DIS_T_N      MECA_DIS_T_L       MECA_DIS_TR_N
!         MECA_DIS_TR_L
!         MECA_2D_DIS_T_N   MECA_2D_DIS_T_L    MECA_2D_DIS_TR_N
!         MECA_2D_DIS_TR_L
!     ------------------------------------------------------------------
!
    integer :: nbterm, nno, nc, neq, irep, i, ndim, ibid
    integer :: ldis, lorien, jeffo, jdepl, infodi, itype
    real(kind=8) :: ulr(12), flr(12)
    real(kind=8) :: pgl(3, 3), klc(144), mat(144), r8bid
    character(len=8) :: k8bid
    character(len=16) :: ch16
!     ------------------------------------------------------------------
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
!     DISCRET DE TYPE RAIDEUR
    call infdis('DISK', infodi, r8bid, k8bid)
    if (infodi .eq. 0) then
        call utmess('A+', 'DISCRETS_27', sk=nomte)
        call infdis('DUMP', ibid, r8bid, 'A+')
    endif
!
!     MATRICE DE RAIDEUR SYMETRIQUE OU PAS
    infodi = 1
    call infdis('SYMK', infodi, r8bid, k8bid)
! --- INFORMATIONS SUR LES DISCRETS :
!        NBTERM  = NOMBRE DE COEFFICIENTS DANS K
!        NNO     = NOMBRE DE NOEUDS
!        NC      = NOMBRE DE COMPOSANTE PAR NOEUD
!        NDIM    = DIMENSION DE L'ELEMENT
!        ITYPE = TYPE DE L'ELEMENT
    call infted(nomte, infodi, nbterm, nno, nc,&
                ndim, itype)
    neq = nno*nc
!
!     --- MATRICE DE RIGIDITE LOCALE ---
    call jevech('PCADISK', 'L', ldis)
    call jevech('PCAORIE', 'L', lorien)
    call matrot(zr(lorien), pgl)
!
!     --- ABSOLU VERS LOCAL ? ---
!     --- IREP = 1 = MATRICE EN REPERE GLOBAL ==> PASSER EN LOCAL ---
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
 10     continue
    endif
!
!     ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
    if (infodi .eq. 1) then
        call vecma(mat, nbterm, klc, neq)
    else if (infodi.eq.2) then
        call vecmap(mat, nbterm, klc, neq)
    endif
!
!     --- CALCUL DES VECTEURS ELEMENTAIRES ----
    if (option .eq. 'SIEF_ELGA') then
        call jevech('PCONTRR', 'E', jeffo)
        call jevech('PDEPLAR', 'L', jdepl)
!
!        --- VECTEUR DEPLACEMENT LOCAL  ULR = PGL * UG  ---
        if (ndim .eq. 3) then
            call utpvgl(nno, nc, pgl, zr(jdepl), ulr)
        else if (ndim.eq.2) then
            call ut2vgl(nno, nc, pgl, zr(jdepl), ulr)
        endif
!
!        --- VECTEUR EFFORT      LOCAL  FLR = KLC * ULR  ---
        call pmavec('ZERO', neq, klc, ulr, flr)
!
    else
        ch16 = option
        call utmess('F', 'ELEMENTS2_47', sk=ch16)
    endif
!
!     ON CHANGE LE SIGNE DES EFFORTS SUR LE PREMIER NOEUD, POUR LES
!     ELEMENTS A 2 NOEUDS
    if (nno .eq. 1) then
        do 50 i = 1, neq
            zr(jeffo+i-1) = flr(i)
 50     continue
    else
        do 60 i = 1, nc
            zr(jeffo+i-1) = -flr(i)
            zr(jeffo+i+nc-1) = flr(i+nc)
 60     continue
    endif
end subroutine
