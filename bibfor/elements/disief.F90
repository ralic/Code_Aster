subroutine disief(nbt, neq, nno, nc, pgl,&
                  klv, dul, sim, ilogic, duly,&
                  sip, fono, force, dimele)
! ----------------------------------------------------------------------
    implicit none
#include "asterc/r8prem.h"
#include "asterfort/pmavec.h"
#include "asterfort/ut2vlg.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vecma.h"
    integer :: nbt, neq, ilogic, nno, nc, dimele
    real(kind=8) :: pgl(3, 3), klv(nbt), dul(neq), sim(neq), duly
    real(kind=8) :: sip(neq), fono(neq), force(3)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DES EFFORTS GENERALISES (REPERE LOCAL)
!     ET DES FORCES NODALES (REPERE GLOBAL). COMME ON TRAITE DES
!     ELEMENTS DISCRETS, CES QUANTITES SONT EGALES, AU REPERE PRES.
!
! ----------------------------------------------------------------------
!
! IN  : NBT    : NOMBRE DE VALEURS POUR LA DEMI-MATRICE
!       NEQ    : NOMBRE DE DDL DE L'ELEMENT
!       NNO    : NOMBRE DE NOEUDS DE L'ELEMENT (1 OU 2)
!       NC     : NOMBRE DE DDL PAR NOEUD
!       PGL    : MATRICE DE PASSAGE REPERE GLOBAL -> LOCAL
!       KLV    : MATRICE DE "RAIDEUR TANGENTE"
!       DUL    : INCREMENT DE DEPLACEMENT LOCAL
!       SIM    : EFFORTS GENERALISES A L'INSTANT PRECEDENT
!       ILOGIC : VAUT 1 DANS LES CAS DU COMPORTEMENT "ARME" (ARMEMENT)
!       DULY   :
!
! OUT : SIP    : EFFORTS GENERALISES ACTUALISES
!       FONO   : FORCES NODALES
!
! =============== DECLARATION DES VARIABLES LOCALES ====================
!
    integer :: n, i
    real(kind=8) :: klc(144), fl(12), zero
!
! ----------------------------------------------------------------------
!
    zero = 0.d0
! --- DEMI-MATRICE KLV TRANSFORMEE EN MATRICE PLEINE KLC
    call vecma(klv, nbt, klc, neq)
! --- CALCUL DE FL = KLC.DUL (INCREMENT D'EFFORT)
    call pmavec('ZERO', neq, klc, dul, fl)
! --- EFFORTS GENERALISES AUX NOEUDS 1 ET 2 (REPERE LOCAL)
!     ON CHANGE LE SIGNE DES EFFORTS SUR LE PREMIER NOEUD
!     POUR LES MECA_DIS_TR_L ET MECA_DIS_T_L
    if (nno .eq. 1) then
        do 90 i = 1, neq
            sip(i) = fl(i) + sim(i)
            fl(i) = fl(i) + sim(i)
90      continue
    else if (nno.eq.2) then
        do 100 i = 1, nc
            sip(i) = -fl(i) + sim(i)
            sip(i+nc) = fl(i+nc) + sim(i+nc)
            fl(i) = fl(i) - sim(i)
            fl(i+nc) = fl(i+nc) + sim(i+nc)
100      continue
    endif
!
! --- PETITE MODIF POUR LES ARMEMENTS
    if (ilogic .eq. 1) then
        sip(2) = sim(2) + force(1)*duly
        sip(8) = sim(8) + force(1)*duly
        fl(2) = -sim(2) - force(1)*duly
        fl(8) = sim(8) + force(1)*duly
    endif
    if (ilogic .eq. 2) then
        if (nno .eq. 1) then
            fl(1) = force(1)
            fl(2) = force(2)
            sip(1) = force(1)
            sip(2) = force(2)
            if (dimele .eq. 3) then
                fl(3) = force(3)
                sip(3) = force(3)
            endif
        else if (nno.eq.2) then
            fl(1) = -force(1)
            sip(1) = force(1)
            fl(1+nc) = force(1)
            sip(1+nc) = force(1)
            fl(2) = -force(2)
            sip(2) = force(2)
            fl(2+nc) = force(2)
            sip(2+nc) = force(2)
            if (dimele .eq. 3) then
                fl(3) = -force(3)
                sip(3) = force(3)
                fl(3+nc) = force(3)
                sip(3+nc) = force(3)
            endif
        endif
        if (abs(force(1)) .lt. r8prem()) then
            do 10 n = 1, neq
                fl(n) = zero
                sip(n) = zero
10          continue
        endif
    endif
!
! --- FORCES NODALES AUX NOEUDS 1 ET 2 (REPERE GLOBAL)
    if (nc .ne. 2) then
        call utpvlg(nno, nc, pgl, fl, fono)
    else
        call ut2vlg(nno, nc, pgl, fl, fono)
    endif
end subroutine
