subroutine dinosi(nbt, neq, nno, nc, pgl,&
                  klv, dul, sim, sip, fono,&
                  numloi, varint)
! ----------------------------------------------------------------------
    implicit none
    include 'asterfort/pmavec.h'
    include 'asterfort/ut2vlg.h'
    include 'asterfort/utpvlg.h'
    include 'asterfort/vecma.h'
    integer :: nbt, neq, nno, nc, numloi
    real(kind=8) :: pgl(3, 3), klv(nbt), dul(neq), sim(neq)
    real(kind=8) :: sip(neq), fono(neq), varint(*)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! ======================================================================
!
!     CALCUL DES EFFORTS GENERALISES (REPERE LOCAL)
!     ET DES FORCES NODALES (REPERE GLOBAL). COMME ON TRAITE DES
!     ELEMENTS DISCRETS, CES QUANTITES SONT EGALES, AU REPERE PRES.
!
! ======================================================================
!
! IN  : NBT    : NOMBRE DE VALEURS POUR LA DEMI-MATRICE
!       NEQ    : NOMBRE DE DDL DE L'ELEMENT
!       NNO    : NOMBRE DE NOEUDS DE L'ELEMENT (1 OU 2)
!       NC     : NOMBRE DE DDL PAR NOEUD
!       PGL    : MATRICE DE PASSAGE REPERE GLOBAL -> LOCAL
!       KLV    : MATRICE DE "RAIDEUR TANGENTE"
!       DUL    : INCREMENT DE DEPLACEMENT LOCAL
!       SIM    : EFFORTS GENERALISES A L'INSTANT PRECEDENT
!       NUMLOI : NUMERO DE LA LOI DES DISCRETS
!       VARINT : VARIABLE INTERNE A T+
!
! OUT : SIP    : EFFORTS GENERALISES ACTUALISES
!       FONO   : FORCES NODALES
!
!**************** DECLARATION DES VARIABLES LOCALES ********************
!
    real(kind=8) :: klc(144), fl(12)
    integer :: ii, iforc
!
!************ FIN DES DECLARATIONS DES VARIABLES LOCALES ***************
!
!     DANS LE CAS DE LA LOI 2, LES EFFORTS SONT DANS V1
!     ILS SONT EXPRIMES DANS LE REPERE LOCAL
    if (numloi .eq. 2) then
        if (nno .eq. 1) then
            do 110 ii = 1, nc
                iforc = 2*(ii-1)+1
                sip(ii) = varint(iforc)
                fl(ii) = varint(iforc)
110          continue
        else if (nno.eq.2) then
            do 115 ii = 1, nc
                iforc = 2*(ii-1)+1
                sip(ii) = varint(iforc)
                sip(ii+nc) = varint(iforc)
                fl(ii) = -varint(iforc)
                fl(ii+nc) = -varint(iforc)
115          continue
        endif
    else
! ------ DEMI-MATRICE KLV TRANSFORMEE EN MATRICE PLEINE KLC
        call vecma(klv, nbt, klc, neq)
!
! ------ CALCUL DE FL = KLC.DUL (INCREMENT D'EFFORT)
        call pmavec('ZERO', neq, klc, dul, fl)
!
! ------ EFFORTS GENERALISES AUX NOEUDS 1 ET 2 (REPERE LOCAL)
!        ON CHANGE LE SIGNE DES EFFORTS SUR LE PREMIER NOEUD
!        POUR LES MECA_DIS_TR_L ET MECA_DIS_T_L
        if (nno .eq. 1) then
            do 100 ii = 1, nc
                sip(ii) = fl(ii) + sim(ii)
                fl(ii) = fl(ii) + sim(ii)
100          continue
        else if (nno.eq.2) then
            do 105 ii = 1, nc
                sip(ii) = -fl(ii) + sim(ii)
                sip(ii+nc) = fl(ii+nc) + sim(ii+nc)
                fl(ii) = fl(ii) - sim(ii)
                fl(ii+nc) = fl(ii+nc) + sim(ii+nc)
105          continue
        endif
    endif
!
! --- FORCES NODALES AUX NOEUDS 1 ET 2 (REPERE GLOBAL)
    if (nc .ne. 2) then
        call utpvlg(nno, nc, pgl, fl, fono)
    else
        call ut2vlg(nno, nc, pgl, fl, fono)
    endif
! ----------------------------------------------------------------------
!
end subroutine
