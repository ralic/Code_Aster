subroutine redrpr(mod, imate, sigp, vip, dsde,&
                  icode)
! =====================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! =====================================================================
    implicit      none
    include 'asterfort/dplitg.h'
    include 'asterfort/dpmata.h'
    include 'asterfort/dpmate.h'
    include 'asterfort/dppatg.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcinve.h'
    include 'blas/ddot.h'
    integer :: imate, icode
    real(kind=8) :: vip(*), sigp(*), dsde(6, 6)
    character(len=8) :: mod
! =====================================================================
! --- ROUTINE POUR LES RECUPERATIONS DES DONNES -----------------------
! --- POUR LE CALCUL DU TENSEUR TANGENT -------------------------------
! --- ICODE = 0 CORRESPONDT AU CAS ELASTIQUE --------------------------
! --- ICODE = 1 SINON -------------------------------------------------
! =====================================================================
    integer :: ndt, ndi, nvi, typedp
    real(kind=8) :: pplus, materf(5, 2), hookf(6, 6), dpdeno, dp
    real(kind=8) :: se(6), seq, plas, alpha, phi
    real(kind=8) :: siie, deux, trois
! ======================================================================
    common /tdim/   ndt, ndi
! =====================================================================
! --- INITIALISATION --------------------------------------------------
! =====================================================================
    parameter  ( deux  = 2.0d0 )
    parameter  ( trois = 3.0d0 )
! =====================================================================
    call lcinve(0.0d0, se)
    call lcinma(0.0d0, dsde)
    call lcinma(0.0d0, hookf)
! =====================================================================
! --- RECUPERATION DES DONNEES MATERIAUX ------------------------------
! =====================================================================
    call dpmate(mod, imate, materf, ndt, ndi,&
                nvi, typedp)
    pplus = vip(1)
    dp = 0.0d0
    plas = vip(nvi)
    icode = 1
    seq = 0.0d0
    if (typedp .eq. 1) then
! =====================================================================
! --- RECUPERATION DE R' POUR UNE LOI DP DE TYPE LINEAIRE -------------
! =====================================================================
        alpha = materf(3,2)
        dpdeno = dplitg( materf, pplus, plas )
    else if (typedp.eq.2) then
! =====================================================================
! --- RECUPERATION DE R' POUR UNE LOI DP DE TYPE PARABOLIQUE ----------
! =====================================================================
        phi = materf(2,2)
        alpha = deux * sin(phi) / (trois - sin(phi))
        dpdeno = dppatg( materf, pplus, plas )
    endif
    if (plas .eq. 0.0d0) then
        icode = 0
    else
        if (plas .eq. 1.0d0) then
! =====================================================================
! --- INTEGRATION ELASTIQUE : SIGF = HOOKF EPSP -----------------------
! =====================================================================
            call lcdevi(sigp, se)
            siie=ddot(ndt,se,1,se,1)
            seq = sqrt (trois*siie/deux)
        endif
        call dpmata(mod, materf, alpha, dp, dpdeno,&
                    pplus, se, seq, plas, dsde)
    endif
! =====================================================================
! =====================================================================
end subroutine
