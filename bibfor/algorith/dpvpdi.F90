subroutine dpvpdi(nbmat, mater, td, tf, tr,&
                  depst, deps)
! =====================================================================
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
! --- LOI DE COMPORTEMENT DE TYPE DRUCKER PRAGER VISCOPLASTIQUE -
! --- VISC_DRUC_PRAG
! --- RETRAIT DE LA DEFORMATION DUE A LA DILATATION THERMIQUE ---------
! =====================================================================
    implicit      none
    include 'asterc/iisnan.h'
    include 'asterfort/u2mess.h'
    integer :: nbmat
    real(kind=8) :: mater(nbmat, 2), td, tf, tr, depst(6), deps(6)
! =====================================================================
! --- IN --- : NBMAT   NOMBRE DE PARAMETRES DU MODELE -----------------
! --- IN --- : MATER   COEFFICIENTS MATERIAU --------------------------
! ---------- : TD      TEMPERATURE DEBUT INCREMENT --------------------
! ---------- : TF      TEMPERATURE FIN INCREMENT ----------------------
! ---------- : TR      TEMPERATURE DE REFERENCE -----------------------
! ---------- : DEPST   INCREMENT DE DEFORMATION TOTALE ----------------
! --- OUT -- : DEPS   INCREMENT DE DEFORMATION MECANIQUE -------------
! =====================================================================
    integer :: ii, ndt, ndi
    real(kind=8) :: alpha
! =====================================================================
    common /tdim/   ndt, ndi
! =====================================================================
! --- LES PARAMETRES MATERIAUX SONT SUPPOSES CONSTANT -----------------
! =====================================================================
!
    alpha = mater(3,1)
! INITIALISATION DE DEPS A DEPST
!
    do 5 ii = 1, ndt
        deps(ii) = depst(ii)
 5  continue
!
!
    if ((iisnan(tr).eq.0) .and. (iisnan(tf).eq.0) .and. (iisnan(td).eq.0)) then
        do 10 ii = 1, ndi
            deps(ii) = depst(ii) - ( alpha*(tf-tr) - alpha*(td-tr))
10      continue
        do 20 ii = ndi+1, ndt
            deps(ii) = depst(ii)
20      continue
        elseif (((iisnan(tr).eq.0).or.(iisnan(td).eq.0).or. (iisnan(tf)&
    .eq.0)).and.(alpha.ne.0.d0)) then
        call u2mess('F', 'CALCULEL_15')
    endif
! =====================================================================
end subroutine
