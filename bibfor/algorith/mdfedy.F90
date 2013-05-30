subroutine mdfedy(nbpal, nbmode, numpas, dt, dtsto,&
                  tcf, vrotat, dplmod, depgen, vitgen,&
                  fexgen, typal, finpal, cnpal, prdeff,&
                  conv, fsauv)
    implicit none
    include 'asterfort/envdep.h'
    include 'asterfort/recfor.h'
    real(kind=8) :: depgen(*), vitgen(*), fexgen(*)
    real(kind=8) :: dplmod(nbpal, nbmode, *), dt, dtsto, tcf
    real(kind=8) :: vrotat, conv
    integer :: numpas
    logical :: prdeff
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.greffet at edf.fr
! ======================================================================
! TOLE CRS_1404
!
!              RECUPERATION DES FORCES VENANT D'EDYOS
!                 ET ENVOI DES CHAMPS CINEMATIQUES
!     ------------------------------------------------------------------
! IN  : NBMODE : NOMBRE DE MODES
! IN  : DEPGEN : DEPLACEMENTS GENERALISES
! IN  : VITGEN : VITESSES GENERALISEES
! VAR : FEXGEN : FORCES GENERALISEES
! IN  : NBPAL  : NOMBRE DE PALIERS
! IN  : DPLMOD : TABLEAU DES DEPL MODAUX AUX NOEUDS DE CHOC
!
! IN  : TCF    : INSTANT DE CALCUL
! IN  : DT     : PAS DE TEMPS
! IN  : DTSTO  : INSTANT DE STOCKAGE
! IN  : VROTAT : VITESSE DE ROTATION
! IN  : NUMDDL : NUMEROTATION DDL
!
! OUT  : CONV   : INDICATEUR DE CONVERGENCE EDYOS
! ----------------------------------------------------------------------
    integer :: i, j, k, l
    real(kind=8) :: dep(nbpal, 6), vit(nbpal, 6), force(nbpal, 3)
!
    integer :: palmax
!-----------------------------------------------------------------------
    integer :: nbmode, nbpal
!-----------------------------------------------------------------------
    parameter (palmax=20)
    character(len=3) :: finpal(palmax)
    character(len=6) :: typal(palmax)
    character(len=8) :: cnpal(palmax)
    real(kind=8) :: fsauv(palmax, 3)
!
    do 30 j = 1, nbpal
        do 5 l = 1, 6
            dep(j,l)= 0.d0
            vit(j,l)= 0.d0
 5      continue
        do 20 i = 1, nbmode
            do 15 k = 1, 6
                dep(j,k)= dep(j,k)+ dplmod(j,i,k)*depgen(i)
                vit(j,k)= vit(j,k)+ dplmod(j,i,k)*vitgen(i)
15          continue
20      continue
30  end do
    prdeff = .true.
!   ENVOI DES CHAMPS CINEMTATIQUES A EDYOS
    call envdep(numpas, nbpal, dt, dtsto, tcf,&
                dep, vit, vrotat, finpal, prdeff)
!   RECEPTION DES EFFORTS VENANT D'EDYOS
    call recfor(numpas, nbpal, force, typal, finpal,&
                cnpal, prdeff, conv)
!   COMBINAISON DES EFFORTS GENERALISES
    if (conv .gt. 0.0d0) then
        do 200 j = 1, nbpal
            fsauv(j,1)=force(j,1)
            fsauv(j,2)=force(j,2)
            fsauv(j,2)=force(j,3)
            do 100 i = 1, nbmode
                fexgen(i)=fexgen(i)+dplmod(j,i,1)*force(j,1) +dplmod(&
                j,i,2)*force(j,2) +dplmod(j,i,3)*force(j,3)
100          continue
200      continue
    else
!      NON CONVERGENCE EDYOS : ON UTILISE FSAUV
        do 210 j = 1, nbpal
            fsauv(j,1)=force(j,1)
            fsauv(j,2)=force(j,2)
            fsauv(j,2)=force(j,3)
            do 110 i = 1, nbmode
                fexgen(i)=fexgen(i)+dplmod(j,i,1)*fsauv(j,1) +dplmod(&
                j,i,2)*fsauv(j,2) +dplmod(j,i,3)*fsauv(j,3)
110          continue
210      continue
    endif
!
end subroutine
