subroutine rc32pm(lieu, seisme, pi, mi, mse,&
                  pm, pb, pmpb)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rc32s0.h'
    include 'asterfort/rc32st.h'
    real(kind=8) :: pi, mi(*), mse(*), pm, pb, pmpb
    logical :: seisme
    character(len=4) :: lieu
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     CALCUL DU PM_PB
!
! IN  : LIEU   : ='ORIG' : ORIGINE DU SEGEMNT, ='EXTR' : EXTREMITE
! IN  : SEISME : =.FALSE. SI PAS DE SEISME, =.TRUE. SINON
! IN  : MI     : EFFORTS ASSOCIEES A L'ETAT STABILISE (6+6)
! IN  : MSE    : EFFORTS DUS AU SEISME
! VAR : PM     : CONTRAINTE EQUIVALENTE PRIMAIRE DE MEMBRANE
! VAR : PB     : CONTRAINTE EQUIVALENTE PRIMAIRE DE FLEXION
! VAR : PMPB   : CONTRAINTE EQUIVALENTE PRIMAIRE DE MEMBRANE+FLEXION
!     ------------------------------------------------------------------
!
    integer :: icmps, icmp, jsigu, nbinst
    real(kind=8) :: sth(6), sij(6), sigu, pmij, pbij, pmpbij
! DEB ------------------------------------------------------------------
    call jemarq()
!
! --- CONTRAINTES DUES AUX CHARGEMENTS UNITAIRES
!
    call jeveuo('&&RC3200.MECA_UNIT .'//lieu, 'L', jsigu)
!
! --- PAS DE THERMIQUE POUR LES CRITERES DE NIVEAU 0
!
    nbinst = 0
    do 2 icmp = 1, 6
        sth(icmp) = 0.d0
 2  end do
!-----------------------------------------------------------------------
!
!                            CALCUL DU PM
!
!-----------------------------------------------------------------------
! --- CALCUL DES CONTRAINTES PAR COMBINAISON LINEAIRE
!     POUR LE CHARGEMENT MI (RECUP M_0)
!
    do 100 icmps = 1, 6
        sij(icmps) = 0.d0
        do 102 icmp = 1, 12
            sigu = zr(jsigu-1+156+6*(icmp-1)+icmps)
            sij(icmps) = sij(icmps) + mi(icmp)*sigu
102      continue
! ------ PRESSION
        sigu = zr(jsigu-1+156+72+icmps)
        sij(icmps) = sij(icmps) + pi*sigu
100  end do
!
    if (seisme) then
        call rc32s0('PMPB', mi, pi, mse, zr(jsigu+156),&
                    nbinst, sth, pmij)
    else
        call rc32st(sij, nbinst, sth, pmij)
    endif
    pm = max( pmij, pm )
!
!-----------------------------------------------------------------------
!
!                            CALCUL DU PB
!
!-----------------------------------------------------------------------
! --- CALCUL DES CONTRAINTES PAR COMBINAISON LINEAIRE
!     POUR LE CHARGEMENT MI (RECUP M_1)
!
    do 110 icmps = 1, 6
        sij(icmps) = 0.d0
        do 112 icmp = 1, 12
            sigu = zr(jsigu-1+234+6*(icmp-1)+icmps)
            sij(icmps) = sij(icmps) + mi(icmp)*sigu
112      continue
! ------ PRESSION
        sigu = zr(jsigu-1+234+72+icmps)
        sij(icmps) = sij(icmps) + pi*sigu
110  end do
!
    if (seisme) then
        call rc32s0('PMPB', mi, pi, mse, zr(jsigu+234),&
                    nbinst, sth, pbij)
    else
        call rc32st(sij, nbinst, sth, pbij)
    endif
    pb = max( pbij, pb )
!
!-----------------------------------------------------------------------
!
!                            CALCUL DU PMPB
!
!-----------------------------------------------------------------------
! --- CALCUL DES CONTRAINTES PAR COMBINAISON LINEAIRE
!     POUR LE CHARGEMENT MI (RECUP LINEARISEES)
!
    do 120 icmps = 1, 6
        sij(icmps) = 0.d0
        do 122 icmp = 1, 12
            sigu = zr(jsigu-1+78+6*(icmp-1)+icmps)
            sij(icmps) = sij(icmps) + mi(icmp)*sigu
122      continue
! ------ PRESSION
        sigu = zr(jsigu-1+78+72+icmps)
        sij(icmps) = sij(icmps) + pi*sigu
120  end do
!
    if (seisme) then
        call rc32s0('PMPB', mi, pi, mse, zr(jsigu+78),&
                    nbinst, sth, pmpbij)
    else
        call rc32st(sij, nbinst, sth, pmpbij)
    endif
    pmpb = max( pmpbij, pmpb )
!
    call jedema()
end subroutine
