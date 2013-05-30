subroutine rc32sa(typz, nommat, mati, matj, snpq,&
                  spij, typeke, spmeca, spther, kemeca,&
                  kether, saltij, sm, fuij)
    implicit   none
    include 'asterc/r8maem.h'
    include 'asterc/r8vide.h'
    include 'asterfort/limend.h'
    include 'asterfort/prccm3.h'
    include 'asterfort/rcvale.h'
    include 'asterfort/u2mesg.h'
    real(kind=8) :: mati(*), matj(*), snpq, spij(2), saltij(2), sm
    real(kind=8) :: typeke, spmeca(2), spther(2), fuij(2)
    character(len=8) :: nommat
    character(len=*) :: typz
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!     CALCUL DE LA CONTRAINTE EQUIVALENTE ALTERNEE  SALT
!     CALCUL DU FACTEUR D'USAGE ET DE SON CUMUL
!
! IN  : NOMMAT : NOM MATERIAU
! IN  : MATI   : MATERIAU ASSOCIE A L'ETAT STABILISE I
! IN  : MATJ   : MATERIAU ASSOCIE A L'ETAT STABILISE J
! IN  : SNPQ   : AMPLITUDE DE VARIATION DES CONTRAINTES LINEARISEES
! IN  : SPIJ   : AMPLITUDE DE VARIATION DES CONTRAINTES TOTALES
! IN  : TYPEKE    : =-1 SI KE_MECA, =1 SI KE_MIXTE
! IN  : SPMECA   : AMPLITUDE DE VARIATION DES CONTRAINTES MECANIQUES
! IN  : SPTHER   : AMPLITUDE DE VARIATION DES CONTRAINTES THERMIQUES
! OUT : SALTIJ : AMPLITUDE DE CONTRAINTE ENTRE LES ETATS I ET J
! OUT : FUIJ : FACTEUR D USAGE POUR LA COMBINAISON ENTRE I ET J
!
!     ------------------------------------------------------------------
!
    real(kind=8) :: e, ec, para(3), m, n, nadm, saltm, salth, kemeca, kether
    real(kind=8) :: kethe1, valr(2)
    character(len=8) :: kbid
    integer :: icodre
    logical :: endur
! DEB ------------------------------------------------------------------
!
! --- LE MATERIAU
!
    e = min ( mati(1) , matj(1) )
    ec = max ( mati(4) , matj(4) )
    sm = min ( mati(5) , matj(5) )
    m = max ( mati(6) , matj(6) )
    n = max ( mati(7) , matj(7) )
!
    para(1) = m
    para(2) = n
    para(3) = ec / e
!
    saltij(1) = 0.d0
    saltij(2) = 0.d0
    fuij(1) = 0.d0
    fuij(2) = 0.d0
!
! --- CALCUL DU COEFFICIENT DE CONCENTRATION ELASTO-PLASTIQUE KE
! --- CALCUL DE LA CONTRAINTE EQUIVALENTE ALTERNEE SALT
! --- CALCUL DU NOMBRE DE CYCLES ADMISSIBLE NADM
!
    if (typeke .lt. 0.d0) then
        call prccm3(nommat, para, sm, snpq, spij(1),&
                    kemeca, saltij(1), nadm)
        fuij(1) = 1.d0 / nadm
        if (typz .eq. 'COMB') then
            call prccm3(nommat, para, sm, snpq, spij(2),&
                        kemeca, saltij(2), nadm)
            fuij(2) = 1.d0 / nadm
        endif
        kether = r8vide()
    else
!
! --- CAS KE_MIXTE
!
        kethe1 = 1.86d0*(1.d0-(1.d0/(1.66d0+snpq/sm)))
        kether = max(1.d0,kethe1)
        call prccm3(nommat, para, sm, snpq, spmeca(1),&
                    kemeca, saltm, nadm)
        salth = 0.5d0 * para(3) * kether * spther(1)
        saltij(1) = saltm + salth
!
! --- CALCUL DU NOMBRE DE CYCLES ADMISSIBLE NADM : TR. 1
!
        call limend(nommat, saltij(1), 'WOHLER', kbid, endur)
        if (endur) then
            nadm=r8maem()
        else
            call rcvale(nommat, 'FATIGUE', 1, 'SIGM    ', saltij(1),&
                        1, 'WOHLER  ', nadm, icodre, 2)
            if (nadm .lt. 0) then
                valr (1) = saltij(1)
                valr (2) = nadm
                call u2mesg('A', 'POSTRELE_61', 0, ' ', 0,&
                            0, 2, valr)
            endif
        endif
        fuij(1) = 1.d0 / nadm
!
        if (typz .eq. 'COMB') then
            call prccm3(nommat, para, sm, snpq, spmeca(2),&
                        kemeca, saltm, nadm)
            salth = 0.5d0 * para(3) * kether * spther(2)
            saltij(2) = saltm + salth
! --- CALCUL DU NOMBRE DE CYCLES ADMISSIBLE NADM : TR. 2
!
            call limend(nommat, saltij(2), 'WOHLER', kbid, endur)
            if (endur) then
                nadm=r8maem()
            else
                call rcvale(nommat, 'FATIGUE', 1, 'SIGM    ', saltij(2),&
                            1, 'WOHLER  ', nadm, icodre, 2)
                if (nadm .lt. 0) then
                    valr (1) = saltij(1)
                    valr (2) = nadm
                    call u2mesg('A', 'POSTRELE_61', 0, ' ', 0,&
                                0, 2, valr)
                endif
            endif
            fuij(2) = 1.d0 / nadm
        endif
!
    endif
!
end subroutine
