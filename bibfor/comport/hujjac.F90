subroutine hujjac(mod, nmat, mater, indi, deps,&
                  nr, yd, yf, ye, nvi,&
                  vind, vins, vinf, drdy, bnews,&
                  mtrac, iret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: alexandre.foucault at edf.fr
! aslint: disable=W1306
    implicit none
!     ----------------------------------------------------------------
!     CALCUL DU JACOBIEN DU SYSTEME NL POUR MODELE DE HUJEUX
!     ----------------------------------------------------------------
!     IN   MOD    :  TYPE DE MODELISATION
!          MATER  :  DONNEES MATERIAU
!          NMAT   :  DIMENSION TABLEAU DONNEES MATERIAU
!          INDI   :  MECANISMES POTENTIELLEMENT ACTIFS
!          DEPS   :  INCREMENT DEFORMATION
!          NR     :  DIMENSION DU VECTEUR INCONNU
!          YD     :  VECTEUR SOLUTION A T
!          YF     :  VECTEUR SOLUTION A T+DT?
!          YE     :  VECTEUR SOLUTION APRES LCINIT
!          NVI    :  NOMBRE DE VARIABLES INTERNES
!          VIND   :  VARIABLES INTERNES A T
!          VINS   :  VARIABLES INTERNES D'ORIGINE
!          BNEWS  :  INDICATEUR LIES A LA TRACTION
!     OUT  DRDY   :  JACOBIEN DU SYSTEME NL A RESOUDRE
!          BNEWS  :  INDICATEUR LIES A LA TRACTION
!          MTRAC  :  INDICATEUR LIE A LA TRACTION
!          IRET   :  CODE RETOUR (>0 -> PB)
!     ----------------------------------------------------------------
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/hujjid.h"
#include "asterfort/hujprj.h"
#include "asterfort/lceqvn.h"
    integer :: indi(7), nr, nvi, iret, nmat
    character(len=8) :: mod
    real(kind=8) :: mater(nmat, 2), deps(6), yd(nr), yf(nr), vind(nvi)
    real(kind=8) :: drdy(nr, nr), vins(nr), ye(nr), vinf(nvi)
    aster_logical :: bnews(3), mtrac
!
    aster_logical :: prox(4), proxc(4), tracti, probt, modif, neglam(3)
    real(kind=8) :: r(nr), ydt(nr), yft(nr), dev(3), pf, qf
    real(kind=8) :: pref, e0, ptrac, rtrac, un, deux, zero, yet(nr), prob(3)
    real(kind=8) :: matert(22, 2)
    integer :: nbmeca, nbmect, i, j, ndt, msup(2), k
!
    parameter     (ndt  = 6   )
    parameter     (zero = 0.d0)
    parameter     (un   = 1.d0)
    parameter     (deux = 2.d0)
!     ----------------------------------------------------------------
! --- INITIALISATION DE LA JACOBIENNE A ZERO
    do 10 i = 1, nr
        do 20 j = 1, nr
            drdy(i,j) = zero
 20     continue
 10 end do
!
! --- PROPRIETES MATERIAU
    pref = mater(8,2)
    e0 = mater(1,1)
    rtrac = abs(pref*1.d-6)
    ptrac = mater(21,2)
!
! --- REDIMENSIONNEMENT DE YD ET YF POUR S'ADAPTER A HUJJID
! --- COPIE A PARTIR DU TRAITEMENT DE HUJMID
    call lceqvn(nr, yd, ydt)
    call lceqvn(nr, yf, yft)
    call lceqvn(nr, ye, yet)
!
    do 30 i = 1, 6
        ydt(i) = yd(i)*e0
        yft(i) = yf(i)*e0
        yet(i) = ye(i)*e0
 30 end do
!
    nbmeca = 0
    do 40 k = 1, 7
        if (indi(k) .gt. 0) then
            if (indi(k) .le. 8) nbmeca = nbmeca + 1
        endif
 40 end do
!
    nbmect = nbmeca
    do 50 i = 1, 7
        if (indi(i) .gt. 8) then
            nbmect = nbmect + 1
        endif
 50 end do
!
    do 60 i = 1, nbmeca
        ydt(ndt+1+i) = yd(ndt+1+i)*e0/abs(pref)
        yft(ndt+1+i) = yf(ndt+1+i)*e0/abs(pref)
        yet(ndt+1+i) = ye(ndt+1+i)*e0/abs(pref)
 60 end do
!
    do 70 i = 1, 22
        matert(i,1) = mater(i,1)
        matert(i,2) = mater(i,2)
 70 end do
!
    call hujjid(mod, matert, indi, deps, prox,&
                proxc, ydt, yft, vind, r,&
                drdy, iret)
!
! ------------------------------------------------------------
! ---> SI ECHEC DANS LE CALCUL DE LA JACOBIENNE DR/DY
! ---  ON VERIFIE LES ETATS DE CONTRAINTES DE YF A L'ITERATION
! ---  DE CORRECTION PRECEDENTE. SI TRACTION IL Y A, ON TRAITE
! ---  LE PB
! ------------------------------------------------------------
    tracti = .false.
    probt = .false.
    if (iret .eq. 1) then
        iret = 3
        do 80 i = 1, 3
            if (prox(i)) then
                prob(i) = un
                probt = .true.
            else if (proxc(i)) then
                prob(i) = deux
                probt = .true.
            endif
 80     continue
        do 90 i = 1, 3
            call hujprj(i, yft, dev, pf, qf)
            if (((rtrac+pf-ptrac)/abs(pref)) .ge. -r8prem()) then
                tracti = .true.
            endif
 90     continue
    endif
!
    if (probt) then
        call lceqvn(nvi, vins, vind)
        do 100 i = 1, 3
            if (prob(i) .eq. un) then
                vind(i+4) = mater(18,2)
                vind(23+i) = un
                vind(27+i) = zero
                vind(4*i+5) = zero
                vind(4*i+6) = zero
                vind(4*i+7) = zero
                vind(4*i+8) = zero
                vind(5*i+31) = zero
                vind(5*i+32) = zero
                vind(5*i+33) = zero
                vind(5*i+34) = zero
                vind(5*i+35) = mater(18,2)
            else if (prob(i).eq.deux) then
                vind(27+i) = zero
            endif
100     continue
        iret = 2
        probt = .false.
!
! --- Y AVAIT IL UN MECANISME CYCLIQUE DEJA DESACTIVE
!     DURANT CETTE TENTATIVE?
        msup(1) = 0
        msup(2) = 0
        j = 0
        do 110 i = 5, 8
            if ((vind(23+i).ne.vins(23+i)) .and. (vind(23+i).eq.zero)) then
                j = j+1
                msup(j) = i
            endif
110     end do
! --- MECANISME CYCLIQUE A DESACTIVE
! --- ET DEJA DESACTIVE ANTERIEUREMENT
        if (j .ne. 0) then
            do 120 i = 1, j
                vind(23+msup(i)) = zero
120         continue
        endif
!
        call lceqvn(nvi, vind, vinf)
!
    endif
!
    if (tracti) then
        call lceqvn(nvi, vins, vind)
        modif = .false.
        do 130 i = 1, nbmect
            if (yet(ndt+1+nbmeca+i) .eq. zero) then
                modif = .true.
                if (indi(i) .le. 8) then
                    if (indi(i) .lt. 5) then
                        if ((abs(vind(4*indi(i)+5)).gt.r8prem()) .or.&
                            (abs(vind(4*indi(i)+6)).gt.r8prem())) then
                            vind(23+indi(i)) = -un
                        else
                            vind(23+indi(i)) = zero
                        endif
                    else
                        vind(23+indi(i)) = zero
                    endif
                else
                    bnews(indi(i)-8) = .true.
                    neglam(indi(i)-8) = .true.
                endif
                tracti = .false.
            endif
130     continue
!
        do 140 i = 1, nbmect
            if (indi(i) .eq. 8) then
                vind(23+indi(i)) = zero
                modif = .true.
            endif
140     continue
!
        mtrac = .false.
        do 150 i = 1, 3
! --- ON NE DOIT PAS REACTIVE UN MECANISME DE TRACTION QUI DONNE
!     COMME PREDICTEUR UN MULTIPLICATEUR PLASTIQUE NEGATIF
            if (.not.neglam(i)) then
                call hujprj(i, yft, dev, pf, qf)
! ----------------------------------------------------
! ---> ACTIVATION MECANISMES DE TRACTION NECESSAIRES
! ----------------------------------------------------
                if (((pf+deux*rtrac-ptrac)/abs(pref)) .gt. -r8prem()) then
                    bnews(i) = .false.
                    if(.not.modif)mtrac = .true.
                endif
            endif
150     continue
        call lceqvn(nvi, vind, vinf)
        iret = 2
    endif
!
!
end subroutine
