subroutine hujini(mod, nmat, mater, intg, deps,&
                  nr, yd, nvi, vind, sigd,&
                  sigf, bnews, mtrac, dy, indi,&
                  iret)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: alexandre.foucault at edf.fr
    implicit none
!     ----------------------------------------------------------------
!     CALCUL DE LA SOLUTION INITIALE ESSAI DY = ( DSIG DVIN )
!     ----------------------------------------------------------------
!     IN   MOD    :  TYPE DE MODELISATION
!          NMAT   :  DIMENSION TABLEAU MATERIAU
!          MATER  :  PROPRIETES MATERIAU
!          INTG   :  NOMBRE DE TENTATIVES D'INTEGRATION
!          DEPS   :  INCREMENT DEFORMATION TOTALE
!          NR     :  DIMENSION SYSTEME NL A RESOUDRE
!          YD     :  VECTEUR INITIAL A T
!          NVI    :  NOMBRE DE VARIABLES INTERNES
!          VIND   :  VARIABLES INTERNES A T
!          SIGD   :  ETAT DE CONTRAINTES A T
!          SIGF   :  PREDICTION ELASTIQUE
!          BNEWS  :  GESTION TRACTION AVEC HUJEUX
!          MTRAC  :  GESTION TRACTION AVEC HUJEUX (BIS)
!          IRET   :  IRET = 2 - RELANCE DU PROCESSUS DE RESOLUTION
!     OUT  DY     :  INCREMENT INITIAL SUR VECTEUR SOLUTION
!          INDI   :  MECANISMES POTENTIELLEMENT ACTIFS
!          NR     :  NR MIS A JOUR SI TRACTION PRESENTE
!          IRET   :  IRET = 0 (OK) - 3 (NON CVG)
!     ----------------------------------------------------------------
#include "asterf_types.h"
#include "asterfort/hujiid.h"
    character(len=8) :: mod
    integer :: nr, nvi, iret, indi(7), intg, nmat
    real(kind=8) :: mater(nmat, 2), deps(6), yd(18), vind(nvi), sigf(6)
    real(kind=8) :: dy(18), sigd(6)
    aster_logical :: bnews(3), mtrac
!
    real(kind=8) :: i1f, dsig(6), zero, un, trois, pref, e0, matert(22, 2)
    aster_logical :: loop, nodef
    integer :: nbmeca, nbmect, i, ii, ndt, ndi, indis(7), diff
!
    parameter     (ndi   = 3   )
    parameter     (ndt   = 6   )
    parameter     (zero  = 0.d0)
    parameter     (un    = 1.d0)
    parameter     (trois = 3.d0)
!     ----------------------------------------------------------------
!--------------------------
! ---- PROPRIETES MATERIAU
! -------------------------
    pref = mater(8,2)
    e0 = mater(1,1)
!
! --- GESTION DES BOUCLES
    if (iret .eq. 2) then
        loop = .true.
        do 10 i = 1, 7
            indis(i) = indi(i)
 10     continue
    else
        loop = .false.
        do 20 i = 1, 7
            indis(i) = 0
 20     continue
    endif
!
    iret = 0
!
! --- PREPARATION DE L'APPEL A HUJIID (ROUTINE DE L'ALGO SPECIFIQUE)
  1 continue
!
    if (iret .eq. 3) goto 999
!
! ---  INITIALISATION VECTEUR D'INDICE INDI(I=1,7)
    do 30 i = 1, 7
        indi(i) = 0
 30 end do
!
! ---  DEFINITION DU NOMBRE DE MECANISMES POTENTIELS ACTIFS
    nbmeca = 0
    do 40 i = 1, 8
        if (vind(23+i) .eq. un) nbmeca = nbmeca + 1
 40 end do
!
! --- REMPLISSAGE DES MECANISMES POTENTIELLEMENT ACTIFS
!
    ii = 1
    do 50 i = 1, 8
        if (vind(23+i) .eq. un) then
!
            if (i .ne. 4) then
                indi(ii) = i
                yd(ndt+1+ii) = vind(i)
                yd(ndt+1+nbmeca+ii) = zero
                ii = ii + 1
            else
                indi(nbmeca) = i
                yd(ndt+1+nbmeca) = vind(i)
                yd(ndt+1+2*nbmeca) = zero
            endif
!
        endif
 50 end do
!
! --- REDIMENSIONNEMENT DE YD POUR S'ADAPTER A HUJIID
! --- COPIE A PARTIR DU TRAITEMENT DE HUJMID
    do 60 i = 1, 6
        yd(i) = yd(i)*e0
 60 end do
!
!
! --- PREPARATION DE L'INCREMENT DE CONTRAINTES
!
    diff = 0
    do 70 i = 1, 7
        diff = diff + indi(i)-indis(i)
 70 end do
    if ((diff.eq.0) .and. (nbmeca.eq.1)) loop=.false.
!
    if (loop) then
        do 80 i = 1, ndt
            dsig(i) = sigf(i) - sigd(i)
 80     continue
    else
        do 90 i = 1, ndt
            dsig(i) = zero
 90     continue
    endif
!
    i1f = (sigf(1)+sigf(2)+sigf(3))/trois
!
!
! --- APPEL A HUJIID
!
    do 100 i = 1, 22
        matert(i,1) = mater(i,1)
        matert(i,2) = mater(i,2)
100 end do
!
    call hujiid(mod, matert, indi, deps, i1f,&
                yd, vind, dy, loop, dsig,&
                bnews, mtrac, iret)
!
!
! --- CONTROLE SUR LA SOLUTION INITIALE PROPOSEE
!
    nbmect = nbmeca
    do 110 i = 1, 7
        if (indi(i) .gt. 8) then
            nbmect = nbmect + 1
        endif
110 end do
!
    nodef = .false.
    if (nbmeca .ne. nbmect) then
        do 120 i = 1, ndi
            if (abs(yd(i)+dsig(i)) .gt. pref**2.d0) nodef = .true.
120     continue
        if (nodef) then
            iret = 3
            if (intg .gt. 5) then
                goto 999
            else
                do 130 i = nbmeca+1, nbmect
                    if (dy(ndt+1+nbmeca+i) .eq. zero) then
                        bnews(indi(i)-8) = .true.
                        iret = 2
                    endif
130             continue
                goto 1
            endif
        endif
    endif
!
! --- REDIMENSIONNEMENT DE YD POUR S'ADAPTER A LCPLNL
! --- COPIE A PARTIR DU TRAITEMENT DE HUJMID
    do 140 i = 1, 6
        yd(i) = yd(i)/e0
        dy(i) = dy(i)/e0
140 end do
!
    do 150 i = 1, nbmeca
        yd(ndt+1+i) = yd(ndt+1+i)/e0*abs(pref)
        dy(ndt+1+i) = dy(ndt+1+i)/e0*abs(pref)
150 end do
!
    nr = ndt+1+nbmeca+nbmect
!
    do 160 i = nr+1, 18
        dy(i) = zero
160 end do
!
999 continue
!
end subroutine
