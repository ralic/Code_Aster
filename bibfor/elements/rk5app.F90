subroutine rk5app(nbeq, vparam, dtemps, yinit, dyinit,&
                  rkfct, solu, decoup)
    implicit none
    integer :: nbeq
    real(kind=8) :: vparam(*), dtemps, yinit(nbeq), dyinit(nbeq), solu(3*nbeq)
    logical :: decoup
    external rkfct
!
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
! person_in_charge: jean-luc.flejou at edf.fr
! --------------------------------------------------------------------------------------------------
!
!          INTÉGRATION PAR MÉTHODE DE RUNGE KUTTA D'ORDRE 5
!
!  intégration des équations
!  dérivée au temps t + dt
!  calcul de l'erreur pour chaque équation
!
! --------------------------------------------------------------------------------------------------
!
! IN
!  nbeq        : nombre d'équations
!  vparam      : paramètres
!  dtemps      : incrément de temps
!  yinit       : valeurs à t
!  dyinit      : vitesse à t
!  rkfct       : subroutine du système à intégrer
!
! OUT
!  solu        : résultat de l'intégration
!        solu(1:nbeq)            : variables intégrées
!        solu(nbeq+1:2*nbeq)     : dérivées a t+dt
!        solu(2*nbeq+1:3*nbeq)   : erreur
!  decoup      : force le découpage
!
! --------------------------------------------------------------------------------------------------
!
!   niveau du runge-kutta
    integer :: nivrk, nn, niv, ii
    parameter  (nivrk=6)
    real(kind=8) :: yy(nbeq), rr(nbeq, nivrk)
!   tables de cash-karp
    real(kind=8) :: tabc(nivrk), tabe(nivrk), tabb(nivrk, nivrk)
!
!   initialisation des tables de cash-karp
!   taba : ( 0.0 , 0.2 , 0.3, 0.6, 1.0 , 7/8 ). remarque  taba(i)= somme( tabb(i,:) )
    data tabc/9.78835978835978781642524d-02, 0.0d0, 4.02576489533011283583619d-01, &
          2.10437710437710451261140d-01, 0.0d0, 2.89102202145680386990989d-01/
    data tabe/1.02177372685185188783130d-01, 0.0d0, 3.83907903439153430635855d-01, &
          2.44592737268518517490534d-01, 1.93219866071428561515866d-02, 0.25d0/
!
    tabb(:,:) = 0.0d0
    tabb(2, 1) = 0.20d0
    tabb(3, 1:2) = (/ 3.0d0/40.0d0, 9.0d0/40.0d0  /)
    tabb(4, 1:3) = (/ 3.0d0/10.0d0, -9.0d0/10.0d0, 6.0d0/5.0d0 /)
    tabb(5, 1:4) = (/ -11.0d0/54.0d0, 2.50d0, -70.0d0/27.0d0, 35.0d0/27.0d0 /)
    tabb(6, 1:5) = (/ 1631.0d0/55296.0d0, 175.0d0/512.0d0, 575.0d0/13824.0d0, &
                      44275.0d0/110592.0d0, 253.0d0/4096.0d0 /)
!
!   niveaux de RK
    do niv = 1, nivrk
        do ii = 1, nbeq
            yy(ii) = yinit(ii)
            do nn = 1, niv - 1
                yy(ii) = yy(ii) + tabb(niv,nn)*dtemps*rr(ii,nn)
            enddo
        enddo
        call rkfct(vparam, nbeq, yy, dyinit, rr(1, niv),&
                   decoup)
        if (decoup) goto 999
    enddo
!
    do ii = 1, nbeq
!       intégration
        solu(ii) = yinit(ii)
!       dérivée à t+dt
        solu(nbeq+ii) = rr(ii,5)
!       erreur
        solu(2*nbeq+ii) = 0.0d0
        do niv = 1, nivrk
            solu(ii) = solu(ii) + tabc(niv) * rr(ii,niv)*dtemps
            solu(2*nbeq+ii) = solu(2*nbeq+ii) + (tabc(niv)- tabe(niv))*rr(ii,niv)* dtemps
        enddo
    enddo
!
999 continue
end subroutine
