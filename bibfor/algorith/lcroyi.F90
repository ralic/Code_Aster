function lcroyi()
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
    implicit none
    real(kind=8) :: lcroyi
!
! *********************************************************************
! *       INTEGRATION DE LA LOI DE ROUSSELIER LOCAL                   *
! *  CALCUL DES BORNES INF ET SUP DE LA FONCTION S(Y) QUAND S(0)<0    *
! *  ET RESOLUTION DE S(Y)=0                                          *
! *  PAR UNE METHODE DE NEWTON AVEC BORNES CONTROLEES ET DICHOTOMIE   *
! *  - LA BORNE SUPERIEURE EST TELLE QUE YSUP=LOG(SIG1*FONC/RM)       *
! *  - LA BORNE INFERIEURE EST TELLE QUE YINF =LOG(SIG1*G/R(PM+DPSUP) *
! *********************************************************************
!
! ----------------------------------------------------------------------
!  COMMON LOI DE COMPORTEMENT ROUSSELIER
!
    include 'asterfort/lcrofs.h'
    include 'asterfort/rcfonc.h'
    include 'asterfort/u2mess.h'
    integer :: itemax, jprolp, jvalep, nbvalp
    real(kind=8) :: prec, young, nu, sigy, sig1, rousd, f0, fcr, acce
    real(kind=8) :: pm, rpm, fonc, fcd, dfcddj, dpmaxi
    common /lcrou/ prec,young,nu,sigy,sig1,rousd,f0,fcr,acce,&
     &               pm,rpm,fonc,fcd,dfcddj,dpmaxi,&
     &               itemax, jprolp, jvalep, nbvalp
! ----------------------------------------------------------------------
!  COMMON GRANDES DEFORMATIONS CANO-LORENTZ
!
    integer :: ind1(6), ind2(6)
    real(kind=8) :: kr(6), rac2, rc(6)
    real(kind=8) :: lambda, mu, deuxmu, unk, troisk, cother
    real(kind=8) :: jm, dj, jp, djdf(3, 3)
    real(kind=8) :: etr(6), dvetr(6), eqetr, tretr, detrdf(6, 3, 3)
    real(kind=8) :: dtaude(6, 6)
!
    common /gdclc/&
     &          ind1,ind2,kr,rac2,rc,&
     &          lambda,mu,deuxmu,unk,troisk,cother,&
     &          jm,dj,jp,djdf,&
     &          etr,dvetr,eqetr,tretr,detrdf,&
     &          dtaude
! ----------------------------------------------------------------------
!
    integer :: iter
    real(kind=8) :: y, e, dp, rp, s, ds, yinf, ysup, r8bid, pente, aire
!
!
! 1 - CALCUL DU MAJORANT
!
    e = sig1*fonc / rpm
    ysup = log(e)
    y = ysup
    call lcrofs(y, dp, s, ds)
!
! 2 - CALCUL DU MINORANT
!
    call rcfonc('V', 1, jprolp, jvalep, nbvalp,&
                r8bid, r8bid, r8bid, pm+dp, rp,&
                pente, aire, r8bid, r8bid)
    e = sig1*fonc / rp
    yinf = max(0.d0, log(e))
!
! 3 - RESOLUTION PAR UNE METHODE DE NEWTON ENTRE LES BORNES
!
    do 10 iter = 1, itemax
        if (abs(s)/sigy .le. prec) goto 100
!
        y = y - s/ds
        if (y .le. yinf .or. y .ge. ysup) y=(yinf+ysup)/2
!
        call lcrofs(y, dp, s, ds)
        if (s .le. 0) yinf = y
        if (s .ge. 0) ysup = y
10  end do
    call u2mess('F', 'ALGORITH3_55')
!
!
100  continue
    lcroyi = y
end function
