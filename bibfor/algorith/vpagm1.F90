function vpagm1(poro)
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
! aslint: disable=
    implicit none
    include 'asterfort/ggpgmo.h'
    include 'asterfort/vpagm2.h'
    include 'asterfort/zerofr.h'
    integer :: iret
    real(kind=8) :: poro
    real(kind=8) :: vpagm1
!
!DEB
!---------------------------------------------------------------
!     FONCTION A(X)  :  CAS DE LA LOI GATT-MONERIE
!---------------------------------------------------------------
! IN  X     :R: ARGUMENT RECHERCHE LORS DE LA RESOLUTION SCALAIRE
!---------------------------------------------------------------
!     L'ETAPE LOCALE DU CALCUL VISCOPLASTIQUE (CALCUL DU TERME
!       ELEMENTAIRE DE LA MATRICE DE RIGIDITE TANGENTE) COMPORTE
!       LA RESOLUTION D'UNE EQUATION SCALAIRE NON LINEAIRE:
!
!           A(X) = 0
!
!       (DPC,SIELEQ,DEUXMU,DELTAT JOUENT LE ROLE DE PARAMETRES)
!---------------------------------------------------------------
!FIN
!     COMMON POUR LES PARAMETRES DES LOIS VISCOPLASTIQUES
    common / nmpavp /sieleq,deuxmu,troisk,deltat,tschem,prec,theta,&
     &                 niter
    real(kind=8) :: sieleq, deuxmu, troisk, deltat, tschem, prec, theta, niter
!
!     COMMON POUR LES PARAMETRES DE LA LOI GATT-MONERIE
    common / nmpagm /ak1,ak2,xn1,xn2,expa1,expa2,expab1,expab2,a1,a2,&
     &                 b1,b2,xw,xq,xh,sige,sigh,sigh0,porom,sgd
    real(kind=8) :: ak1, ak2, xn1, xn2, expa1, expa2, expab1, expab2, a1, a2, b1
    real(kind=8) :: b2, xw, xq, xh, sige, sigh, sigh0, porom, sgd
!
    real(kind=8) :: a0, xap, x, g, devpkk, dgdst, dgdev
    integer :: ibid
!
    x = poro - porom
    sigh = sigh0 - (troisk/3.d0)*(x/(1.d0-poro))
!
!     PREMIER POTENTIEL
    a1 = (poro**(2.d0/(xn1+1))) *(xn1*(1.d0 - poro**expa1))**(-1.d0*expab1)
    b1 = (1.d0+((2.d0/3.d0)*poro))/((1.d0-poro)**expab1)
!     SECOND POTENTIEL
    a2 = (poro**(2.d0/(xn2+1))) *(xn2*(1.d0 - poro**expa2))**(-1.d0*expab2)
    b2 = (1.d0+((2.d0/3.d0)*poro))/((1.d0-poro)**expab2)
!
    if (poro .lt. 0.d0) write(*,*) 'A2', a2, porom, poro
!
    a0 = - sieleq
    xap = sieleq
    xap = xap - sieleq*1.d-12
    if (abs(a0) .le. prec) then
        sige = 0.d0
    else
        call zerofr(0, 'DEKKER2', vpagm2, 0.d0, xap,&
                    prec*2, int(niter), sige, iret, ibid)
    endif
    call ggpgmo(sige, sigh, theta, deuxmu, g,&
                devpkk, dgdst, dgdev, tschem)
    vpagm1 = x - (1.d0-porom-x)*3.d0*devpkk*deltat
    vpagm1 = vpagm1*sgd
!      VPAGM1 = X
!
end function
