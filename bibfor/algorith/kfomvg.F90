subroutine kfomvg(pr, sr, m, n, usm,&
                  usn, s1, krl, krg, dklds,&
                  dkgds)
! ======================================================================
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
! ======================================================================
!
! KFOMVG : CALCUL DES PERMEABILITES RELATIVES
!         PAR FONCTION MUALEM-VAN-GENUCHTEN
!
    implicit      none
!
! IN
    real(kind=8) :: pr, sr, m, n, usm, usn, s1
! OUT
    real(kind=8) :: krl, krg, dklds, dkgds
! LOCAL
    real(kind=8) :: umsr, usumsr, a
!
    umsr=(1.d0-sr)
    usumsr=1.d0/umsr
    krl=(s1**0.5d0)*((1.d0-(1.d0-s1**usm)**m)**2.d0)
    krg=((1.d0-s1)**0.5d0)*((1.d0-s1**usm)**(2.d0*m))
    a=1.d0-s1**usm
    dklds=usumsr*(krl/(2.d0*s1)+2.d0*((s1)**0.5d0)*(1.d0-a**m)&
     &       *(a**(m-1.d0))*(s1**(usm-1.d0)))
    a=1.d0-s1
    dkgds=usumsr*(-krg/(2.d0*a)-2.d0*(a**0.5d0)*&
     &         ((1.d0-s1**usm)**(2.d0*m-1.d0))*(s1**(usm-1.d0)))
!
!
end subroutine
