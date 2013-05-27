subroutine mudirx(nbsom, geom, idim, al1, al2,&
                  axe, ang)
    implicit none
!
    include 'asterc/r8pi.h'
    include 'asterfort/assert.h'
    real(kind=8) :: geom(idim, nbsom), axe(3, 3), ang(2), al1, al2
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!.......................................................................
! .                                                                    .
! .  - FONCTION REALISEE : CALCULE LES COSINUS DIRECTEURS DE LA MATRICE.
! .                        DE PASSAGE DU REPERE DE L'ELEMENT AU REPERE .
! .                        DE REFERENCE AINSI QUE LES 3 DIRECTIONS     .
! .                        NORMEES DU REPERE DE L'ELEMENT.             .
! .  - ARGUMENTS :                                                     .
! .                                                                    .
! .      ENTREE :  NBSOM  --> NB DE SOMMETS DE L'ELEMENT (3 OU 4)      .
! .                GEOM   --> TABLEAU DES COORDONNEES DES SOMMETS      .
! .                IDIM   --> DIMENSION DE GEOM (2 OU 3)               .
! .                AL1    --> ANGLE 1 DU REPERE DE REFERENCE           .
! .                AL2    --> ANGLE 2 DU REPERE DE REFERENCE           .
! .      SORTIE :                                                      .
! .                AXE    <-- 3 DIRECTIONS NORMEES DU REPERE DE        .
! .                           L'ELEMENT                                .
! .                ANG    <-- COSINUS ET SINUS DE L'ANGLE (REF,ELEM)   .
! .                                                                    .
! .  - ROUTINES APPELEES:                                              .
! .                                                                    .
! .    JEFINI                                                          .
!.......................................................................
!
    real(kind=8) :: xi1, xi2, xi3, x12, y12, z12, x13, y13, z13, x24, y24, z24
    real(kind=8) :: s
    real(kind=8) :: pjxi1, pjxi2, pjxi3, s1, s2, s3, psxin, coepi
!
!-----------------------------------------------------------------------
    integer :: idim, nbsom
!-----------------------------------------------------------------------
    coepi=r8pi()/180.d0
    if (nbsom .ne. 3 .and. nbsom .ne. 4) then
        call assert(.false.)
    endif
    if (idim .ne. 2 .and. idim .ne. 3) then
        call assert(.false.)
    endif
    xi1=cos(coepi*al2)*cos(coepi*al1)
    xi2=cos(coepi*al2)*sin(coepi*al1)
    xi3=sin(coepi*al2)
    s=(xi1**2+xi2**2+xi3**2)**0.5d0
    xi1=xi1/s
    xi2=xi2/s
    xi3=xi3/s
    x12=geom(1,2)-geom(1,1)
    y12=geom(2,2)-geom(2,1)
    z12=0.d0
    if (idim .eq. 3) z12=geom(3,2)-geom(3,1)
    s=(x12**2+y12**2+z12**2)**0.5d0
    axe(1,1)=x12/s
    axe(2,1)=y12/s
    axe(3,1)=z12/s
    x13=geom(1,3)-geom(1,1)
    y13=geom(2,3)-geom(2,1)
    z13=0.d0
    if (idim .eq. 3) z13=geom(3,3)-geom(3,1)
    if (nbsom .eq. 3) then
        s1=y12*z13-z12*y13
        s2=z12*x13-x12*z13
        s3=x12*y13-y12*x13
        s=(s1**2+s2**2+s3**2)**0.5d0
        axe(1,3)=s1/s
        axe(2,3)=s2/s
        axe(3,3)=s3/s
    endif
    if (nbsom .eq. 4) then
        x24=geom(1,4)-geom(1,2)
        y24=geom(2,4)-geom(2,2)
        z24=0.d0
        if (idim .eq. 3) z24=geom(3,4)-geom(3,2)
        s1=y13*z24-z13*y24
        s2=z13*x24-x13*z24
        s3=x13*y24-y13*x24
        s=(s1**2+s2**2+s3**2)**0.5d0
        axe(1,3)=s1/s
        axe(2,3)=s2/s
        axe(3,3)=s3/s
    endif
    axe(1,2)=axe(2,3)*axe(3,1)-axe(3,3)*axe(2,1)
    axe(2,2)=axe(3,3)*axe(1,1)-axe(1,3)*axe(3,1)
    axe(3,2)=axe(1,3)*axe(2,1)-axe(2,3)*axe(1,1)
    psxin=xi1*axe(1,3)+xi2*axe(2,3)+xi3*axe(3,3)
    pjxi1=xi1-psxin*axe(1,3)
    pjxi2=xi2-psxin*axe(2,3)
    pjxi3=xi3-psxin*axe(3,3)
    s=(pjxi1**2+pjxi2**2+pjxi3**2)**0.5d0
    pjxi1=pjxi1/s
    pjxi2=pjxi2/s
    pjxi3=pjxi3/s
    ang(1)=pjxi1*axe(1,1)+pjxi2*axe(2,1)+pjxi3*axe(3,1)
    ang(2)=pjxi1*axe(1,2)+pjxi2*axe(2,2)+pjxi3*axe(3,2)
end subroutine
