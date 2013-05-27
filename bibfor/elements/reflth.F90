subroutine reflth(ang, li, lr)
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
    implicit none
! .                                                                    .
! .  - FONCTION REALISEE : CALCULE LE PASSAGE DES TERMES DE CONDUCTIVITE
! .                        DU REPERE DE REFERENCE AU REPERE DE L'ELEMENT
! .  - ARGUMENTS :                                                     .
! .                                                                    .
! .      ENTREE :                                                      .
! .                   ANG --> COSINUS ET SINUS DE LA MATRICE DE PASSAGE.
! .                   LI  --> DILATATION ELEMENTAIRE REPERE DE REFERENCE
! .      SORTIE :                                                      .
! .                   LR  <-- DILATATION ELEMENTAIRE REPERE DE L'ELEMENT
! .                                                                    .
!.......................................................................
    real(kind=8) :: ang(2), li(3), lr(3)
    real(kind=8) :: f, c, s, c2, s2
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    f=(ang(1)**2+ang(2)**2)**0.5d0
    c=ang(1)/f
    s=ang(2)/f
    c2=c**2
    s2=s**2
    lr(1)=c2*li(1)+s2*li(2)-2.d0*c*s*li(3)
    lr(2)=s2*li(1)+c2*li(2)+2.d0*c*s*li(3)
    lr(3)=c*s*li(1)-c*s*li(2)+(c2-s2)*li(3)
end subroutine
