subroutine gddyng(kp, nno, en, x0sk, rmkm1,&
                  rmk, omkm1, ompkm1, omk, ompk,&
                  x0sec, rgmkm, rgmk, omgkm, ompgkm,&
                  omgk, ompgk)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! FONCTION: POUR UN ELEMENT DE POUTRE EN GRAND DEPLACEMENT, CALCULE
!           CERTAINES GRANDEURS DYNAMIQUES AUX POINTS DE GAUSS.
!
!     IN  : KP        : NUMERO DU POINT DE GAUSS
!           NNO       : NOMBRE DE NOEUDS
!           EN        : FONCTIONS DE FORME
!           X0SK      : DERIVEE SECONDE ACTUALISEE DU DEPLACEMENT
!           RMKM1     : INCREMENT DE ROTATION ENTRE L'INSTANT N  ET
!                       L'ITER. I   DE L'INSTANT N+1
!           RMK       : INCREMENT DE ROTATION ENTRE L'INSTANT N  ET
!                       L'ITER. I+1 DE L'INSTANT N+1
!           OMKM1     : VITESSE      ANGULAIRE, A L'ITERATION I
!           OMPKM1    : ACCELERATION ANGULAIRE, A L'ITERATION I
!           OMK       : VITESSE      ANGULAIRE, A L'ITERATION I+1
!           OMPK      : ACCELERATION ANGULAIRE, A L'ITERATION I+1
!
!     OUT, AU POINT DE GAUSS NUMERO KP:
!           X0SEC     : DERIVEE SECONDE ACTUALISEE DU DEPLACEMENT
!           RGMKM     : INCREMENT DE ROTATION ENTRE L'INSTANT N  ET
!                       L'ITER. I   DE L'INSTANT N+1
!           RGMK      : INCREMENT DE ROTATION ENTRE L'INSTANT N  ET
!                       L'ITER. I+1 DE L'INSTANT N+1
!           OMGKM     : VITESSE      ANGULAIRE, A L'ITERATION I
!           OMPGKM    : ACCELERATION ANGULAIRE, A L'ITERATION I
!           OMGK      : VITESSE      ANGULAIRE, A L'ITERATION I+1
!           OMPGK     : ACCELERATION ANGULAIRE, A L'ITERATION I+1
! ------------------------------------------------------------------
    implicit none
    real(kind=8) :: en(3, 2), x0sk(3, 3), rmkm1(3, 3), rmk(3, 3), omkm1(3, 3)
    real(kind=8) :: ompkm1(3, 3), omk(3, 3), ompk(3, 3), x0sec(3), rgmkm(3)
    real(kind=8) :: rgmk(3), omgkm(3), ompgkm(3), omgk(3), ompgk(3)
!
!
!-----------------------------------------------------------------------
    integer :: kc, kp, ne, nno
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    zero = 0.d0
    do 1 kc = 1, 3
        x0sec (kc) = zero
        rgmkm (kc) = zero
        rgmk(kc) = zero
        omgkm (kc) = zero
        ompgkm (kc) = zero
        omgk (kc) = zero
        ompgk (kc) = zero
 1  end do
    do 12 kc = 1, 3
        do 11 ne = 1, nno
            x0sec (kc) = x0sec (kc) + en(ne,kp)*x0sk (kc,ne)
            rgmkm (kc) = rgmkm (kc) + en(ne,kp)*rmkm1 (kc,ne)
            rgmk (kc) = rgmk (kc) + en(ne,kp)*rmk (kc,ne)
            omgkm (kc) = omgkm (kc) + en(ne,kp)*omkm1 (kc,ne)
            ompgkm (kc) = ompgkm (kc) + en(ne,kp)*ompkm1 (kc,ne)
            omgk (kc) = omgk (kc) + en(ne,kp)*omk (kc,ne)
            ompgk (kc) = ompgk (kc) + en(ne,kp)*ompk(kc,ne)
11      end do
12  end do
end subroutine
