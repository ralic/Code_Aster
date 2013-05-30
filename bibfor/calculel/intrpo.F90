subroutine intrpo(r, s, t, nno, vh)
!
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
!
    implicit none
! ......................................................................
!     CALCUL DES VALEURS DES FONCTONS D'INTERPOLATION
!     ET DE LEURS DERIVEES AU POINT (R,S,T)
!
! IN    NNO  : NOMBRE DE NOEUDS
! IN    R,S,T: COORDONNES DU POINT CONSIDERE
! OUT    VH  : VALEUR DE LA FONCTION D INTERPOLATION AU POINT
! ......................................................................
!
    real(kind=8) :: vh(50), hr(3), hs(3), ht(3)
    integer :: n27(27)
!
!-----------------------------------------------------------------------
    integer :: i, i1, ih, j, k, nno
    real(kind=8) :: de, hu, r, rm, rp, s, sm
    real(kind=8) :: sp, t, tm, tp, uh, un
!-----------------------------------------------------------------------
    data un,de,hu/1.d0,2.d0,8.d0/
    data n27/1,14,5,12,21,26,4,20,8,9,15,23,13,22,27,11,19,25,2,16,&
     &         6,10,17,24,3,18,7/
!
!----------HEXAEDRE A 8 NOEUDS
!
    if (nno .eq. 8) then
!
        rm=un-r
        rp=un+r
        sm=un-s
        sp=un+s
        tm=un-t
        tp=un+t
        uh=un/hu
!---------------FONCTIONS D INTERPOLATION
        vh(1)=uh*rm*sm*tm
        vh(2)=uh*rp*sm*tm
        vh(3)=uh*rp*sp*tm
        vh(4)=uh*rm*sp*tm
        vh(5)=uh*rm*sm*tp
        vh(6)=uh*rp*sm*tp
        vh(7)=uh*rp*sp*tp
        vh(8)=uh*rm*sp*tp
    endif
!
!----------ELEMENTS A 20 OU A 27 NOEUDS
!
    if (nno .eq. 20 .or. nno .eq. 27) then
        hr(1)=r*(r-un)/de
        hr(2)=un-r*r
        hr(3)=r*(r+un)/de
        hs(1)=s*(s-un)/de
        hs(2)=un-s*s
        hs(3)=s*(s+un)/de
        ht(1)=t*(t-un)/de
        ht(2)=un-t*t
        ht(3)=t*(t+un)/de
!
        i1=0
        do 131 i = 1, 3
            do 131 j = 1, 3
                do 131 k = 1, 3
                    i1=i1+1
                    ih=n27(i1)
                    vh(ih)=hr(i)*hs(j)*ht(k)
131              continue
    endif
!
end subroutine
