subroutine vdefgn(nomte, nb2, epais, zic, sigma,&
                  effgtg)
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
!--- INTEGRATION DES CONTRAINTES DANS L'EPAISSEUR D'UNE COUCHE DE COQUE
!--- 3D BASEE SUR LA METHODE DE NEWTON-COTES A 3 POINTS
!--- EN SORTIE: EFFGTG CONTIENT LES 5 EFFORTS ET 2 MOMEMTS AUX NOEUDS
!
    implicit none
!
    character(len=16) :: nomte
    real(kind=8) :: epais, zic, demiep
    real(kind=8) :: sigma(6, *), effgtg(8, *)
    integer :: i, j, nb2
    real(kind=8) :: wnc1, wnc2, wnc3, zic1, zic2, zic3
!-----------------------------------------------------------------------
!
!--- DANS LE CAS DE EFGE_ELNO, ON CALCULE AUX NB1 NOEUDS
!
!--- INITIALISATION
!
    do 5 i = 1, nb2
        do 6 j = 1, 8
            effgtg(j,i)=0.d0
 6      end do
 5  end do
!
    demiep=epais/2.d0
!
    wnc1 = 0.33333333333333D0
    wnc2 = 1.33333333333333D0
    wnc3 = 0.33333333333333D0
!
    zic1 = zic
    zic2 = zic1 + demiep
    zic3 = zic2 + demiep
!
!-- TEST TYPE D'ELEMENT
!
    if (nomte .eq. 'MEC3QU9H') then
!
        effgtg(1,1)=demiep*(wnc1*sigma(1,1)+wnc2*sigma(1,5) +wnc3*&
        sigma(1,9))
        effgtg(1,2)=demiep*(wnc1*sigma(1,2)+wnc2*sigma(1,6) +wnc3*&
        sigma(1,10))
        effgtg(1,3)=demiep*(wnc1*sigma(1,3)+wnc2*sigma(1,7) +wnc3*&
        sigma(1,11))
        effgtg(1,4)=demiep*(wnc1*sigma(1,4)+wnc2*sigma(1,8) +wnc3*&
        sigma(1,12))
        effgtg(1,5)=(effgtg(1,1)+effgtg(1,2))/2.d0
        effgtg(1,6)=(effgtg(1,2)+effgtg(1,3))/2.d0
        effgtg(1,7)=(effgtg(1,3)+effgtg(1,4))/2.d0
        effgtg(1,8)=(effgtg(1,4)+effgtg(1,1))/2.d0
        effgtg(1,9)=(effgtg(1,5)+effgtg(1,6) +effgtg(1,7)+effgtg(1,8))&
        /4.d0
!
        effgtg(2,1)=demiep*(wnc1*sigma(2,1)+wnc2*sigma(2,5) +wnc3*&
        sigma(2,9))
        effgtg(2,2)=demiep*(wnc1*sigma(2,2)+wnc2*sigma(2,6) +wnc3*&
        sigma(2,10))
        effgtg(2,3)=demiep*(wnc1*sigma(2,3)+wnc2*sigma(2,7) +wnc3*&
        sigma(2,11))
        effgtg(2,4)=demiep*(wnc1*sigma(2,4)+wnc2*sigma(2,8) +wnc3*&
        sigma(2,12))
        effgtg(2,5)=(effgtg(2,1)+effgtg(2,2))/2.d0
        effgtg(2,6)=(effgtg(2,2)+effgtg(2,3))/2.d0
        effgtg(2,7)=(effgtg(2,3)+effgtg(2,4))/2.d0
        effgtg(2,8)=(effgtg(2,4)+effgtg(2,1))/2.d0
        effgtg(2,9)=(effgtg(2,5)+effgtg(2,6) +effgtg(2,7)+effgtg(2,8))&
        /4.d0
!
        effgtg(3,1)=demiep*(wnc1*sigma(4,1)+wnc2*sigma(4,5) +wnc3*&
        sigma(4,9))
        effgtg(3,2)=demiep*(wnc1*sigma(4,2)+wnc2*sigma(4,6) +wnc3*&
        sigma(4,10))
        effgtg(3,3)=demiep*(wnc1*sigma(4,3)+wnc2*sigma(4,7) +wnc3*&
        sigma(4,11))
        effgtg(3,4)=demiep*(wnc1*sigma(4,4)+wnc2*sigma(4,8) +wnc3*&
        sigma(4,12))
        effgtg(3,5)=(effgtg(3,1)+effgtg(3,2))/2.d0
        effgtg(3,6)=(effgtg(3,2)+effgtg(3,3))/2.d0
        effgtg(3,7)=(effgtg(3,3)+effgtg(3,4))/2.d0
        effgtg(3,8)=(effgtg(3,4)+effgtg(3,1))/2.d0
        effgtg(3,9)=(effgtg(3,5)+effgtg(3,6) +effgtg(3,7)+effgtg(3,8))&
        /4.d0
!
        effgtg(4,1)=demiep*(wnc1*zic1*sigma(1,1) +wnc2*zic2*sigma(1,5)&
        +wnc3*zic3*sigma(1,9))
        effgtg(4,2)=demiep*(wnc1*zic1*sigma(1,2) +wnc2*zic2*sigma(1,6)&
        +wnc3*zic3*sigma(1,10))
        effgtg(4,3)=demiep*(wnc1*zic1*sigma(1,3) +wnc2*zic2*sigma(1,7)&
        +wnc3*zic3*sigma(1,11))
        effgtg(4,4)=demiep*(wnc1*zic1*sigma(1,4) +wnc2*zic2*sigma(1,8)&
        +wnc3*zic3*sigma(1,12))
        effgtg(4,5)=(effgtg(4,1)+effgtg(4,2))/2.d0
        effgtg(4,6)=(effgtg(4,2)+effgtg(4,3))/2.d0
        effgtg(4,7)=(effgtg(4,3)+effgtg(4,4))/2.d0
        effgtg(4,8)=(effgtg(4,4)+effgtg(4,1))/2.d0
        effgtg(4,9)=(effgtg(4,5)+effgtg(4,6) +effgtg(4,7)+effgtg(4,8))&
        /4.d0
!
        effgtg(5,1)=demiep*(wnc1*zic1*sigma(2,1) +wnc2*zic2*sigma(2,5)&
        +wnc3*zic3*sigma(2,9))
        effgtg(5,2)=demiep*(wnc1*zic1*sigma(2,2) +wnc2*zic2*sigma(2,6)&
        +wnc3*zic3*sigma(2,10))
        effgtg(5,3)=demiep*(wnc1*zic1*sigma(2,3) +wnc2*zic2*sigma(2,7)&
        +wnc3*zic3*sigma(2,11))
        effgtg(5,4)=demiep*(wnc1*zic1*sigma(2,4) +wnc2*zic2*sigma(2,8)&
        +wnc3*zic3*sigma(2,12))
        effgtg(5,5)=(effgtg(5,1)+effgtg(5,2))/2.d0
        effgtg(5,6)=(effgtg(5,2)+effgtg(5,3))/2.d0
        effgtg(5,7)=(effgtg(5,3)+effgtg(5,4))/2.d0
        effgtg(5,8)=(effgtg(5,4)+effgtg(5,1))/2.d0
        effgtg(5,9)=(effgtg(5,5)+effgtg(5,6) +effgtg(5,7)+effgtg(5,8))&
        /4.d0
!
        effgtg(6,1)=demiep*(wnc1*zic1*sigma(4,1) +wnc2*zic2*sigma(4,5)&
        +wnc3*zic3*sigma(4,9))
        effgtg(6,2)=demiep*(wnc1*zic1*sigma(4,2) +wnc2*zic2*sigma(4,6)&
        +wnc3*zic3*sigma(4,10))
        effgtg(6,3)=demiep*(wnc1*zic1*sigma(4,3) +wnc2*zic2*sigma(4,7)&
        +wnc3*zic3*sigma(4,11))
        effgtg(6,4)=demiep*(wnc1*zic1*sigma(4,4) +wnc2*zic2*sigma(4,8)&
        +wnc3*zic3*sigma(4,12))
        effgtg(6,5)=(effgtg(6,1)+effgtg(6,2))/2.d0
        effgtg(6,6)=(effgtg(6,2)+effgtg(6,3))/2.d0
        effgtg(6,7)=(effgtg(6,3)+effgtg(6,4))/2.d0
        effgtg(6,8)=(effgtg(6,4)+effgtg(6,1))/2.d0
        effgtg(6,9)=(effgtg(6,5)+effgtg(6,6) +effgtg(6,7)+effgtg(6,8))&
        /4.d0
!
        effgtg(7,1)=demiep*(wnc1*sigma(5,1)+wnc2*sigma(5,5) +wnc3*&
        sigma(5,9))
        effgtg(7,2)=demiep*(wnc1*sigma(5,2)+wnc2*sigma(5,6) +wnc3*&
        sigma(5,10))
        effgtg(7,3)=demiep*(wnc1*sigma(5,3)+wnc2*sigma(5,7) +wnc3*&
        sigma(5,11))
        effgtg(7,4)=demiep*(wnc1*sigma(5,4)+wnc2*sigma(5,8) +wnc3*&
        sigma(5,12))
        effgtg(7,5)=(effgtg(7,1)+effgtg(7,2))/2.d0
        effgtg(7,6)=(effgtg(7,2)+effgtg(7,3))/2.d0
        effgtg(7,7)=(effgtg(7,3)+effgtg(7,4))/2.d0
        effgtg(7,8)=(effgtg(7,4)+effgtg(7,1))/2.d0
        effgtg(7,9)=(effgtg(7,5)+effgtg(7,6) +effgtg(7,7)+effgtg(7,8))&
        /4.d0
!
        effgtg(8,1)=demiep*(wnc1*sigma(6,1)+wnc2*sigma(6,5) +wnc3*&
        sigma(6,9))
        effgtg(8,2)=demiep*(wnc1*sigma(6,2)+wnc2*sigma(6,6) +wnc3*&
        sigma(6,10))
        effgtg(8,3)=demiep*(wnc1*sigma(6,3)+wnc2*sigma(6,7) +wnc3*&
        sigma(6,11))
        effgtg(8,4)=demiep*(wnc1*sigma(6,4)+wnc2*sigma(6,8) +wnc3*&
        sigma(6,12))
        effgtg(8,5)=(effgtg(8,1)+effgtg(8,2))/2.d0
        effgtg(8,6)=(effgtg(8,2)+effgtg(8,3))/2.d0
        effgtg(8,7)=(effgtg(8,3)+effgtg(8,4))/2.d0
        effgtg(8,8)=(effgtg(8,4)+effgtg(8,1))/2.d0
        effgtg(8,9)=(effgtg(8,5)+effgtg(8,6) +effgtg(8,7)+effgtg(8,8))&
        /4.d0
!
    else if (nomte.eq.'MEC3TR7H') then
!
        effgtg(1,1)=demiep*(wnc1*sigma(1,1)+wnc2*sigma(1,4) +wnc3*&
        sigma(1,7))
        effgtg(1,2)=demiep*(wnc1*sigma(1,2)+wnc2*sigma(1,5) +wnc3*&
        sigma(1,8))
        effgtg(1,3)=demiep*(wnc1*sigma(1,3)+wnc2*sigma(1,6) +wnc3*&
        sigma(1,9))
        effgtg(1,4)=(effgtg(1,1)+effgtg(1,2))/2.d0
        effgtg(1,5)=(effgtg(1,2)+effgtg(1,3))/2.d0
        effgtg(1,6)=(effgtg(1,3)+effgtg(1,1))/2.d0
        effgtg(1,7)=(effgtg(1,1)+effgtg(1,2)+effgtg(1,3))/3.d0
!
        effgtg(2,1)=demiep*(wnc1*sigma(2,1)+wnc2*sigma(2,4) +wnc3*&
        sigma(2,7))
        effgtg(2,2)=demiep*(wnc1*sigma(2,2)+wnc2*sigma(2,5) +wnc3*&
        sigma(2,8))
        effgtg(2,3)=demiep*(wnc1*sigma(2,3)+wnc2*sigma(2,6) +wnc3*&
        sigma(2,9))
        effgtg(2,4)=(effgtg(2,1)+effgtg(2,2))/2.d0
        effgtg(2,5)=(effgtg(2,2)+effgtg(2,3))/2.d0
        effgtg(2,6)=(effgtg(2,3)+effgtg(2,1))/2.d0
        effgtg(2,7)=(effgtg(2,1)+effgtg(2,2)+effgtg(2,3))/3.d0
!
        effgtg(3,1)=demiep*(wnc1*sigma(4,1)+wnc2*sigma(4,4) +wnc3*&
        sigma(4,7))
        effgtg(3,2)=demiep*(wnc1*sigma(4,2)+wnc2*sigma(4,5) +wnc3*&
        sigma(4,8))
        effgtg(3,3)=demiep*(wnc1*sigma(4,3)+wnc2*sigma(4,6) +wnc3*&
        sigma(4,9))
        effgtg(3,4)=(effgtg(3,1)+effgtg(3,2))/2.d0
        effgtg(3,5)=(effgtg(3,2)+effgtg(3,3))/2.d0
        effgtg(3,6)=(effgtg(3,3)+effgtg(3,1))/2.d0
        effgtg(3,7)=(effgtg(3,1)+effgtg(3,2)+effgtg(3,3))/3.d0
!
        effgtg(4,1)=demiep*(wnc1*zic1*sigma(1,1) +wnc2*zic2*sigma(1,4)&
        +wnc3*zic3*sigma(1,7))
        effgtg(4,2)=demiep*(wnc1*zic1*sigma(1,2) +wnc2*zic2*sigma(1,5)&
        +wnc3*zic3*sigma(1,8))
        effgtg(4,3)=demiep*(wnc1*zic1*sigma(1,3) +wnc2*zic2*sigma(1,6)&
        +wnc3*zic3*sigma(1,9))
        effgtg(4,4)=(effgtg(4,1)+effgtg(4,2))/2.d0
        effgtg(4,5)=(effgtg(4,2)+effgtg(4,3))/2.d0
        effgtg(4,6)=(effgtg(4,3)+effgtg(4,1))/2.d0
        effgtg(4,7)=(effgtg(4,1)+effgtg(4,2)+effgtg(4,3))/3.d0
!
        effgtg(5,1)=demiep*(wnc1*zic1*sigma(2,1) +wnc2*zic2*sigma(2,4)&
        +wnc3*zic3*sigma(2,7))
        effgtg(5,2)=demiep*(wnc1*zic1*sigma(2,2) +wnc2*zic2*sigma(2,5)&
        +wnc3*zic3*sigma(2,8))
        effgtg(5,3)=demiep*(wnc1*zic1*sigma(2,3) +wnc2*zic2*sigma(2,6)&
        +wnc3*zic3*sigma(2,9))
        effgtg(5,4)=(effgtg(5,1)+effgtg(5,2))/2.d0
        effgtg(5,5)=(effgtg(5,2)+effgtg(5,3))/2.d0
        effgtg(5,6)=(effgtg(5,3)+effgtg(5,1))/2.d0
        effgtg(5,7)=(effgtg(5,1)+effgtg(5,2)+effgtg(5,3))/3.d0
!
        effgtg(6,1)=demiep*(wnc1*zic1*sigma(4,1) +wnc2*zic2*sigma(4,4)&
        +wnc3*zic3*sigma(4,7))
        effgtg(6,2)=demiep*(wnc1*zic1*sigma(4,2) +wnc2*zic2*sigma(4,5)&
        +wnc3*zic3*sigma(4,8))
        effgtg(6,3)=demiep*(wnc1*zic1*sigma(4,3) +wnc2*zic2*sigma(4,6)&
        +wnc3*zic3*sigma(4,9))
        effgtg(6,4)=(effgtg(6,1)+effgtg(6,2))/2.d0
        effgtg(6,5)=(effgtg(6,2)+effgtg(6,3))/2.d0
        effgtg(6,6)=(effgtg(6,3)+effgtg(6,1))/2.d0
        effgtg(6,7)=(effgtg(6,1)+effgtg(6,2)+effgtg(6,3))/3.d0
!
        effgtg(7,1)=demiep*(wnc1*sigma(5,1)+wnc2*sigma(5,4) +wnc3*&
        sigma(5,7))
        effgtg(7,2)=demiep*(wnc1*sigma(5,2)+wnc2*sigma(5,5) +wnc3*&
        sigma(5,8))
        effgtg(7,3)=demiep*(wnc1*sigma(5,3)+wnc2*sigma(5,6) +wnc3*&
        sigma(5,9))
        effgtg(7,4)=(effgtg(7,1)+effgtg(7,2))/2.d0
        effgtg(7,5)=(effgtg(7,2)+effgtg(7,3))/2.d0
        effgtg(7,6)=(effgtg(7,3)+effgtg(7,1))/2.d0
        effgtg(7,7)=(effgtg(7,1)+effgtg(7,2)+effgtg(7,3))/3.d0
!
        effgtg(8,1)=demiep*(wnc1*sigma(6,1)+wnc2*sigma(6,4) +wnc3*&
        sigma(6,7))
        effgtg(8,2)=demiep*(wnc1*sigma(6,2)+wnc2*sigma(6,5) +wnc3*&
        sigma(6,8))
        effgtg(8,3)=demiep*(wnc1*sigma(6,3)+wnc2*sigma(6,6) +wnc3*&
        sigma(6,9))
        effgtg(8,4)=(effgtg(8,1)+effgtg(8,2))/2.d0
        effgtg(8,5)=(effgtg(8,2)+effgtg(8,3))/2.d0
        effgtg(8,6)=(effgtg(8,3)+effgtg(8,1))/2.d0
        effgtg(8,7)=(effgtg(8,1)+effgtg(8,2)+effgtg(8,3))/3.d0
!
    endif
!
end subroutine
