subroutine b3d_jacob2(x33, x3, v33, epsv)
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
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!=====================================================================
!     digonalisation d'une matrice 33 dont la direction 3 a deja ete reperee comme principale
!=====================================================================
    implicit none
    real(kind=8) :: x33(3, 3)
    real(kind=8) :: x3(3)
    real(kind=8) :: v33(3, 3)
    real(kind=8) :: epsv, a, b, c, epsv1, delta, vnorm, scal
    real(kind=8) :: v22(2, 2), x2(2)
!      call affiche33(x33)
!     valeurs propres 
    a=x33(1,1)
    b=x33(2,2)
    c=x33(1,2)
    epsv1=epsv*max(abs(a),abs(b))
    if (abs(c) .lt. epsv1) then
!      matrice deja digonale
        if (a .ge. b) then
            x2(1)=a
            x2(2)=b
            v22(1,1)=1.d0
            v22(2,1)=0.d0
            v22(1,2)=0.d0
            v22(2,2)=1.d0
        else 
            x2(1)=b
            x2(2)=a
            v22(1,1)=0.d0
            v22(2,1)=1.d0
            v22(1,2)=1.d0
            v22(2,2)=0.d0
        end if
    else
!       il faut digonaliser la sous matrice 22       
        delta=(a-b)**2+4.d0*c**2
        x2(1)=0.5d0*((a+b)+dsqrt(delta))
        x2(2)=0.5d0*((a+b)-dsqrt(delta))
!       1 er vecteur propre
        if (abs(a-x2(1)) .ge. abs(c)) then
            v22(1,1)=-c/(a-x2(1)) 
            v22(2,1)=1.d0
        else
            v22(1,1)=1.d0 
            v22(2,1)=-(a-x2(1))/c
        endif
        vnorm=dsqrt(v22(1,1)**2+v22(2,1)**2)
        v22(1,1)=v22(1,1)/vnorm
        v22(2,1)=v22(2,1)/vnorm
!       2 emme vecteur propre
        if (abs(a-x2(2)) .ge. abs(c)) then
            v22(1,2)=-c/(a-x2(2)) 
            v22(2,2)=1.d0
        else
            v22(1,2)=1.d0 
            v22(2,2)=-(a-x2(2))/c
        endif
        vnorm=dsqrt(v22(1,2)**2+v22(2,2)**2)
        v22(1,2)=v22(1,2)/vnorm
        v22(2,2)=v22(2,2)/vnorm 
        scal=v22(1,1)*v22(1,2)+v22(2,1)*v22(2,2)
        if (abs(scal) .gt. 1.d-5) then
            print*,'pb produit scal ds b3d_jacob2'
            print*,'produit scalaire',scal
            print*,x2
            print*,v22(1,1),v22(1,2)
            print*,v22(2,1),v22(2,2)
            read*
        end if
    end if
!     matrice de passage
    if (x33(3,3) .ge. x2(1)) then
!       la direction ortho au plan de calcul qui est principale
        x3(1)=x33(3,3)
        x3(2)=x2(1)
        x3(3)=x2(2)
        v33(1,1)=0.d0
        v33(2,1)=0.d0
        v33(3,1)=1.d0
        v33(1,2)=v22(1,1)
        v33(2,2)=v22(2,1)
        v33(3,2)=0.d0
        v33(1,3)=v22(1,2)
        v33(2,3)=v22(2,2)
        v33(3,3)=0.D0
    else
!       x2(1) est principale
        x3(1)=x2(1)
        v33(1,1)=v22(1,1)
        v33(2,1)=v22(2,1)
        v33(3,1)=0.d0
        if (x2(2) .ge. x33(3,3)) then
            x3(2)=x2(2)
            x3(3)=x33(3,3)
            v33(1,2)=v22(1,2)
            v33(2,2)=v22(2,2)
            v33(3,2)=0.d0
            v33(1,3)=0.d0
            v33(2,3)=0.d0
            v33(3,3)=1.d0  
        else
            x3(2)=x33(3,3)
            x3(3)=x2(2)
            v33(1,2)=0.d0
            v33(2,2)=0.d0
            v33(3,2)=1.d0
            v33(1,3)=v22(1,2)
            v33(2,3)=v22(2,2)
            v33(3,3)=0.d0
        end if
    end if
end subroutine
