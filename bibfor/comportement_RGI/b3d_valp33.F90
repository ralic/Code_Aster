subroutine b3d_valp33(x33, x3, v33)
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
! person_in_charge: etienne grimal at edf.fr
!=====================================================================
!=====================================================================
!     A.Sellier jeu. 02 sept. 2010 18:13:24 CEST
!     diagonalisagion 33 a partir de jacobi + quelques controles
!     declarations externes
!     une matrice est consideree comme deja diagonale si test 20 verifie
!     fonction de la prescision relative epsv
!     epsv est aussi utilise   dans jacob3 pour decider si deux valeurs
!     propres petites par rapport a la troisieme peuvent etre consideree
!     comme des doubles, ce qui evite de rechercher des vecteurs propres
!     avec une matrice mal conditonnee
!     enfin epsv est utilise en fin de programme pour tester si la matri
!     de passage fonctionne correctement, pour cela on verifie si la pro
!     vt*v est verifiee  a epsv pres hors diagonale si ce n est pas le c
!     on affiche un message non bloquant
!=====================================================================
    implicit none
#include "asterfort/matini.h"
#include "asterfort/b3d_jacob3.h"
#include "asterfort/b3d_jacob2.h"
#include "asterfort/matmat.h"
#include "asterfort/affiche33.h"
        real(kind=8) :: x33(3, 3)
        real(kind=8) :: x3(3)
        real(kind=8) :: v33(3, 3)
!     declarations locales
    real(kind=8) :: epsv, xmax, depsv
    integer :: i, j
    real(kind=8) :: un
    real(kind=8) :: eps3(3),depsv2,eps1,xn2,xn3

    parameter(un=1.d0,epsv=1.d-4)
    real(kind=8) :: v33t(3, 3)
    real(kind=8) :: u33(3, 3), u033(3, 3), dif33(3, 3)
    logical :: vpmultiple, erreur, diago, ordre
    integer :: imin, imax, imoy
!
!
!     epsv*d(1) valeur en dessous la quelle un terme hors diagonale est
!     lors du calcul des vecteurs propres
    vpmultiple=.false.
    diago=.false.
    ordre=.true.
!     print*
!     call affiche33(x33)
!     on verifie si x33 n est pas deja diagonale ( a epsv*xmax pres)
    xmax=max(abs(x33(1,1)),abs(x33(2,2)),abs(x33(3,3)))
    depsv=epsv*xmax
       if((abs(x33(1,2)).le.depsv).and.&
     &   (abs(x33(1,3)).le.depsv).and.&
     &   (abs(x33(2,3)).le.depsv))then
    diago=.true.
       call matini(3,3,0.D0,v33)
!       print*,'matrice deja diagonale'
!      mise en ordre des valeurs propres
    imin=1
    if (x33(2,2) .lt. x33(imin,imin)) imin=2
    if (x33(3,3) .lt. x33(imin,imin)) imin=3
    imax=1
    if (x33(2,2) .ge. x33(imax,imax)) imax=2
    if (x33(3,3) .ge. x33(imax,imax)) imax=3
    if (imax .eq. imin) then
        imax=1
        imin=3
        end if
        imoy=6-imin-imax
        x3(1)=x33(imax,imax)
        x3(2)=x33(imoy,imoy)
        x3(3)=x33(imin,imin)
        v33(imax,1)=1.d0
        v33(imoy,2)=1.d0
        v33(imin,3)=1.d0
!      verif de l ordre
        if ((x3(1).ge.x3(2)) .and. (x3(2).ge.x3(3))) then
            ordre=.true.
        else
            erreur=.true.
            ordre=.false.
            end if
        else
!      on teste si on est pas en contrainte plane ou axisym
            depsv2=epsv*abs(x33(3,3))
            if ((abs(x33(1,3)).le.depsv2) .and. (abs(x33(2,3)).le.depsv2)) then
!        on utilise le fait que la matrice ne soit pas diagonale dans
!        la base rz ou xy pour se ramener a un pb 2 d
!         print*,'on passe par diag2d'
                call b3d_jacob2(x33, x3, v33, epsv)
            else
!        cas general
!         print*,'cas general'
!         call affiche33(x33)
!         read*
                call b3d_jacob3(x33, 3, x3, v33, vpmultiple,&
                                epsv)
                end if
                end if
!
!**************************************************
!     controle d orthogonalite et des normes de v33
!     le but est de s assurer que v33 est bien une
!     matrice de passage
!      print*,'valeurs propres', x3(1),x3(2),x3(3)
!      do i=1,3
!       do j=1,3
!        v33t(i,j)=v33(j,i)
!       end do
!      end do
!      call matmat(v33,v33t,3,3,3,u33)
!      print*, 'image matrice identite av correction'
!      call affiche33(u33)
!     correction des erreurs numeriques eventuelle
!
!     **verif produit scalaire entre v1 et v2*****
                eps1=0.d0
                do i = 1, 3
                    eps1=eps1+v33(i,1)*v33(i,2)
                end do
!     print*,'erreur produit scalaire v1 v2 av corr',eps1
!     correction v2 pour assurer le produit scalaire
                xn2=0.d0
                do i = 1, 3
                    v33(i,2)=v33(i,2)-eps1*v33(i,1)
                    xn2=xn2+v33(i,2)**2
                end do
                xn2=dsqrt(xn2)
!     renormalisation de v2
!      print*,'xn2 ds valp33',xn2
                if (xn2 .lt. 0.95) then
                    erreur=.true.
                    goto 10
                    end if
                    do i = 1, 3
                        v33(i,2)=v33(i,2)/xn2
                    end do
!     test nouveau produit scalaire
!      eps1=0.d0
!      do i=1,3
!       eps1=eps1+v33(i,1)*v33(i,2)
!      end do
!      print*,'erreur produit scalaire v1 v2 ap corr',eps1
!
!     **verif produit vectoriel v1 V v2 -v3=0*****
                    eps3(1)=v33(2,1)*v33(3,2)-v33(3,1)*v33(2,2)-v33(1,3)
                    eps3(2)=v33(3,1)*v33(1,2)-v33(1,1)*v33(3,2)-v33(2,3)
                    eps3(3)=v33(1,1)*v33(2,2)-v33(2,1)*v33(1,2)-v33(3,3)
!      print*,'erreur produit vectoriel av corr'
!      do i=1,3
!       print*,'eps(',i,')=',eps3(i)
!      end do
!
!     correction produit vectoriel
                    xn3=0.d0
                    do i = 1, 3
                        v33(i,3)=v33(i,3)+eps3(i)
                        xn3=xn3+v33(i,3)**2
                    end do
!     renormalisation de v3
                    xn3=dsqrt(xn3)
                    do i = 1, 3
                        v33(i,3)=v33(i,3)/xn3
                    end do
!
!     **verif produit vectoriel v1 V v2 -v3=0*****
!      eps3(1)=v33(2,1)*v33(3,2)-v33(3,1)*v33(2,2)-v33(1,3)
!      eps3(2)=v33(3,1)*v33(1,2)-v33(1,1)*v33(3,2)-v33(2,3)
!      eps3(3)=v33(1,1)*v33(2,2)-v33(2,1)*v33(1,2)-v33(3,3)
!      print*,'erreur produit vectoriel ap corr'
!      do i=1,3
!       print*,'eps(',i,')=',eps3(i)
!      end do
!
!     ** image diagonalisee de la matrice initiale**
!      call b3d_chrep(u33,x33,v33)
!      print*, 'matrice dans la base principale'
!      call affiche33(u33)
!      print*, 'retour matrice dans base fixe'
                    do i = 1, 3
                        do j = 1, 3
                            v33t(i,j)=v33(j,i)
                        end do
                    end do
!      call b3d_chrep(u033,u33,v33t)
!      call affiche33(u033)
!      print*,'comparaision matrice init'
!      do i=1,3
!        do j=1,3
!         dif33(i,j)=x33(i,j)-u033(i,j)
!        end do
!      end do
!      call affiche33(dif33)
!***********************************************
!
!     verif validite des matrice de passage
!     (on change de nase une matrice unitaire)
                    erreur=.false.
       call matmat(v33,v33t,3,3,3,u33)
!      print*,'Image matrice identite apres correction'
!       call affiche33(u33)
                    do i = 1, 3
                        do j = 1, 3
                            if (i .eq. j) then
                                if (abs(u33(i,i)-un) .gt. epsv) then
                                    erreur=.true.
                                    goto 10
                                endif
                            else
                                if (abs(u33(i,j)) .gt. epsv) then
                                    erreur=.true.
                                    goto 10
                                endif
                                end if
                                end do
                                end do
!     affichage des variables en cas de pb de diagonalisation
!10    continue
                                10 if(erreur)then
                                print*,'_______________________________'
                                print*,'pb vecteur propre ds b3d_valp33'
                                print*,'matrice deja digonale :',diago
                                print*,'ordre :',ordre
                                print*,'vp multiple :',vpmultiple
                                print*,'matrice a diagonaliser :'
                                call affiche33(x33)
                                print*,'valeurs propres:',x3(1),x3(2),x3(3)
                                print*,'x1-x2',x3(1)-x3(2)
                                print*,'x2-x3',x3(2)-x3(3)
                                print*,'|',epsv,'*x(1)|',abs(epsv * x3(1))
                                print*,'|',epsv,'*x(2)|',abs(epsv * x3(2))
                                print*,'matrice de passage:'
                                call affiche33(v33)
                                print*,'image matrice identite:'
                                call affiche33(u33)
                                print*,'Augmenter epsv dans b3d_valp33.eso'
!      pour poursuivre le calcul on suppose la base principale
!      confondu avec la base fixe
                                do i = 1, 3
                                    do j = i, 3
                                        if (i .eq. j) then
                                            v33(i,i)=1.d0
                                        else
                                            v33(i,j)=0.d0
                                            v33(j,i)=0.d0
                                            end if
                                            end do
                                            x3(i)=x33(i,i)
                                            end do
                                            print*,'on impose la base principale=la base fixe'
                               call affiche33(x33)
                                            print*,'valeurs propres:',x3(1),x3(2),x3(3)
                                            print*,'matrice de passage:'
                                            call affiche33(v33)
!      read*
                                            end if
end subroutine
