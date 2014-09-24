subroutine b3d_tail(xmat, nmat, ifou, mfr1, nmat0,&
                    nmat1, t33, n33, local)
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
!     chargement des tailles de l element en cours pour la procedure de
!     en fonction de la formulation
!     declarations externes
!=====================================================================
    implicit none
#include "asterf_types.h"
    real(kind=8) :: ray
    integer :: nmat, ifou, mfr1, nmat1, nmat0
    aster_logical :: local
    real(kind=8) :: xmat(nmat)
    real(kind=8) :: n33(3, 3)
    real(kind=8) :: t33(3, 3)
!      print*,'ds b3d_tail:'
!      print*, 'nmat,ifou,mfr1,nmat0,nmat1'
!      print*, nmat,ifou,mfr1,nmat0,nmat1
!     nombre de parametre modele elastique: nmat0
!     nombre de parametres obligatoires endommagement:nmat1
    if ((mfr1.eq.1) .or. (mfr1.eq.33)) then
        if (ifou .eq. 2) then
!       formulation massive 3d
            t33(1,1)=xmat(nmat1+1)
            t33(2,2)=xmat(nmat1+2)
            t33(3,3)=xmat(nmat1+3)
            t33(1,2)=xmat(nmat1+4)
            t33(1,3)=xmat(nmat1+5)
            t33(2,3)=xmat(nmat1+6)
            t33(2,1)=t33(1,2)
            t33(3,1)=t33(1,3)
            t33(3,2)=t33(2,3)
            n33(1,1)=xmat(nmat1+7)
            n33(2,2)=xmat(nmat1+8)
            n33(3,3)=xmat(nmat1+9)
            n33(1,2)=xmat(nmat1+10)
            n33(1,3)=xmat(nmat1+11)
            n33(2,3)=xmat(nmat1+12)
            n33(2,1)=n33(1,2)
            n33(3,1)=n33(1,3)
            n33(3,2)=n33(2,3)
        end if
        if ((ifou.eq.0) .or. (ifou.eq.-1)) then
!       mode axisymetrique ou deformation plane
!       chargement des matrices des tailles de EF
            t33(1,1)=xmat(nmat1+1)
            t33(2,2)=xmat(nmat1+2)
            if (ifou .eq. 0) then
!        cas axisym on recupere le rayon en 8 pour calculer la dim 3
                ray=xmat(nmat1+8)
                t33(3,3)=3.14d0*ray
!         print*,t33(3,3)
            else
!        cas def plane on recupere l epaisseur vraie
                t33(3,3)=xmat(nmat1+8)
            end if
            t33(1,2)=xmat(nmat1+4)
            t33(1,3)=0.d0
            t33(2,3)=0.d0
            t33(2,1)=t33(1,2)
            t33(3,1)=t33(1,3)
            t33(3,2)=t33(2,3)
            n33(1,1)=xmat(nmat1+5)
            n33(2,2)=xmat(nmat1+6)
            n33(3,3)=1.d0
            n33(1,2)=xmat(nmat1+7)
            n33(1,3)=0.d0
            n33(2,3)=0.d0
            n33(2,1)=n33(1,2)
            n33(3,1)=n33(1,3)
            n33(3,2)=n33(2,3)
        end if
    else
        print*,'ENDO3D non implante pour cette formulation'
        read*
    end if
!     variable logique localisation
!     la transformation de Hillerborgh n est activee que si t33 non nul
!     c a d fourni en prametres materiaux
!      if(t33(1,1).eq.0.)then
!       if(local)then
!        print*, 'Traitement Hillerborgh necessite les tailles'
!        print*, 'Programme stop dans b3d_tail'
!        stop
!       end if
!      end if
!      call affiche33(t33)
!      read*
end subroutine
