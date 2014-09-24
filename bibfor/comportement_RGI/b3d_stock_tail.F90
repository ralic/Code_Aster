subroutine b3d_stock_tail(xmat, nmatt, ifour, mfr1, nmat0,&
                          nmat1, t33, n33, local, vt33,&
                          var0, varf, nvari, erreur, gf,&
                          fr, rt, epic, beta1, gama1,&
                          nvar1)
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
!     chargement des matrice de taille, digonalisation si necessaire
!=====================================================================
    implicit none
#include "asterfort/b3d_tail.h"
#include "asterfort/b3d_valp33.h"
#include "asterf_types.h"
    integer :: nmatt, ifour, mfr1, nmat0, nmat1, nvari, erreur, nvar1
    aster_logical :: local, copier_vt33
    real(kind=8) :: xmat(nmatt), var0(nvari), varf(nvari)
    real(kind=8) :: t33(3, 3), n33(3, 3), vt33(3, 3)
    real(kind=8) :: gf, fr, rt, epic, beta1, gama1
!     declarations locales
    real(kind=8) :: vpt_connue, t3(3)
!     recuperation des matrices de taille t33 et n33 des elements finis
!      print*,'av tail ds stock tail'
    call b3d_tail(xmat, nmatt, ifour, mfr1, nmat0,&
                  nmat1, t33, n33, local)
!      print*,'apres tail ds stock tail'
!     recuperation des vecteurs propres des tailles des elements
    vpt_connue=var0(nvar1+1)
    if (vpt_connue .eq. 1.) then
!      valeurs propres, deja connu on les charge
        vt33(1,1)=var0(nvar1+2)
        vt33(2,1)=var0(nvar1+3)
        vt33(3,1)=var0(nvar1+4)
        vt33(1,2)=var0(nvar1+5)
        vt33(2,2)=var0(nvar1+6)
        vt33(3,2)=var0(nvar1+7)
        vt33(1,3)=var0(nvar1+8)
        vt33(2,3)=var0(nvar1+9)
        vt33(3,3)=var0(nvar1+10)
    else
!      il faut caluler les vecteurs propres des tailles des elements
!       print*,'Calcul des vecteurs propres des tailles des elements'
        call b3d_valp33(t33, t3, vt33)
!      test de la taille maximale des mailles pour ne pas avoir de snap-
!       xlimax=6.d0*Gf*fr/rt/epic/(-4.d0*beta1*fr+fr-2.d0+2.d0
!     # *beta1+2.d0*gama1*fr+4.d0*gama1*beta1*fr-2.d0*gama1
!     # *beta1+2.d0*gama1)
!       if((t3(1)*1.1d0).gt.xlimax)then
!        print*,'Element de maillage trop grand dans b3d_stock_tail'
!        print*,'li=', (t3(1)*1.1d0)
!                 print*,'lmax traction=',xlimax
!             print*,' Reduire la taille des mailles ou augmenter Gft Gf
!        print*,'Gf=',Gf
!        print*,'fr=',fr
!        print*,'Resistance=',rt
!        print*,'epic=',epic
!        print*,'beta=',beta1
!        print*,'gama=',gama1
!        read*
!        erreur=1
!       end if
!      stockage dans les premieres variables internes pour eviter
!      le recalcul a chaque passage
        varf(nvar1+1)=1.d0
        varf(nvar1+2)=vt33(1,1)
        varf(nvar1+3)=vt33(2,1)
        varf(nvar1+4)=vt33(3,1)
        varf(nvar1+5)=vt33(1,2)
        varf(nvar1+6)=vt33(2,2)
        varf(nvar1+7)=vt33(3,2)
        varf(nvar1+8)=vt33(1,3)
        varf(nvar1+9)=vt33(2,3)
        varf(nvar1+10)=vt33(3,3)
!      on passe l indicateur de calcul de ces varibles a 1 pour le test
!      lors des calculs suivants (attention si changement de maillage en
!      cours de calcul (avec proj des vari alors remettre a zero cette
!      variable pour forcer un recalcul)
        end if
end subroutine
