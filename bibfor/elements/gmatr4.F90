subroutine gmatr4(nnoff, ndimte, milieu, connex, &
                  abscur, matr, pair)

implicit none

#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/elrfvf.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/normev.h"
#include "asterfort/wkvect.h"

    integer           :: nnoff, ndimte
    character(len=24) :: abscur, matr
    aster_logical     :: milieu, connex, pair

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
!      CALCUL DE LA MATRICE DU SYSTEME LINEAIRE [A] {GS} = {GTHI}
!      METHODE LAGRANGE_REGU
!
! ENTREE
!
!   NNOFF    --> NOMBRE DE NOEUDS DU FOND DE FISSURE
!   ABSCUR   --> ABSCISSES CURVILIGNES S
!   NDIMTE   --> NOMBRE DE CHAMPS THETA CHOISIS
!   MILIEU   --> .TRUE.  : ELEMENT QUADRATIQUE
!                .FALSE. : ELEMENT LINEAIRE
!   CONNEX   --> .TRUE.  : SI FOND FERME
!                .FALSE. : SI FOND OUVERT

! SORTIE
!
!   MATR     --> MATRICE DU SYTEME A RESOUDRE
!
! ......................................................................

    integer, parameter       :: nddl = 2, nbnomx = 3, npg = 14
    integer, dimension(nddl) :: conna, conna2, conns
    integer                  :: i, j, ij, imatr
    integer                  :: ipg, js, iseg, jseg, kseg, nseg
    integer                  :: nno, nno2, conn(3)
    real(kind=8)             :: ksis(1), psi(nddl)
    real(kind=8)             :: mele(nddl, nddl)
    real(kind=8)             :: xpg(npg), wpg(npg)
    real(kind=8)             :: jac, jacs,ksi(1)
    real(kind=8)             :: ff(nbnomx), dff(3, nbnomx)
    character(len=8)         :: elrefe
    character(len=8), parameter :: elrefs='SE2'

! ......................................................................

!    COOR ET POID DU POINT DE GAUSS
!
    data xpg /  -.1080549487073437,&
     &           .1080549487073437,&
     &          -.3191123689278897,&
     &           .3191123689278897,&
     &          -.5152486363581541,&
     &           .5152486363581541,&
     &          -.6872929048116855,&
     &           .6872929048116855,&
     &          -.8272013150697650,&
     &           .8272013150697650,&
     &          -.9284348836635735,&
     &           .9284348836635735,&
     &          -.9862838086968123,&
     &           .9862838086968123/
!
!
    data wpg /    .2152638534631578,&
     &            .2152638534631578,&
     &            .2051984637212956,&
     &            .2051984637212956,&
     &            .1855383974779378,&
     &            .1855383974779378,&
     &            .1572031671581935,&
     &            .1572031671581935,&
     &            .1215185706879032,&
     &            .1215185706879032,&
     &            .0801580871597602,&
     &            .0801580871597602,&
     &            .0351194603317519,&
     &            .0351194603317519/

! ......................................................................

    call jemarq()

!   NOMBRE DE SEGMENT DU FOND DE FISSURE
    if (milieu) then
        elrefe = 'SE3'
    else
       elrefe = 'SE2'
    endif       
!
!   ABSCISSES CURVILIGNES S
    call jeveuo(abscur, 'L', js)
!
    call wkvect(matr, 'V V R8', ndimte*ndimte, imatr)
!
    conn   = 0
    conna  = 0
    conna2 = 0
    ksi    = 0.d0
    ff     = 0.d0
    dff    = 0.d0
    psi    = 0.d0
!
!   BOUCLE SUR LES SUPER-ELEMENTS    
    do kseg = 1, ndimte - 1
!
!        CONNECTIVITE DU SUPER-SEGMENT LINAIRE PAR RAPPORT A LA NUMEROTATION DES 
!        SUPER-NOEUDS --> ASSEMBLAGE DE LA MATRICE
         conna(1) = kseg
         conna(2) = kseg + 1
!
!        CONNECTIVITE DU SUPER-SEGMENT LINAIRE PAR RAPPORT A LA NUMEROTATION DES 
!        NOEUDS DU FOND --> CALCUL DE JACOBIEN
         if (kseg .eq. ndimte - 1 .and. pair) then
!           CAS PARTICULIER DU DERNIER SEGMENT, POUR UN NOMBRE IMPAIR DE SEGMENT
            if (milieu) then
               conns(1) = nnoff - 2
               conns(2) = nnoff
            else
               conns(1) = nnoff - 1
               conns(2) = nnoff
            endif
         else
!           CAS GENERAL
            if (milieu) then
               conns(1) = 4*(kseg-1) + 1
               conns(2) = 4*kseg + 1
            else
               conns(1) = 2*(kseg-1) + 1
               conns(2) = 2*kseg + 1
            endif
         endif
!
!        SOUS-DECOUPAGE DU SUPER-ELEMENT
         if (kseg.eq.ndimte - 1.and.pair) then
!           CAS PARTICULIER DU DERNIER SEGMENT, POUR UN NOMBRE IMPAIR DE SEGMENT
            nseg=1
         else
!           CAS GENERAL
            nseg=2
         endif
!
!        CALCUL DU JACOBIEN (SEGM DE REFERENCE --> SEGM REEL)
         if (kseg .eq. ndimte - 1 .and. pair) then
!           CAS PARTICULIER DU DERNIER SEGMENT, POUR UN NOMBRE IMPAIR DE SEGMENT
            jacs = zr(js-1+conns(2)) - zr(js-1+conns(1))
         else
!           CAS GENERAL
            jacs = 0.5d0*(zr(js-1+conns(2)) - zr(js-1+conns(1)))
         endif
!
!       CALCUL DE LA MATRICE ELEMENTAIRE POUR L'ELEMENT COURANT
         mele = 0.d0
         do jseg = 1, nseg      
!
!            CALCUL DE L'INDICE DU SOUS-ELEMENT            
             iseg=2*(kseg - 1) + jseg
!
!            CONNECTIVITE DU SOUS-ELEMENT
             if (milieu) then
                  conn(1) = 2*iseg-1
                  conn(2) = 2*iseg+1
                  conn(3) = 2*iseg
             else
                  conn(1) = iseg
                  conn(2) = iseg+1
             endif

!            BOUCLE SUR LES POINTS DE GAUSS DU SEGMENT
             do ipg=1,npg
            
!                CALCUL DES FONCTIONS DE FORMES
                  ksi(1)=xpg(ipg)
                  call elrfvf(elrefe, ksi, nbnomx, ff, nno)

!                 calcul des coordonnees du point de Gauss courant dans le
!                 super-segment de reference
!                 Rq : - les segments d'indice impair sont envoyes sur une
!                        première moitié de super-segment, i.e. [-1, 0] 
!                      - les segments d'indice impair sont envoyes sur une
!                        deuxième moitié de super-segment, i.e. [0, +1] 
                  if (mod(iseg, 2).eq.1) then
                       ksis(1)=0.5d0*(xpg(ipg)-1.d0)
                  else
                       ksis(1)=0.5d0*(xpg(ipg)+1.d0)
                  endif

!                 correction du jacobien pour tenir compte de la transformation [-1,+1] 
!                 vers [-1, 0] ou [0, 1]
                  jac = 0.5d0*jacs     
     
!                CALCUL DES FONCTIONS DE FORMES SUR LE SUPER-SEGMENT
                  call elrfvf(elrefs, ksis, nbnomx, psi, nno2)

!                 ajout de la contribution du point de Gauss a la matrice
!                 de masse elementaire
                  do i = 1, nddl
                       do j = 1, nddl 
                           mele(i, j) = mele(i, j) + psi(i)*psi(j)*jac*wpg(ipg)
                       end do
                  end do
            end do

          end do  

!       AJOUT DE LA CONTRIBUTION DE DE LA MATRICE DE MASSE ELEMENTAIRE
!       A LA MATRICE DE MASSE ASSEMBLEE
         do i=1, nddl
             do j=1, nddl
                 ij = (conna(i)-1)*ndimte + conna(j)     
                 zr(imatr + ij - 1) = zr(imatr + ij - 1) + mele(i, j)    
             end do
         end do
         
!---------CAS CONNEXE --> FOND FERME------------!

        if (connex) then
!
!           AJOUT D'UNE CONTRIBUTION SUR LA LIGNE NDIMTE
            if (conna(1).eq.1) then
                conna2 = conna
                conna2(1) = ndimte

                i=1 
                do j=1, nddl
                     ij = (conna2(i)-1)*ndimte + conna2(j)
                     zr(imatr + ij - 1) = zr(imatr + ij - 1) + mele(i, j)
                end do
            endif

!           AJOUT D'UNE CONTRIBUTION SUR LA LIGNE 1
            if (conna(2).eq.ndimte) then
                conna2 = conna
                conna2(2) = 1

                i=2 
                do j=1, nddl
                     ij = (conna2(i)-1)*ndimte + conna2(j)
                     zr(imatr + ij - 1) = zr(imatr + ij - 1) + mele(i, j)
                end do
            endif
        endif
    end do

    call jedema()

end subroutine
