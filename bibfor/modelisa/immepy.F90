subroutine immepy(nbcnx, xyzma, x3dca, itetra, xbar,&
                  immer)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!  DESCRIPTION : TENTATIVE D'IMMERSION D'UN NOEUD CABLE X3DCA(3) DANS
!  -----------   UNE MAILLE PYRAMIDE APPARTENANT A LA STRUCTURE BETON
!                APPELANT : IMMENO
!
!  IN     : NBCNX  : INTEGER , SCALAIRE
!                    NOMBRE DE NOEUDS DE LA MAILLE PYRAMIDE DANS
!                    LAQUELLE EST TENTEE L'IMMERSION
!  IN     : XYZMA  : REAL*8 , TABLEAU DE DIMENSIONS (3,NNOMAX)
!                    TABLEAU DES COORDONNEES DES NOEUDS DE LA MAILLE
!                    PYRAMIDE DANS LAQUELLE EST TENTEE L'IMMERSION
!  IN     : X3DCA  : REAL*8 , VECTEUR DE DIMENSION 3
!                    COORDONNEES DU NOEUD CABLE
!  OUT    : ITETRA : INTEGER , SCALAIRE
!                    SI IMMERSION REUSSIE : INDICATEUR DU SOUS-DOMAINE
!                    TETRAEDRE AUQUEL APPARTIENT LE NOEUD CABLE
!                    ITETRA = 1 OU 2
!  OUT    : XBAR   : REAL*8 , VECTEUR DE DIMENSION 4
!                    SI IMMERSION REUSSIE : COORDONNEES BARYCENTRIQUES
!                    DU NOEUD CABLE DANS LE SOUS-DOMAINE TETRAEDRE
!                    AUQUEL IL APPARTIENT
!  OUT    : IMMER  : INTEGER , SCALAIRE
!                    INDICE D'IMMERSION
!                    IMMER = -1  IMMERSION NON REUSSIE
!                    IMMER =  0  LE NOEUD CABLE EST A L'INTERIEUR
!                                DE LA MAILLE
!                    IMMER = 100 + 10 * NUMERO DE FACE
!                                LE NOEUD CABLE EST SUR UNE FACE
!                                DE LA MAILLE
!                    IMMER = 100 + 10 * NUMERO DE FACE + NUMERO D'ARETE
!                                LE NOEUD CABLE EST SUR UNE ARETE
!                                DE LA MAILLE
!                    IMMER =  2  LE NOEUD CABLE COINCIDE AVEC UN DES
!                                NOEUDS DE LA MAILLE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterc/r8prem.h"
#include "asterfort/cotfac.h"
#include "asterfort/tstbar.h"
#include "asterfort/u2mess.h"
    integer :: nbcnx, itetra, immer
    real(kind=8) :: xyzma(3, *), x3dca(*), xbar(*)
!
! VARIABLES LOCALES
! -----------------
    real(kind=8) :: d, dx, dy, dz
    integer :: ktest, f1(4), idc, id(6), ii, j
    logical :: facnp1
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    facnp1=.false.
!
!CCC    ORIENTATION FACE QUADRANGULAIRE NOEUDS 1-2-3-4
!
    f1(1)=1
    f1(2)=2
    f1(3)=3
    f1(4)=4
!
    call cotfac(xyzma, f1(1), f1(2), f1(3), 5,&
                xyzma(1, f1(4)), idc)
    if (idc .lt. 0) then
        ii =f1(4)
        f1(4)=f1(1)
        f1(1)=f1(2)
        f1(2)=f1(3)
        f1(3)=ii
        facnp1=.true.
    endif
!
!
    ii=0
!CCC    POSITION COTE INTERNE PREMIERE FACE (2 PLANS)
    call cotfac(xyzma, f1(1), f1(2), f1(3), 5,&
                x3dca(1), id(1))
    if (id(1) .ge. 0) then
        ii=ii+1
        call cotfac(xyzma, f1(3), f1(4), f1(1), 5,&
                    x3dca(1), id(2))
!
!CCC    POSITION COTE INTERNE DEUXIEME FACE (1 PLAN)
        if (id(2) .ge. 0) then
            ii=ii+1
            call cotfac(xyzma, 1, 2, 5, 3,&
                        x3dca(1), id(3))
!
!CCC    POSITION COTE INTERNE TROISIEME FACE (1 PLAN)
            if (id(3) .ge. 0) then
                ii=ii+1
                call cotfac(xyzma, 2, 3, 5, 4,&
                            x3dca(1), id(4))
!
!CCC    POSITION COTE INTERNE QUATRIEME FACE (1 PLAN)
                if (id(4) .ge. 0) then
                    ii=ii+1
                    call cotfac(xyzma, 3, 4, 5, 1,&
                                x3dca(1), id(5))
!
!CCC    POSITION COTE INTERNE CINQUIEME FACE (1 PLAN)
                    if (id(5) .ge. 0) then
                        ii=ii+1
                        call cotfac(xyzma, 4, 1, 5, 2,&
                                    x3dca(1), id(6))
                        if (id(6) .ge. 0) ii=ii+1
                    endif
                endif
            endif
        endif
    endif
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      NOEUD A L EXTERIEUR DU VOLUME DE LA MAILLE
!      ON A TROUVE : ON RESSORT COMPLETEMENT
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    if (ii .lt. 6) then
!
        immer=-1
        goto 9999
!
    else
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ktest=0
        do 10 j = 1, 6
            ktest=ktest+id(j)
10      continue
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      NOEUD IMMERGE DANS LE VOLUME DE LA MAILLE
!      CALCUL DES COORDONNES BARYCENTRIQUES
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
        if (ktest .gt. 3) then
!
            if (nbcnx .eq. 13) then
                do 20 j = 6, 13, 1
                    dx = xyzma(1,j) - x3dca(1)
                    dy = xyzma(2,j) - x3dca(2)
                    dz = xyzma(3,j) - x3dca(3)
                    d = dx*dx + dy*dy + dz*dz
                    if (d .lt. r8prem()) then
                        immer=2
                        goto 9999
                    endif
20              continue
            endif
!
!     TEST D'APPARTENANCE A UN SOUS-DOMAINE TETRAEDRE, PAR DETERMINATION
!     DES COORDONNEES BARYCENTRIQUES (DECOUPAGE DU PYRA EN DEUX TETRAS)
!
!.....TETRAEDRE 1-2-3-5
!
            itetra = 1
            call tstbar(4, xyzma(1, 1), xyzma(1, 2), xyzma(1, 3), xyzma(1, 5),&
                        x3dca(1), xbar(1), immer)
            if (immer .ge. 0) goto 9999
!
!.... TETRAEDRE 1-3-4-5
!
            itetra = 2
            call tstbar(4, xyzma(1, 1), xyzma(1, 3), xyzma(1, 4), xyzma(1, 5),&
                        x3dca(1), xbar(1), immer)
            if (immer .ge. 0) goto 9999
!
!  DANS LE CAS DE FACES REORIENTEE (FACNP1 VRAI) ON TESTE LA PRESENCE
!  DANS LES PETITS TETRAEDRES DEFINIS PAR CHAQUE FACE.
!
!.... TETRAEDRE 1-2-3-4
!
            if (facnp1) then
                itetra = 3
                call tstbar(4, xyzma(1, 1), xyzma(1, 2), xyzma(1, 3), xyzma(1, 4),&
                            x3dca(1), xbar(1), immer)
                if (immer .ge. 0) goto 9999
            endif
!
            if (immer .lt. 0) then
                call u2mess('F', 'MODELISA4_72')
            endif
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      NOEUD COINCIDANT AVEC UN NOEUD SOMMET -APPARTIENT A + DE 3 PLANS
!      ON A TROUVE : ON RESSORT COMPLETEMENT
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
        else
!
            immer=2
            goto 9999
!
        endif
    endif
!
9999  continue
!
! --- FIN DE IMMEPY.
end subroutine
