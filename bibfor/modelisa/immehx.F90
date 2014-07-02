subroutine immehx(nbcnx, xyzma, x3dca, itetra, xbar,&
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
!  -----------   UNE MAILLE HEXAEDRE APPARTENANT A LA STRUCTURE BETON
!                APPELANT : IMMENO
!
!  IN     : NBCNX  : INTEGER , SCALAIRE
!                    NOMBRE DE NOEUDS DE LA MAILLE HEXAEDRE DANS
!                    LAQUELLE EST TENTEE L'IMMERSION
!  IN     : XYZMA  : REAL*8 , TABLEAU DE DIMENSIONS (3,NNOMAX)
!                    TABLEAU DES COORDONNEES DES NOEUDS DE LA MAILLE
!                    HEXAEDRE DANS LAQUELLE EST TENTEE L'IMMERSION
!  IN     : X3DCA  : REAL*8 , VECTEUR DE DIMENSION 3
!                    COORDONNEES DU NOEUD CABLE
!  OUT    : ITETRA : INTEGER , SCALAIRE
!                    SI IMMERSION REUSSIE : INDICATEUR DU SOUS-DOMAINE
!                    TETRAEDRE AUQUEL APPARTIENT LE NOEUD CABLE
!                    ITETRA = 1 OU 2 OU 3 OU 4 OU 5 OU 6
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
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/cotfac.h"
#include "asterfort/tstbar.h"
#include "asterfort/utmess.h"
    integer :: nbcnx, itetra, immer
    real(kind=8) :: xyzma(3, *), x3dca(*), xbar(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: idc, id(12), ii, j, ktest
    real(kind=8) :: d, dx, dy, dz
    integer :: f1(4), f2(4), f3(4), f4(4), f5(4), f6(4)
    aster_logical :: facnp1, facnp2, facnp3, facnp4, facnp5, facnp6
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!     INDICATEUR SI DES FACES NON PLANES A 4 NOEUDS ONT DU ETRE
!     RENUMEROTEES AFIN QUE LA SURFACE DECRITE DEVIENNE ENVELOPPE
!     CONVEXE DU VOLUME TETRAEDRE
!
    facnp1=.false.
    facnp2=.false.
    facnp3=.false.
    facnp4=.false.
    facnp5=.false.
    facnp6=.false.
!
!CCC    ORIENTATION PREMIERE FACE NOEUDS 1-2-3-4
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
!CCC    ORIENTATION DEUXIEME FACE NOEUDS 3-4-8-7
!
    f2(1)=3
    f2(2)=4
    f2(3)=8
    f2(4)=7
!
    call cotfac(xyzma, f2(1), f2(2), f2(3), 5,&
                xyzma(1, f2(4)), idc)
    if (idc .lt. 0) then
        ii =f2(4)
        f2(4)=f2(1)
        f2(1)=f2(2)
        f2(2)=f2(3)
        f2(3)=ii
        facnp2=.true.
    endif
!
!CCC    ORIENTATION TROISIEME FACE NOEUDS 6-7-8-5
!
    f3(1)=6
    f3(2)=7
    f3(3)=8
    f3(4)=5
!
    call cotfac(xyzma, f3(1), f3(2), f3(3), 1,&
                xyzma(1, f3(4)), idc)
    if (idc .lt. 0) then
        ii =f3(4)
        f3(4)=f3(1)
        f3(1)=f3(2)
        f3(2)=f3(3)
        f3(3)=ii
        facnp3=.true.
    endif
!
!CCC    ORIENTATION QUATRIEME FACE NOEUDS 6-5-1-2
!
    f4(1)=6
    f4(2)=5
    f4(3)=1
    f4(4)=2
!
    call cotfac(xyzma, f4(1), f4(2), f4(3), 4,&
                xyzma(1, f4(4)), idc)
    if (idc .lt. 0) then
        ii =f4(4)
        f4(4)=f4(1)
        f4(1)=f4(2)
        f4(2)=f4(3)
        f4(3)=ii
        facnp4=.true.
    endif
!
!CCC    ORIENTATION CINQUIEME FACE NOEUDS 6-2-3-7
!
    f5(1)=6
    f5(2)=2
    f5(3)=3
    f5(4)=7
!
    call cotfac(xyzma, f5(1), f5(2), f5(3), 5,&
                xyzma(1, f5(4)), idc)
    if (idc .lt. 0) then
        ii =f5(4)
        f5(4)=f5(1)
        f5(1)=f5(2)
        f5(2)=f5(3)
        f5(3)=ii
        facnp5=.true.
    endif
!
!CCC    ORIENTATION SIXIEME FACE NOEUDS 1-5-8-4
!
    f6(1)=1
    f6(2)=5
    f6(3)=8
    f6(4)=4
!
    call cotfac(xyzma, f6(1), f6(2), f6(3), 2,&
                xyzma(1, f6(4)), idc)
    if (idc .lt. 0) then
        ii =f6(4)
        f6(4)=f6(1)
        f6(1)=f6(2)
        f6(2)=f6(3)
        f6(3)=ii
        facnp6=.true.
    endif
!
    ii=0
!CCC    POSITION COTE INTERNE PREMIERE FACE (2 PLANS)
    call cotfac(xyzma, f1(1), f1(2), f1(3), 5,&
                x3dca(1), id(1))
    if (id(1) .ge. 0) then
        ii=ii+1
        call cotfac(xyzma, f1(3), f1(4), f1(1), 5,&
                    x3dca(1), id(2))
!CCC    POSITION COTE INTERNE DEUXIEME FACE (2 PLANS)
        if (id(2) .ge. 0) then
            ii=ii+1
            call cotfac(xyzma, f2(1), f2(2), f2(3), 5,&
                        x3dca(1), id(3))
            if (id(3) .ge. 0) then
                ii=ii+1
                call cotfac(xyzma, f2(3), f2(4), f2(1), 5,&
                            x3dca(1), id(4))
!CCC    POSITION COTE INTERNE TROISIEME FACE (2 PLANS)
                if (id(4) .ge. 0) then
                    ii=ii+1
                    call cotfac(xyzma, f3(1), f3(2), f3(3), 1,&
                                x3dca(1), id(5))
                    if (id(5) .ge. 0) then
                        ii=ii+1
                        call cotfac(xyzma, f3(3), f3(4), f3(1), 1,&
                                    x3dca(1), id(6))
!CCC    POSITION COTE INTERNE QUATRIEME FACE (2 PLANS)
                        if (id(6) .ge. 0) then
                            ii=ii+1
                            call cotfac(xyzma, f4(1), f4(2), f4(3), 4,&
                                        x3dca(1), id(7))
                            if (id(7) .ge. 0) then
                                ii=ii+1
                                call cotfac(xyzma, f4(3), f4(4), f4(1), 4,&
                                            x3dca(1), id(8))
!CCC    POSITION COTE INTERNE CINQUIEME FACE (2 PLANS)
                                if (id(8) .ge. 0) then
                                    ii=ii+1
                                    call cotfac(xyzma, f5(1), f5(2), f5( 3), 5,&
                                                x3dca(1), id(9))
                                    if (id(9) .ge. 0) then
                                        ii=ii+1
                                        call cotfac(xyzma, f5(3), f5(4), f5(1), 5,&
                                                    x3dca(1), id(10))
!CCC    POSITION COTE INTERNE SIXIEME FACE (2 PLANS)
                                        if (id(10) .ge. 0) then
                                            ii=ii+1
                                            call cotfac(xyzma, f6(1), f6(2), f6(3), 2,&
                                                        x3dca(1), id(11))
                                            if (id(11) .ge. 0) then
                                                ii=ii+1
                                                call cotfac(xyzma, f6(3), f6(4), f6(1), 2,&
                                                            x3dca(1), id(12))
                                                if (id(12) .ge. 0) ii=ii+1
                                            endif
                                        endif
                                    endif
                                endif
                            endif
                        endif
                    endif
                endif
            endif
        endif
    endif
!
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      NOEUD A L EXTERIEUR DU VOLUME DE LA MAILLE
!      ON A TROUVE : ON RESSORT COMPLETEMENT
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    if (ii .lt. 12) then
!
        immer=-1
        goto 9999
!
    else
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ktest=0
        do 10 j = 1, 12
            ktest=ktest+id(j)
 10     continue
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      NOEUD IMMERGE DANS LE VOLUME DE LA MAILLE
!      CALCUL DES COORDONNES BARYCENTRIQUES SAUF SI
!      COINCIDENCE AVEC NOEUD MILIEU SI MAILLE HEXA20 OU HEXA27
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
        if (ktest .gt. 9) then
!
            if (nbcnx .eq. 20) then
                do 20 j = 9, 20, 1
                    dx = xyzma(1,j) - x3dca(1)
                    dy = xyzma(2,j) - x3dca(2)
                    dz = xyzma(3,j) - x3dca(3)
                    d = dx*dx + dy*dy + dz*dz
                    if (d .lt. r8prem()) then
                        immer=2
                        goto 9999
                    endif
 20             continue
            endif
            if (nbcnx .eq. 27) then
                do 30 j = 21, 27, 1
                    dx = xyzma(1,j) - x3dca(1)
                    dy = xyzma(2,j) - x3dca(2)
                    dz = xyzma(3,j) - x3dca(3)
                    d = dx*dx + dy*dy + dz*dz
                    if (d .lt. r8prem()) then
                        immer=2
                        goto 9999
                    endif
 30             continue
            endif
!
!     TEST D'APPARTENANCE A UN SOUS-DOMAINE TETRAEDRE PAR DETERMINATION
!     DES COORDONNEES BARYCENTRIQUES (DECOUPAGE D HEXA EN CINQ TETRAS)
!
!.....TETRAEDRE 6-3-8-1
!
            itetra = 1
            call tstbar(4, xyzma(1, 6), xyzma(1, 3), xyzma(1, 8), xyzma(1, 1),&
                        x3dca(1), xbar(1), immer)
            if (immer .ge. 0) goto 9999
!
!.... TETRAEDRE 1-3-8-4
!
            itetra = 2
            call tstbar(4, xyzma(1, 1), xyzma(1, 3), xyzma(1, 8), xyzma(1, 4),&
                        x3dca(1), xbar(1), immer)
            if (immer .ge. 0) goto 9999
!
!.... TETRAEDRE 6-8-1-5
!
            itetra = 3
            call tstbar(4, xyzma(1, 6), xyzma(1, 8), xyzma(1, 1), xyzma(1, 5),&
                        x3dca(1), xbar(1), immer)
            if (immer .ge. 0) goto 9999
!
!.....TETRAEDRE 1-3-6-2
!
            itetra = 4
            call tstbar(4, xyzma(1, 1), xyzma(1, 3), xyzma(1, 6), xyzma(1, 2),&
                        x3dca(1), xbar(1), immer)
            if (immer .ge. 0) goto 9999
!
!.... TETRAEDRE 6-8-3-7
!
            itetra = 5
            call tstbar(4, xyzma(1, 6), xyzma(1, 8), xyzma(1, 3), xyzma(1, 7),&
                        x3dca(1), xbar(1), immer)
            if (immer .ge. 0) goto 9999
!
!  DANS LE CAS DE FACES REORIENTEE (FACNP VRAI) ON TESTE LA PRESENCE
!  DANS LES PETITS TETRAEDRES DEFINIS PAR CHAQUE FACE.
!
!.... TETRAEDRE 1-2-3-4
!
            if (facnp1) then
                itetra = 6
                call tstbar(4, xyzma(1, 1), xyzma(1, 2), xyzma(1, 3), xyzma(1, 4),&
                            x3dca(1), xbar(1), immer)
                if (immer .ge. 0) goto 9999
            endif
!
!.... TETRAEDRE 3-4-8-7
!
            if (facnp2) then
                itetra = 7
                call tstbar(4, xyzma(1, 3), xyzma(1, 4), xyzma(1, 8), xyzma(1, 7),&
                            x3dca(1), xbar(1), immer)
                if (immer .ge. 0) goto 9999
            endif
!
!.... TETRAEDRE 6-7-8-5
!
            if (facnp3) then
                itetra = 8
                call tstbar(4, xyzma(1, 6), xyzma(1, 7), xyzma(1, 8), xyzma(1, 5),&
                            x3dca(1), xbar(1), immer)
                if (immer .ge. 0) goto 9999
            endif
!
!.... TETRAEDRE 6-5-1-2
!
            if (facnp4) then
                itetra = 9
                call tstbar(4, xyzma(1, 6), xyzma(1, 5), xyzma(1, 1), xyzma(1, 2),&
                            x3dca(1), xbar(1), immer)
                if (immer .ge. 0) goto 9999
            endif
!
!.... TETRAEDRE 6-2-3-7
!
            if (facnp5) then
                itetra = 10
                call tstbar(4, xyzma(1, 6), xyzma(1, 2), xyzma(1, 3), xyzma(1, 7),&
                            x3dca(1), xbar(1), immer)
                if (immer .ge. 0) goto 9999
            endif
!
!.... TETRAEDRE 1-5-8-4
!
            if (facnp6) then
                itetra = 11
                call tstbar(4, xyzma(1, 1), xyzma(1, 5), xyzma(1, 8), xyzma(1, 4),&
                            x3dca(1), xbar(1), immer)
                if (immer .ge. 0) goto 9999
            endif
!
            if (immer .lt. 0) then
                call utmess('F', 'MODELISA4_72')
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
!
9999 continue
!
! --- FIN DE IMMEHX.
end subroutine
