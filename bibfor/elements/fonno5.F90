subroutine fonno5(noma, indic, nbnoff, noe, na,&
                  nb, ndim, nbnoel, indr, vnor,&
                  vdir)
    implicit none
#include "jeveux.h"
#include "asterfort/gdire3.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/vecini.h"
    character(len=8) :: noma
    integer :: indic(4), nbnoff, noe(4, 4), na, nb, ndim, nbnoel, indr(2)
    real(kind=8) :: vnor(2, 3), vdir(2, 3)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     ------------------------------------------------------------------
!     BUT : CALCUL DES VECTEURS DE LA BASE LOCALE :
!             - VNOR : VECTEUR NORMAL A LA SURFACE DE LA FISSURE
!             - VDIR : VECTEUR DANS LA DIRECTION DE PROPAGATION
!
!           RQ : CHACUN CONTIENT EN FAIT 2 VECTEURS (UN PAR LEVRE)
!     ------------------------------------------------------------------
!
! ENTREES
!     NOMA   : NOM DU MAILLAGE
!     INDIC  : INDICE DES FACES INTERNES
!     NBNOFF : NOMBRE DE NOEUD EN FOND DE FISSURE
!     NOE    : NOEUDS DES FACES CONTENANT NA et NB ET APPARTENANT AUX
!              MAILLES CONNECTEES AU NOEUD SOMMET COURANT
!              ET AUX LEVRES
!     NA     : NUMERO DU NOEUD SOMMET COURANT
!     NB     : NUMERO DU NOEUD SOMMET SUIVANT
!
! SORTIES
!     NBNOEL : NOMBRE DE NOEUDS SOMMETS PAR ELEMENTS
!     INDR   : INDICES DES FACES LIBRES
!     VNOR   : VECTEUR NORMAL A LA SURFACE DE LA FISSURE
!     VDIR   : VECTEUR DANS LA DIRECTION DE PROPAGATION
!
!     ----------------------------------------------------
!
    integer ::  compte
    integer :: indice, inp, ino1, ino2, ico
    integer :: m(8)
    real(kind=8) :: vect1(3), vect2(3), vect3(3), vect4(3), norm1
    real(kind=8) :: coord(3, 4)
    real(kind=8), pointer :: vale(:) => null()
!
!     -----------------------------------------------------------------
!
    call jemarq()
!
!     RECUPERATION DE L'ADRESSE DES COORDONNEES
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
!
    compte = 0
    indice = 0
    do 150 inp = 1, 4
        if ((inp.ne.indic(1)) .and. (inp.ne.indic(2)) .and. ( inp.ne.indic(3)) .and.&
            (inp.ne.indic(4))) then
            compte=compte+1
!         RENUMEROTATION LOCALE POUR AVOIR DANS LES DEUX PREMIERS
!         NOEUDS LES NOEUDS DU FOND DE FISSURE
!         CELA PERMET DEFINIR LE VECTEUR NORMAL A L'AIDE DE GDIRE3
            if (ndim .eq. 3) then
                if (noe(inp,4) .eq. 0) then
                    nbnoel = 3
                    indice = 1
                else
                    nbnoel = 4
                    indice = 2
                endif
                do 151 ino1 = 1, nbnoel
                    m(ino1) = noe(inp,ino1)
                    m(ino1+nbnoel) = noe(inp,ino1)
151              continue
                do 152 ino1 = 1, nbnoel
                    if (m(ino1) .eq. na) then
                        if (m(ino1+1) .eq. nb) then
                            do 153 ino2 = 1, nbnoel
                                noe(inp,ino2) = m(ino1-1+ino2)
153                          continue
                        else
                            do 154 ino2 = 1, nbnoel
                                noe(inp,ino2) = m(ino1+1+nbnoel-ino2)
154                          continue
                        endif
                    endif
152              continue
                do 155 ico = 1, 3
                    do 156 ino1 = 1, nbnoel
                        coord(ico,ino1) = vale((noe(inp,ino1)- 1)*3+ico )
156                  continue
                    vect1(ico) = coord(ico,2) - coord(ico,1)
155              continue
                call normev(vect1, norm1)
!           CALCUL DU VECTEUR DIRECTION DE PROPAGATION
                call gdire3(coord, vect4(1), vect4(2), vect4(3), indice)
!
!           CALCUL DU VECTEUR NORMAL A LA FACE
                call provec(vect4, vect1, vect3)
                call normev(vect3, norm1)
                do 157 ico = 1, 3
                    vnor(compte,ico) = vect3(ico)
                    vdir(compte,ico) = vect4(ico)
157              continue
            else
!           LE NOEUD DU FOND DOIT ETRE LE PREMIER
                nbnoel = 2
                m(1) = noe(inp,1)
                m(2) = noe(inp,2)
                if (m(1) .ne. na) then
                    noe(inp,1) = m(2)
                    noe(inp,2) = m(1)
                endif
!           CALCUL DU VECTEUR DIRECTION DE PROPAGATION
                do 158 ico = 1, 3
                    do 159 ino1 = 1, 2
                        coord(ico,ino1) = vale((noe(inp,ino1)- 1)*3+ico )
159                  continue
                    vect1(ico) = coord(ico,1) - coord(ico,2)
158              continue
                call normev(vect1, norm1)
!           CALCUL DU VECTEUR NORMAL A L'ARETE
                call vecini(3, 0.d0, vect3)
                vect3(3)=1.d0
                call provec(vect3, vect1, vect2)
                do 160 ico = 1, 3
                    vnor(compte,ico) = vect2(ico)
                    vdir(compte,ico) = vect1(ico)
160              continue
            endif
            indr(compte) = inp
        endif
150  end do
    call jedema()
end subroutine
