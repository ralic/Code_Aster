subroutine xprali(p1, p2, vnele, nelcou, poifis,&
                  trifis, libre, vin)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
    real(kind=8) :: vnele(3), vin(3)
    character(len=19) :: poifis, trifis
    logical(kind=1) :: libre
    integer :: p1, p2, nelcou
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
! person_in_charge: daniele.colombo at ifpen.fr
!
!     ------------------------------------------------------------------
!
!       XPRALI   : X-FEM PROPAGATION : ARETE LIBRE
!       ------     -     --            -     --
!
!    DANS LE CADRE DE LA PROPAGATION X-FEM, LA SURFACE DE LA FISSURE
!    EST DECRITE PAR TRIANGLES. CET SUBROUTINE DETERMINE SI UNE ARETE
!    DE UN DE CES TRIANGLES EST SUR LA SURFACE LIBRE DU DOMAIN.
!
!    ENTREE
!        P1     = COORDONNEES DU PREMIER POINT DEFINISSANT L'ARETE
!        P2     = COORDONNEES DU DEUXIEME POINT DEFINISSANT L'ARETE
!        VNELE  = VECTEUR NORMAL AU PLAN DU TRIANGLE DUQUEL L'ARETE FAIT
!                 PARTIE
!        NELCOU = NUMERO DE L'ELEMENT COUPE PAR LSN=0 DUQUEL LE TRIANGLE
!                 FAIT PARTIE
!        POIFIS = NOM DE L'OBJET JEVEUX OU LA LISTE DES COORDONNEES DES
!                 POINTS D'INTERSECTION ENTRE LES ELEMENTS ET LSN=0
!                 EST STOCKEE (VOIR XPRLS0.F)
!        TRIFIS = NOM DE L'OBJET JEVEUX OU LE NUMERO ET LA LISTE DES
!                 POINTS D'INTERSECTION ENTRE LES ELEMENTS ET LSN=0 SONT
!                 STOCKES (VOIR XPRLS0.F)
!    SORTIE
!        LIBRE  = .TRUE. SI L'ARETE EST SUR LA SURFACE LIBRE DU DOMAIN
!                 .FALSE. SINON
!        VIN    = DANS LE CAS OU LIBRE=.TRUE., VECTEUR UNITAIRE NORMAL A
!                 LA SURFACE LIBRE DU DOMAIN. SA DIRECTION EST TELLE QUE
!                 LE VECTEUR ENTRE DANS LE DOMAIN.
!
!     ------------------------------------------------------------------
!
!
    integer :: jpoi, jtri, elcut, elshar, i, j, nptint, occur, np
    real(kind=8) :: a(3), b(3), v1(3), vnp(3), pdir
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
!
    libre = .false.
!
!     RETRIEVE THE VECTORS CONTAINING THE TRIANGULATION OF LSN=0
    call jeveuo(poifis, 'L', jpoi)
    call jeveuo(trifis, 'L', jtri)
!
!     RETRIEVE THE NUMBER OF ELEMENTS CUT BY THE LSN=0
    call jelira(trifis, 'LONMAX', elcut)
    elcut = elcut/7
!
!     ******************************************************************
!     CALCULATE HOW MANY ELEMENTS SHARE THE EDGE P1-P2
!     ******************************************************************
!
!     INITIALISE THE VARIABLE WHERE THE NUMBER OF ELEMENTS SHARING THE
!     EDGE P1-P2 IS STORED
    elshar = 0
!
!     LOOP ON THE ELEMENTS CUT BY THE LSN=0
    do 2000 i = 1, elcut
!
!        RETREIVE THE NUMBER OF INTERSECTION POINTS BETWEEN THE LSN=0
!        AND THE ELEMENT
        nptint = zi(jtri-1+7*(i-1)+1)
!
!        NUMBER OF OCCURRENCES OF P1 AND P2 FOUND IN THE INTERSECTION
!        POINT LIST OF THE ELEMENT
        occur=0
!
        do 2500 j = 1, nptint
!
!           RETREIVE THE NPTINT-TH INTERSECTION POINT OF THE ELEMENT
            np = zi(jtri-1+7*(i-1)+j+1)
!
!           ONE OCCURRENCE FOUND
            if ((np.eq.p1) .or. (np.eq.p2)) occur=occur+1
!
2500      continue
!
!        ELEMENT "I" SHARES THE EDGE
        if (occur .eq. 2) then
            elshar=elshar+1
!           THE EDGE ISN' FREE IF AT LEAST TWO ELEMENTS SHARE IT
            if (elshar .gt. 1) goto 1000
        endif
!
2000  end do
!
1000  continue
!
!     ******************************************************************
!     IF THE EDGE BELONGS ONLY TO ONE ELEMENT, WE SHOULD CHECK IF IT
!     IS ON THE FREE SURFACE OF THE DOMAIN OR INSIDE THE ELEMENT
!     ******************************************************************
!
!     NO OTHER TRIANGLES OF OTHER ELEMENTS SHEARING THE EDGE. IT
!     COULD BE INSIDE THE SELECTED ELEMENT. LET'S CHECK IT.
    if (elshar .eq. 1) then
!
!        RETREIVE THE COORDINATES OF P1
        a(1) = zr(jpoi-1+4*(p1-1)+1)
        a(2) = zr(jpoi-1+4*(p1-1)+2)
        a(3) = zr(jpoi-1+4*(p1-1)+3)
!
!        RETREIVE THE COORDINATES OF P2
        b(1) = zr(jpoi-1+4*(p2-1)+1)
        b(2) = zr(jpoi-1+4*(p2-1)+2)
        b(3) = zr(jpoi-1+4*(p2-1)+3)
!
!        CALCULATE THE VECTOR CONNECTIN P1 AND P2
        v1(1) = b(1)-a(1)
        v1(2) = b(2)-a(2)
        v1(3) = b(3)-a(3)
!
!        CALCULATE THE NORMAL TO THE EDGE
        call provec(v1, vnele, vnp)
!
!        RETREIVE THE NUMBER OF INTERSECTION POINTS FOR THE ELEMENT
        nptint = zi(jtri-1+7*(nelcou-1)+1)
!
!        NUMBER OF POINTS LYING INSIDE THE DOMAIN
        occur = 2
!
!        CHECK IF THE INTERSECTION POINTS OF THE ELEMENT ARE ALL
!        INCLUDED IN THE SAME SEMISPACE DEFINED BY THE PLANE ABOVE
        do 3000 i = 1, nptint
!           RETREIVE THE POINT
            np=zi(jtri-1+7*(nelcou-1)+i+1)
!
!           IF IT IS NOT ONE OF THE ENDS OF THE EDGE, CALCULATE
!           THE SCALAR PRODUCT BETWEEN NP-P1 VECTOR AND THE NORMAL TO
!           THE PLANE
            if (.not.((np.eq.p1).or.(np.eq.p2))) then
                v1(1) = zr(jpoi-1+4*(np-1)+1)-a(1)
                v1(2) = zr(jpoi-1+4*(np-1)+2)-a(2)
                v1(3) = zr(jpoi-1+4*(np-1)+3)-a(3)
                pdir = v1(1)*vnp(1)+v1(2)*vnp(2)+v1(3)*vnp(3)
!              IF THE SCALAR PRODUCT IS POSITIVE, THE POINT BELONGS TO
!              THE SAME SEMISPACE OF THE NORMAL
                if (pdir .ge. 0.d0) occur=occur+1
            endif
3000      continue
!
!        YES, THE EDGE IS A FREE EDGE. THE NORMAL DISTANCE MUST BE
!        CALCULATED
        if (occur .eq. 2) then
            libre = .true.
            call normev(vnp, pdir)
!           THE VNP VECTOR POINTS OUTWARD TO THE DOMAIN AND MUST BE
!           INVERSED.
            vin(1) = -1.d0*vnp(1)
            vin(2) = -1.d0*vnp(2)
            vin(3) = -1.d0*vnp(3)
        else if (occur.eq.nptint) then
            libre = .true.
            call normev(vnp, pdir)
!           THE VNP VECTOR POINTS INWARD TO THE DOMAIN.
            vin(1) = vnp(1)
            vin(2) = vnp(2)
            vin(3) = vnp(3)
        endif
!
    endif
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
