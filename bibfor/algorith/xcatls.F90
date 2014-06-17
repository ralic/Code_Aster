subroutine xcatls(ndim, geofis, callst, jltsv, jltsl,&
                  jlnsv, jlnsl, noma, vect1, vect2,&
                  noeud, a, b, r, cote)
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/disell.h"
#include "asterfort/dismoi.h"
#include "asterfort/disrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/utmess.h"
    integer :: ndim, jltsv, jltsl, jlnsv, jlnsl
    character(len=8) :: noma, cote
    character(len=16) :: geofis
    logical :: callst
    real(kind=8) :: vect1(3), vect2(3), noeud(3), a, b, r
!
! ----------------------------------------------------------------------
!      CALCUL INITIAL DES LEVEL-SETS POUR LES FORMES PRE-DEFINIES
!      (CATALGOUE DE FISSURES)
!
! IN :
!  NDIM   : DIMENSION DU MAILLAGE
!  GEOFIS : GEOMETRIE DE LA FISSURE
!  CALLST : LOGICAL POUR SAVOIR SI IL FAUT CALCULER LA LST
!  NOMA   : OBJET MAILLAGE
!  A,B,R,NOEUD,COTE,VECT1,VECT2 :
!
! IN/OUT :
!  JLTSV  : ADRESSE POUR LES VALEURS DE LA LST
!  JLTSL  : ADRESSE POUR LE LOGICAL DE LA LST
!  JLNSV  : ADRESSE POUR LES VALEURS DE LA LSN
!  JLNSL  : ADRESSE POUR LE LOGICAL DE LA LSN
!
!            QUANTITES DEFINISSANT LA GEO DE LA FISS
!     ------------------------------------------------------------------
!
    integer :: ino, nbno,  i, j
    real(kind=8) :: p2d(2), p3d(3), norme, vect3(3), mat(3, 3), ploc(3)
    real(kind=8) :: h
    real(kind=8) :: nori(3), next(3), nmil(3), vseg(3), nseg
    character(len=8) :: fiss
    character(len=16) :: valk(3), typdis, k16bid
    real(kind=8), pointer :: vale(:) => null()
!
    call jemarq()
!
    call getres(fiss, k16bid, k16bid)
    call dismoi('TYPE_DISCONTINUITE', fiss, 'FISS_XFEM', repk=typdis)
!
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
!
!     VERIFICATIONS (CAR REGLES INMPOSSIBLES DANS CAPY)
    if (.not.callst) then
        if (geofis .eq. 'CYLINDRE' .or. geofis .eq. 'DEMI_PLAN' .or. geofis .eq. 'SEGMENT'&
            .or. geofis .eq. 'DEMI_DROITE') then
            valk(1) = 'INTERFACE'
            valk(2) = geofis
            valk(3) = 'FISSURE'
            call utmess('F', 'XFEM_23', nk=3, valk=valk)
        endif
    else if (callst) then
        if (geofis .eq. 'DROITE' .or. geofis .eq. 'ENTAILLE') then
            valk(1) = 'FISSURE'
            valk(2) = geofis
            valk(3) = 'INTERFACE'
            call utmess('F', 'XFEM_23', nk=3, valk=valk)
        endif
    endif
!
!     CAS ENTAILLE : ON SE RAMENE AU CAS D'UN RECTANGLE A BORDS ARONDIS
    if (geofis .eq. 'ENTAILLE') b=r
!
    if (geofis .eq. 'ELLIPSE' .or. geofis .eq. 'RECTANGLE' .or. geofis .eq. 'CYLINDRE' .or.&
        geofis .eq. 'ENTAILLE') then
!
!       ----------------------------------------------------------------
!         TRAITEMENT DES CAS ELLIPSE, RECTANGLE, CYLINDRE ET ENTAILLE
!       ----------------------------------------------------------------
!
!       VECT1  = VECT DU DEMI-GRAND AXE
!       VECT2  = VECT DU DEMI-PETIT AXE
!       NOEUD  = CENTRE DE L'ELLIPSE / RECTANGLE /ENTAILLE
!       A      =  DEMI-GRAND AXE (OU DEMI-LONGUEUR)
!       B      =  DEMI-PETIT AXE
!       SI FISSURE :
!       COTE   =  COTE DE LA FISSURE ('IN' OU 'OUT')
!
        do ino = 1, nbno
!
!         COORDONNEES 3D DU POINT DANS LE REPERE GLOBAL
            do i = 1, ndim
                p3d(i)=vale(3*(ino-1)+i)
            end do
!
!         BASE LOCALE : (VECT1,VECT2,VECT3)
            call normev(vect1, norme)
            call normev(vect2, norme)
            call provec(vect1, vect2, vect3)
!
!         MATRICE DE PASSAGE LOC-GLOB
            do i = 1, ndim
                mat(1,i)=vect1(i)
                mat(2,i)=vect2(i)
                mat(3,i)=vect3(i)
            end do
!
!         PROJECTION DU POINT 3D DANS LE REPERE LOCAL LIE A L'ELLIPSE
            do i = 1, ndim
                ploc(i)=0.d0
                do j = 1, ndim
                    ploc(i) = ploc(i) + mat(i,j) * (p3d(j)-noeud(j))
                end do
            end do
!
!         DISTANCE DU POINT A L'ELLIPSE / RECTANGLE
            if (geofis .eq. 'ELLIPSE' .or. geofis .eq. 'CYLINDRE') then
                call disell(ploc, a, b, h)
                elseif (geofis.eq.'RECTANGLE'.or.geofis.eq.'ENTAILLE')&
            then
                call disrec(ploc, a, b, r, h)
            endif
!
!         STOCKAGE DES LEVEL SETS
            if (typdis .eq. 'FISSURE') then
!
                zl(jlnsl-1+(ino-1)+1)=.true.
                zl(jltsl-1+(ino-1)+1)=.true.
!
                if (geofis .eq. 'ELLIPSE' .or. geofis .eq. 'RECTANGLE') then
!
!             LEVEL SET NORMALE CORRESPOND A LA 3EME COORDONNEE LOCALE
                    zr(jlnsv-1+(ino-1)+1)=ploc(3)
!
!             SI LA FISSURE EST A L'EXTERIEUR DE L'ELLIPSE, ON PREND
!             L'OPPOSEE DE H (PAR DEFAUT, LA FISSURE EST A L'INTERIEUR)
                    if (cote .eq. 'OUT') h = -1.d0 * h
!
!             LEVEL SET TANGENTE CORRESPOND A LA DISTANCE DU POINT
!             A L'ELLIPSE / RECTANGLE DANS LE PLAN (VECT1,VECT2)
                    zr(jltsv-1+(ino-1)+1)=h
!
                else if (geofis.eq.'CYLINDRE') then
!
                    zr(jltsv-1+(ino-1)+1) = ploc(3)
                    zr(jlnsv-1+(ino-1)+1) = h
!
                endif
!
            else if (typdis.eq.'INTERFACE') then
!
                zr(jlnsv-1+(ino-1)+1)= h
                zl(jlnsl-1+(ino-1)+1)=.true.
!
!           LEVEL SET TANGENTE PAS DEFINIE
                ASSERT(.not.callst)
                zr(jltsv-1+(ino-1)+1)= -1.d0
                zl(jltsl-1+(ino-1)+1)=.true.
!
            endif
!
        end do
!
    else if (geofis.eq.'DEMI_PLAN') then
!
!       ----------------------------------------------------------------
!                  TRAITEMENT DU CAS DEMI-PLAN
!       ----------------------------------------------------------------
!
!       VECT1 = VECT NORMAL AU PLAN DE FISSURE
!       VECT2 = VECT DANS LE PLAN DE FISSURE
!        (NORMALE AU FOND : DIRECTION DE PROPAGATION POTENTIELLE)
!       NOEUD = NOEUD DU FOND DE FISSURE
!
        do ino = 1, nbno
!
!         COORDONNEES 3D DU POINT DANS LE REPERE GLOBAL
            do i = 1, ndim
                p3d(i)=vale(3*(ino-1)+i)
            end do
!
!         BASE LOCALE : (VECT2,VECT3,VECT1)
            call normev(vect1, norme)
            call normev(vect2, norme)
            call provec(vect1, vect2, vect3)
!
!         MATRICE DE PASSAGE LOC-GLOB
            do i = 1, 3
                mat(1,i)=vect2(i)
                mat(2,i)=vect3(i)
                mat(3,i)=vect1(i)
            end do
!
!         PROJECTION DU POINT 3D DANS LE REPERE LOCAL
            do i = 1, 3
                ploc(i)=0.d0
                do j = 1, 3
                    ploc(i) = ploc(i) + mat(i,j) * (p3d(j)-noeud(j))
                end do
            end do
!
!         LEVEL SET NORMALE CORRESPOND A LA 3EME COORDONNEE LOCALE
            zr(jlnsv-1+(ino-1)+1)= ploc(3)
            zl(jlnsl-1+(ino-1)+1)=.true.
!
!         LEVEL SET TANGENTE CORRESPOND A LA 1ERE COORDONNEE LOCALE
            zr(jltsv-1+(ino-1)+1)= ploc(1)
            zl(jltsl-1+(ino-1)+1)=.true.
!
        end do
!
    else if (geofis.eq.'SEGMENT') then
!
!       ----------------------------------------------------------------
!                  TRAITEMENT DU CAS SEGMENT
!       ----------------------------------------------------------------
!
!       VECT1 = NOEUD DU FOND ORIGINE
!       VECT2 = NOEUD DU FOND EXTREMITE
!
        do i = 1, 3
            nori(i) = vect1(i)
            next(i) = vect2(i)
            nmil(i) = (nori(i) + next(i))/2
            vseg(i) = next(i)-nori(i)
        end do
!
        nseg = sqrt(vseg(1)**2 + vseg(2)**2 + vseg(3)**2)
!
        do ino = 1, nbno
!
!         COORDONNEES 2D DU POINT DANS LE REPERE GLOBAL
            do i = 1, ndim
                p2d(i)=vale(3*(ino-1)+i)
            end do
!
            vect3(1) = 0.d0
            vect3(2) = 0.d0
            vect3(3) = 1.d0
!
!         BASE LOCALE : (VSEG,VECT2)
            call normev(vseg, norme)
            call provec(vect3, vseg, vect2)
!
!         MATRICE DE PASSAGE LOC-GLOB
            do i = 1, 2
                mat(1,i)=vseg(i)
                mat(2,i)=vect2(i)
            end do
!
!         PROJECTION DU POINT 2D DANS LE REPERE LOCAL
!         POSITIONNE AU CENTRE DU SEGEMENT
            do i = 1, 2
                ploc(i)=0.d0
                do j = 1, 2
                    ploc(i) = ploc(i) + mat(i,j) * (p2d(j)-nmil(j))
                end do
            end do
!
!         LEVEL SET NORMALE CORRESPOND A LA 2EME COORDONNEE LOCALE
            zr(jlnsv-1+(ino-1)+1)= ploc(2)
            zl(jlnsl-1+(ino-1)+1)=.true.
!
!         LEVEL SET TANGENTE EST DEFINIE PAR :
            zr(jltsv-1+(ino-1)+1)= abs(ploc(1)) - nseg/2
            zl(jltsl-1+(ino-1)+1)=.true.
!
        end do
!
!
    else if (geofis.eq.'DEMI_DROITE'.or. geofis.eq.'DROITE') then
!
!       ----------------------------------------------------------------
!                   TRAITEMENT DES CAS DEMI_DROITE ET DROITE
!       ----------------------------------------------------------------
!
!       POUR LA DEMI-DROITE :
!         VECT1 = VECTEUR DIRECTEUR DE LA DEMI-DROITE
!                 DANS LA DIRECTION DE PROPA
!         NOEUD = NOEUD DU FOND DE FISSURE
!
!       POUR LA DROITE :
!         VECT1 = VECTEUR DIRECTEUR DE LA  DROITE
!         NOEUD = UN POINT DE LA DROITE
!
        do ino = 1, nbno
!
!         COORDONNEES 2D DU POINT DANS LE REPERE GLOBAL
            do i = 1, ndim
                p2d(i)=vale(3*(ino-1)+i)
            end do
!
            vect3(1) = 0.d0
            vect3(2) = 0.d0
            vect3(3) = 1.d0
!
!         BASE LOCALE : (VECT1,VECT2)
            call normev(vect1, norme)
            call provec(vect3, vect1, vect2)
!
!         MATRICE DE PASSAGE LOC-GLOB
            do i = 1, 2
                mat(1,i)=vect1(i)
                mat(2,i)=vect2(i)
            end do
!
!         PROJECTION DU POINT 2D DANS LE REPERE LOCAL
            do i = 1, 2
                ploc(i)=0.d0
                do j = 1, 2
                    ploc(i) = ploc(i) + mat(i,j) * (p2d(j)-noeud(j))
                end do
            end do
!
!         LEVEL SET NORMALE CORRESPOND A LA 2EME COORDONNEE LOCALE
            zr(jlnsv-1+(ino-1)+1)= ploc(2)
            zl(jlnsl-1+(ino-1)+1)=.true.
!
            if (geofis .eq. 'DEMI_DROITE') then
!
!           LEVEL SET TANGENTE CORRESPOND A LA 1ERE COORDONNEE LOCALE
                zr(jltsv-1+(ino-1)+1)= ploc(1)
                zl(jltsl-1+(ino-1)+1)=.true.
!
            else if (geofis.eq.'DROITE') then
!
!           LEVEL SET TANGENTE PAS DEFINIE
                ASSERT(.not.callst)
                zr(jltsv-1+(ino-1)+1)= -1.d0
                zl(jltsl-1+(ino-1)+1)=.true.
!
            endif
!
        end do
!
    endif
!
    call jedema()
end subroutine
