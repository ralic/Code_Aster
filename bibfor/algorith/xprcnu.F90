subroutine xprcnu(noma, cnxinv, base, vcn, grlr, lcmin)
    implicit none
!
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: noma
    character(len=19) :: cnxinv
    character(len=24) :: vcn, grlr
    character(len=1) :: base
    real(kind=8) :: lcmin
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
!     ------------------------------------------------------------------
!
!       XPRCNU   : X-FEM PROPAGATION : CREATION DE LA TABLE DE CONNEXION
!       ------     -     --                                    -
!                                       DES NODES POUR LA METHODE FAST MARCHING UPWIND
!                                           -                     -
!
!    DANS LE CADRE DE LA PROPAGATION DE FISSURE XFEM AVEC LA METHODE
!    FAST MARCHING UPWIND, ON CALCULE LA BASE LOCALE DE LA GRILLE 
!    UTILISEE ET ON ARRANGE LES NOEUDS SELON LA DIRECTION DES AXES DE CETTE BASE
!
!    ENTREE
!            NOMA   = NOM DU MAILLAGE SUR LEQUEL LES LEVEL SETS SONT
!                     STOCKES
!            CNXINV = CONNECTIVITE INVERSEE DU MAILLAGE NOMA
!            BASE   = 'V' OU 'G'. BASE JEVEUX SUR LAQUELLE LES OBJETS
!                     VCN, VCND ET REFJEV (VOIR CI-DESSOUS) SERONT
!                     CREES. DANS LE CAS OU LE MAILLAGE NOMA EST UNE
!                     GRILLE AUXILIAIRE, C'EST BIEN DE UTILISER LA BASE
!                     GLOBALE POUR LES ATTACHER A LA SD DU MODELE DE
!                     LA GRILLE.
!            VCN    = NOM DE L'OBJET JEVEUX OU STOCKER LA TABLE (VOIR
!                     CI-DESSOUS DANS LE CODE POUR LA DESCRIPTION DE LA
!                     TABLE)
!            GRLR   = NOM DE L'OBJET JEVEUX OU STOCKER LA TABLE VCND,
!                     LA BASE LOCALE DE LA GRILLE ET LA LONGUEUER DE
!                     LA PLUS PETITE ARETE DE LA GRILLE (VOIR SD_GRILLE
!                     ET CI-DESSOUS DANS LE CODE POUR UNE DESCRIPTION DE
!                     CES OBJETS)
!
!    SORTIE
!            VCN    = OBJET CREE
!            GRLR   = OBJET CREE
!
!     ------------------------------------------------------------------
!
!
!     MESH INFORMATION RETREIVING AND GENERAL PURPOSE VARIABLES
    integer :: nbno, nbma,   jconx2,  itypma
    integer :: ifm, niv, ndim, ndime, dimuns
    character(len=8) :: typma
    integer :: i, j
!
!     LOCAL REFERENCE SYSTEM
    real(kind=8) :: locref(3, 3), nodref(4, 3), modvec, partol
    parameter        (partol=1.d-2)
    integer :: elmori, notpar, jref
!
!     CREATION OF THE CONNECTION TABLE OF THE NODES
    integer :: jgrlr, jvcn, jvcnd
    integer :: node, nodeps, nodedg, nbelno, elno, jelno, elnol
    real(kind=8) :: nodxyz(2, 3), absxyz(3), locxyz(3)
    integer :: nodcon(3), eldef(8), nocur, orph, unsupp, ar(12, 3), nbar
    integer :: maxedg(3), numnod(3)
    integer, pointer :: typmail(:) => null()
    integer, pointer :: connex(:) => null()
    integer, pointer :: tmdim(:) => null()
    real(kind=8), pointer :: vale(:) => null()
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
! SET THE NUMBER OF EDGES OF THE ELEMENT SHARING ONE NODE FOR 2D MODELS
    maxedg(2) = 2
! SET THE NUMBER OF EDGES OF THE ELEMENT SHARING ONE NODE FOR 3D MODELS
    maxedg(3) = 3
!
! SET THE NUMBER OF NODES IN THE SUPPORTED 2D ELEMENTS
    numnod(2) = 4
! SET THE NUMBER OF NODES IN THE SUPPORTED 3D ELEMENTS
    numnod(3) = 8
!
! SET THE MAXIMUM DIMENSION OF THE UNSUPPORTED ELEMENTS
    dimuns = 0
!
! RETRIEVE THE NUMBER OF NODES AND ELEMENTS IN THE MESH
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
! RETRIEVE THE COORDINATES OF THE NODES
!                12345678901234567890
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
! RETRIEVE THE DEFINITION OF THE ELEMENTS IN TERMS OF NODES
    call jeveuo(noma//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
! RETRIEVE THE TYPE OF EACH ELEMENT IN THE MESH
    call jeveuo(noma//'.TYPMAIL', 'L', vi=typmail)
! RETRIEVE THE DIMENSIONS OF THE EXISTING ELEMENTS
    call jeveuo('&CATA.TM.TMDIM', 'L', vi=tmdim)
! RETRIEVE THE DIMENSION OF THE PROBLEM (2D AND 3D ARE SUPPORTED)
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
!
!     CREATE THE JEVEUX OBJECTS WHERE THE RESULTS WILL BE STORED
    call wkvect(vcn, base//' V I', 6*nbno, jvcn)
    call wkvect(grlr, base//' V R', 10+6*nbno, jgrlr)
!
!     SET THE POINTERS FOR THE DIFFERENT PARTS OF THE GRLR OBJECT
    jref = jgrlr+1
    jvcnd = jgrlr+10
!
! ----------------------------------------------------------------------
! FIRST OF ALL, THE LOCAL REFERENCE SYSTEM FOR THE GRID MUST BE
! CALCULATED. IT WILL BE USED BELOW FOR THE NODE ORDERING TASK.
! ----------------------------------------------------------------------
!
    elmori = 0
!
!     SEARCH THE FIRST SUPPORTED ELEMENT IN THE MESH
    do i = 1, nbma
        itypma=typmail(i)
        call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
        if (((ndim.eq.2).and.(typma(1:5).eq.'QUAD4')) .or.&
            ((ndim.eq.3) .and.(typma(1:5).eq.'HEXA8'))) then
            elmori = i
            exit
        endif
    end do
!
!     CHECK IF A SUPPORTED ELEMENT HAS BEEN FOUND
    if (elmori .eq. 0) then
        call utmess('F', 'XFEM2_54')
    endif
!
!     RETRIEVE THE NODE CONNECTION OF ALL THE EDGES OF THE FIRST
!     SUPPORTED ELEMENT
    call conare(typma, ar, nbar)
!
!     RETRIEVE THE TWO (2D CASE) OR THREE (3D CASE) EDGES THAT SHARE THE
!     NODE AT THE ORIGIN OF THE LOCAL REFERENCE SYSTEM
    j=0
    do i = 1, nbar
        if (ar(i,1) .eq. 1) then
            j=j+1
!           THE NUMBER OF EDGES SHARING THE NODE SHOULD ALWAYS BE
!           LOWER OR EQUAL TO THE MAXIMUM VALUE EXPECTED
            ASSERT(j.le.maxedg(ndim))
            nodcon(j) = ar(i,2)
        endif
!
        if (ar(i,2) .eq. 1) then
            j = j+1
!           THE NUMBER OF EDGES SHARING THE NODE SHOULD ALWAYS BE
!           LOWER OR EQUAL TO THE MAXIMUM VALUE EXPECTED
            ASSERT(j.le.maxedg(ndim))
            nodcon(j) = ar(i,1)
        endif
    end do
!
!     THE NUMBER OF EDGES RETRIEVED SHOULD ALWAYS BE EQUAL TO
!     THE NUMBER OF THE EXPECTED EDGES
    ASSERT(j.eq.maxedg(ndim))
!
!     RETRIEVE THE COORDINATES OF THE ORIGIN
    j = connex(zi(jconx2-1+elmori)+1-1)
    nodref(1,1) = vale(3*(j-1)+1)
    nodref(1,2) = vale(3*(j-1)+2)
    nodref(1,3) = vale(3*(j-1)+3)
!
!     RETRIEVE THE COORDINATES OF THE OTHER NODES DEFINING THE LOCAL
!     AXES (Xl,Yl,Zl)
    do i = 1, ndim
        j = connex(zi(jconx2-1+elmori)+nodcon(i)-1)
        nodref(i+1,1) = vale(3*(j-1)+1)
        nodref(i+1,2) = vale(3*(j-1)+2)
        nodref(i+1,3) = vale(3*(j-1)+3)
    end do
!
!     EVALUATE THE UNIT VECTORS DEFINING THE THREE LOCAL AXES
    do i = 1, ndim
!        VECTOR...
        locref(i,1) = nodref(i+1,1) - nodref(1,1)
        locref(i,2) = nodref(i+1,2) - nodref(1,2)
        locref(i,3) = nodref(i+1,3) - nodref(1,3)
!        ...MODULE OF THE VECTOR...
        modvec = (locref(i,1)**2+locref(i,2)**2+locref(i,3)**2)** 0.5d0
!        ...UNIT VECTOR!
        locref(i,1) = locref(i,1)/modvec
        locref(i,2) = locref(i,2)/modvec
        locref(i,3) = locref(i,3)/modvec
    end do
!
!     FOR THE 2D CASE, THE Zl AXIS DIRECTION IS KNOWN IN ADVANCE BECAUSE
!     IT'S COINCIDENT WITH THE GLOBAL Z AXIS
    if (ndim .eq. 2) then
        locref(3,1) = 0
        locref(3,2) = 0
        locref(3,3) = 1
    endif
!
!     CHECK IF THE LOCAL AXES ARE ORTHOGONAL EACH OTHER
!     Xl - Yl
    modvec = locref(1,1)*locref(2,1)+locref(1,2)*locref(2,2)+ locref(1,3)*locref(2,3)
    if (abs(modvec) .gt. partol) then
        call utmess('F', 'XFEM2_55')
    endif
!     Xl - Zl
    modvec = locref(1,1)*locref(3,1)+locref(1,2)*locref(3,2)+ locref(1,3)*locref(3,3)
    if (abs(modvec) .gt. partol) then
        call utmess('F', 'XFEM2_55')
    endif
!     Yl - Zl
    modvec = locref(2,1)*locref(3,1)+locref(2,2)*locref(3,2)+ locref(2,3)*locref(3,3)
    if (abs(modvec) .gt. partol) then
        call utmess('F', 'XFEM2_55')
    endif
!
!     THE DIRECTIONS OF THE THREE AXES ARE CORRECT (ORTHOGONAL TO EACH
!     OTHER) BUT THE SENSE COULD BE WRONG. THEREFORE THE Z-AXIS IS
!     RECALCULATED IN SUCH A WAY THAT ITS SENSE IS COHERENT WITH THE
!     SENSE OF X AND Y AXES. IT IS CALCULATED AS THE VECTORIAL PRODUCT
!     OF X AND Y AXES.
    locref(3,1) = locref(1,2)*locref(2,3)-locref(1,3)*locref(2,2)
    locref(3,2) = locref(1,3)*locref(2,1)-locref(1,1)*locref(2,3)
    locref(3,3) = locref(1,1)*locref(2,2)-locref(1,2)*locref(2,1)
!
!     STORE THE LOCAL REFERENCE SYSTEM IN THE JEVEUO OBJECT. THIS WILL
!     BE USED LATER BY XPRUPW.F
!     THIS BASE IS STORED IN A 9 ELEMENTS VECTOR. ELEMENTS 1 TO 3
!     ARE X-AXIS COMPONENTS, ELEMENTS 4 TO 6 ARE Y-AXIS COMPONENTS AND
!     ELEMENTS 7 TO 9 ARE Z-AXIS COMPONENT.
    do i = 1, 3
        zr(jref-1+3*(i-1)+1) = locref(i,1)
        zr(jref-1+3*(i-1)+2) = locref(i,2)
        zr(jref-1+3*(i-1)+3) = locref(i,3)
    end do
!
!-----------------------------------------------------------------------
! CREATION OF THE TWO VECTORS DESCRIBING THE CONNECTION OF THE NODES.
! FOR EACH NODE IN THE MESH, THE DATA CONTAINED IN THE TWO VECTORS
! ARE AS FOLLOWS:
!
! VECTOR 1, INTEGERS, 6 ELEMENTS: N+X,N-X,N+Y,N-Y,N+Z,N-Z
! VECTOR 2, REALS, 6 ELEMENTS: DX+,DX-,DY+,DY-,DZ+,DZ-
!
! WHERE
!   N+i  = NEAREST NODE IN POSITIVE i-DIRECTION
!   Di+  = DISTANCE BETWEEN THE CURRENT NODE AND N+i (IN i-DIRECTION,
!          OF COURSE!)
!   N-i  = NEAREST NODE IN NEGATIVE i-DIRECTION
!   Di-  = DISTANCE BETWEEN THE CURRENT NODE AND N-i (OPPOSITE TO
!          i-DIRECTION, OF COURSE!)
!-----------------------------------------------------------------------
!
!     CHECK THE NUMBER OF UNSUPPORTED ELEMENTS IN THE MESH
    unsupp = 0
!
!     CHECK THE NUMBER OF EDGES IN THE MESH THAT ARE NOT PARALLEL TO
!     THE LOCAL REFERENCE SYSTEM
    notpar = 0
!
!     SHORTEST EDGE IN THE GRID
    lcmin=r8maem()
!
!     ANALYSE EACH NODE IN THE MESH
    do node = 1, nbno
!
!        RETRIEVE THE COORDINATES OF THE NODE
        nodxyz(1,1) = vale(3*(node-1)+1)
        nodxyz(1,2) = vale(3*(node-1)+2)
        nodxyz(1,3) = vale(3*(node-1)+3)
!
!        RETRIEVE THE ELEMENTS CONTAINING THE NODE
        call jelira(jexnum(cnxinv, node), 'LONMAX', nbelno)
        call jeveuo(jexnum(cnxinv, node), 'L', jelno)
!
!        FOR EACH OF THESE ELEMENTS, RETRIEVE THE THREE EDGES CONTAINING
!        THE ACTUAL NODE AND DETERMINE THEIR ORIENTATION AND LENGTH
        do elnol = 1, nbelno
!
!           GET THE ELEMENT NUMBER
            elno = zi(jelno-1+elnol)
!
!           ONLY THE SUPPORTED ELEMENTS ARE CONSIDERED
            itypma=typmail(elno)
            call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
!
            if (((typma(1:5).eq.'HEXA8').and.(ndim.eq.3)) .or.&
                ((typma( 1:5).eq.'QUAD4').and.(ndim.eq.2))) then
!
!              GET THE ELEMENT DEFINITION AND THE NODE POSITION INTO THE
!              ELEMENT DEFINITION
                nodeps = 0
                do nocur = 1, numnod(ndim)
                    eldef(nocur) = connex(zi(jconx2-1+elno)+ nocur-1)
                    if (eldef(nocur) .eq. node) nodeps=nocur
                end do
!
!              THE NODE SHOULD ALWAYS BE PRESENT INTO THE ELEMENT
!              DEFINITION. HOWEVER IT IS BETTER TO CHECK IT, JUST IN THE
!              CASE SOMETHING IS WRONG IN THE MESH DEFINITION
                ASSERT(nodeps.gt.0)
!
!              RETRIEVE ALL THE EDGES OF THE ELEMENT
                call conare(typma, ar, nbar)
!
!              RETRIEVE THE THREE EDGES CONTAINING THE ACTUAL NODE. ONLY
!              THE POSITION INTO THE ELEMENT DEFINITION OF THE EDGE
!              NODES DIFFERENT FROM THE CURRENT ONE IS STORED.
                j=0
                do i = 1, nbar
!                 IF THE CURRENT NODE IS THE FIRST IN THE EDGE
!                 DEFINITION, THE SECOND NODE IN THE EDGE DEFINITION IS
!                 STORED
                    if (ar(i,1) .eq. nodeps) then
                        j = j+1
!                   THE NUMBER OF EDGES SHARING THE NODE SHOULD ALWAYS
!                   BE LOWER OR EQUAL TO THE MAXIMUM VALUE EXPECTED
                        ASSERT(j.le.maxedg(ndim))
                        nodcon(j) = ar(i,2)
                    endif
!
!                 IF THE CURRENT NODE IS THE SECOND IN THE EDGE
!                 DEFINITION, THE FIRST NODE IN THE EDGE DEFINITION IS
!                 STORED
                    if (ar(i,2) .eq. nodeps) then
                        j = j+1
!                   THE NUMBER OF EDGES SHARING THE NODE SHOULD ALWAYS
!                   BE LOWER OR EQUAL TO THE MAXIMUM VALUE EXPECTED
                        ASSERT(j.le.maxedg(ndim))
                        nodcon(j) = ar(i,1)
                    endif
                end do
!
!              THE NUMBER OF EDGES RETRIEVED SHOULD ALWAYS BE EQUAL TO
!              THE NUMBER OF THE EXPECTED EDGES
                ASSERT(j.eq.maxedg(ndim))
!
!              BUILD EACH EDGE SHARING THE CURRENT NODE AND EVALUATE ITS
!              DIRECTION
                do i = 1, maxedg(ndim)
                    nodedg = eldef(nodcon(i))
                    nodxyz(2,1) = vale(3*(nodedg-1)+1)
                    nodxyz(2,2) = vale(3*(nodedg-1)+2)
                    nodxyz(2,3) = vale(3*(nodedg-1)+3)
!
                    nodxyz(2,1) = nodxyz(2,1) - nodxyz(1,1)
                    nodxyz(2,2) = nodxyz(2,2) - nodxyz(1,2)
                    nodxyz(2,3) = nodxyz(2,3) - nodxyz(1,3)
!
!                 EXPRESSS THE EDGE VECTOR (EXPRESSED IN THE GLOBAL
!                 REFERENCE SYSTEM) IN THE LOCAL REFERENCE SYSTEM
                    do j = 1, 3
                        locxyz(j) = (&
                                    nodxyz(2,1)*locref(j,1)+ nodxyz( 2,2)*locref(j,2)+ nodxyz(2,3&
                                    &)*locref(j,3)&
                                    )
                    end do
!
!                 EVALUATE THE ABSOLUTE VALUE OF THE COMPONENTS OF THE
!                 EDGE VECTOR EXPRESSED IN THE LOCAL REFERENCE SYSTEM
                    absxyz(1) = abs(locxyz(1))
                    absxyz(2) = abs(locxyz(2))
                    absxyz(3) = abs(locxyz(3))
!
!                 TOLERANCE ON THE LOCAL COMPONENTS OF THE EDGE VECTOR
!                 USED TO CHECK IF THE EDGE IS PARALLEL TO ONE LOCAL
!                 AXIS
                    modvec = ((absxyz(1)**2+absxyz(2)**2+absxyz(3)**2) **0.5d0 )*partol
!
                    if ((absxyz(1).gt.absxyz(2)) .and. (absxyz(1) .gt.absxyz(3))) then
!
!                    CHECK IF THE EDGE IS REALLY PARALLEL TO Xl AXIS
                        if (.not.(&
                            (absxyz(1).gt.modvec) .and. (absxyz(2) .lt.modvec) .and.&
                            (absxyz(3).lt.modvec)&
                            )) then
                            notpar = notpar+1
                        endif
!
!                    CHECK THAT THE VALUE OF DELTAX IS GREATER THAN ZERO
                        if (.not.(absxyz(1).gt.r8prem())) then
                            call utmess('F', 'XFEM2_57')
                        endif
!
!                    CALCULATE THE SHORTEST EDGE IN THE GRID
                        if (absxyz(1) .lt. lcmin) lcmin=absxyz(1)
!
                        if (locxyz(1) .gt. 0) then
!                       EDGE PARALLEL TO Xl-AXIS, DELTAX POSITIVE
                            zi(jvcn-1+6*(node-1)+1) = nodedg
                            zr(jvcnd-1+6*(node-1)+1) = absxyz(1)
                        else
!                       EDGE PARALLEL TO Xl-AXIS, DELTAX NEGATIVE
                            zi(jvcn-1+6*(node-1)+2) = nodedg
                            zr(jvcnd-1+6*(node-1)+2) = absxyz(1)
                        endif
!
                    else
                        if (absxyz(2) .gt. absxyz(3)) then
!
!                           CHECK IF THE EDGE IS REALLY PARALLEL TO Yl
!                           AXIS
                            if (.not.(&
                                (absxyz(2).gt.modvec) .and. (absxyz(1).lt.modvec) .and.&
                                (absxyz(3) .lt.modvec)&
                                )) then
                                notpar = notpar+1
                            endif
!
!                           CHECK THAT THE VALUE OF DELTAY IS GREATER
!                           THAN ZERO
                            if (.not.(absxyz(2).gt.r8prem())) then
                                call utmess('F', 'XFEM2_57')
                            endif
!
!                           CALCULATE THE SHORTEST EDGE IN THE GRID
                            if (absxyz(2) .lt. lcmin) lcmin=absxyz(2)
!
                            if (locxyz(2) .gt. 0) then
!                              EDGE PARALLEL TO Yl-AXIS, DELTAY POSITIVE
                                zi(jvcn-1+6*(node-1)+3) = nodedg
                                zr(jvcnd-1+6*(node-1)+3) = absxyz(2)
                            else
!                              EDGE PARALLEL TO Yl-AXIS, DELTAY NEGATIVE
                                zi(jvcn-1+6*(node-1)+4) = nodedg
                                zr(jvcnd-1+6*(node-1)+4) = absxyz(2)
                            endif
!
                        else
!
!                           CHECK IF THE EDGE IS REALLY PARALLEL TO Zl
!                           AXIS
                            if (.not.(&
                                (absxyz(3).gt.modvec) .and. (absxyz(1).lt.modvec) .and.&
                                (absxyz(2) .lt.modvec)&
                                )) then
                                notpar = notpar+1
                            endif
!                           CHECK THAT THE VALUE OF DELTAZ IS GREATER
!                           THAN ZERO
                            if (.not.(absxyz(3).gt.r8prem())) then
                                call utmess('F', 'XFEM2_57')
                            endif
!
!                           CALCULATE THE SHORTEST EDGE IN THE GRID
                            if (absxyz(3) .lt. lcmin) lcmin=absxyz(3)
!
                            if (locxyz(3) .gt. 0) then
!                              EDGE PARALLEL TO Zl-AXIS, DELTAZ POSITIVE
                                zi(jvcn-1+6*(node-1)+5) = nodedg
                                zr(jvcnd-1+6*(node-1)+5) = absxyz(3)
                            else
!                              EDGE PARALLEL TO Zl-AXIS, DELTAZ NEGATIVE
                                zi(jvcn-1+6*(node-1)+6) = nodedg
                                zr(jvcnd-1+6*(node-1)+6) = absxyz(3)
                            endif
!
                        endif
!
                    endif
!
                end do
!
            else
!
!               THE ELEMENT IS NOT SUPPORTED
                unsupp = unsupp + 1
!
!               CHECK THE DIMENSION OF THE UNSUPPORTED ELEMENT
                ndime=tmdim(itypma)
!
!               STORE THE MAXIMUM DIMENSION OF THE UNSUPPORTED ELEMENTS
                if (ndime .gt. dimuns) dimuns=ndime
!
            endif
!
        end do
!
    end do
!
!     IF EDGES NOT PARALLEL TO THE LOCAL REFERENCE SYSTEM HAVE BEEN
!     DETECTED, A FATAL ERROR IS ISSUED
    if (notpar .gt. 0) then
        call utmess('F', 'XFEM2_55')
    endif
!
!     ANALYSE EACH NODE IN THE MESH TO CHECK IF THERE ARE SOME NODES
!     THAT DO NOT BELONG TO THE TYPE OF ELEMENT CONSIDERED FOR THE
!     UPWIND SCHEME
!
    orph = 0
    do node = 1, nbno
!
        i = 0
!
!        CHECK THE NEIGHBORING NODES IN Xl AND Yl DIRECTIONS
        do nocur = 1, 4
            if (zi(jvcn-1+6*(node-1)+nocur) .eq. 0) i = i+1
        end do
!
!        CHECK THE NEIGHBORING NODES IN Zl DIRECTION ONLY FOR THE 3D
!        CASE
        if (ndim .eq. 3) then
            do nocur = 5, 6
                if (zi(jvcn-1+6*(node-1)+nocur) .eq. 0) i = i+1
            end do
        endif
!
!        THE NODE DOES NOT BELONG TO ANY ALLOWED ELEMENT FOR THE UPWIND
!        SCHEME. HERE I LABEL THIS NODE AS "ORPHAN NODE".
        if (((i.eq.6).and.(ndim.eq.3)) .or. ((i.eq.4).and.(ndim.eq.2))) orph=orph+1
!
    end do
!
!     MANAGE THE UNSUPPORTED ELEMENTS
    if (unsupp .gt. 0) then
!
!        CASE 1: ALL THE UNSUPPORTED ELEMENTS ARE OF LOWER DIMENSION
!                THEN THE DIMENSION OF THE PROBLEM AND THERE ARE SOME
!                ORPHAN NODES
        if ((dimuns.lt.ndim) .and. (orph.gt.0)) then
            if (ndim .eq. 3) then
                call utmess('F', 'XFEM2_52')
            else
                call utmess('F', 'XFEM2_50')
            endif
        endif
!
!        CASE 2: AT LEAST ONE OF THE UNSUPPORTED ELEMENTS HAS THE SAME
!                DIMENSION OF THE PROBLEM
        if (dimuns .ge. ndim) then
            call utmess('F', 'XFEM2_53')
        endif
!
    endif
!
!     STORE THE LCMIN VALUE IN THE GRLR OBJECT
    zr(jgrlr-1+1) = lcmin
!
    if (niv .gt. 1) then
        write(ifm,900)
        write(ifm,905)
        do i = 1, nbno
            write(ifm,901)i,zi(jvcn-1+6*(i-1)+1),zi(jvcn-1+6*(i-1)+2),&
            zi(jvcn-1+6*(i-1)+3),zi(jvcn-1+6*(i-1)+4),zi(jvcn-1+6*(i-&
            1)+5), zi(jvcn-1+6*(i-1)+6)
            write(ifm,904)zr(jvcnd-1+6*(i-1)+1),zr(jvcnd-1+6*(i-1)+2),&
            zr(jvcnd-1+6*(i-1)+3),zr(jvcnd-1+6*(i-1)+4),zr(jvcnd-1+6*(&
            i-1)+5), zr(jvcnd-1+6*(i-1)+6)
        end do
    endif
!
    900 format('NODE  | NX+  | NX-  | NY+  | NY-  | NZ+  | NZ-  |')
    901 format(i6,6('|',i6),'|')
    904 format('      ',6('|',f6.3),'|')
    905 format('      | DX+  | DX-  | DY+  | DY-  | DZ+  | DZ-  |')
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
