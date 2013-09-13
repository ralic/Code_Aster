subroutine xprtor(method, model, noma, cnxinv, fispre,&
                  fiss, vcn, grlr, cnsln, grln,&
                  cnslt, grlt, tore, radtor, radimp,&
                  cnsdis, disfr, cnsbl, nodcal, elecal,&
                  liggrd, vcnt, grlrt)
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescns.h"
#include "asterfort/cnscno.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlim1.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeundf.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: method, model, noma, fispre, fiss
    character(len=19) :: cnsln, grln, cnslt, grlt, nodcal, elecal, cnsdis, disfr
    character(len=19) :: cnsbl, cnxinv, liggrd
    character(len=24) :: vcn, grlr, vcnt, grlrt
    logical :: tore
    real(kind=8) :: radtor, radimp
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
!       XPRTOR   : X-FEM PROPAGATION : CREATION DU TOR POUR LA
!       ------     -     --                        ---
!                  LOCALISATION DU DOMAINE DU CALCUL
!
! DANS LE CONTEXTE DE LA PROPAGATION X-FEM ET DE LA REPRESENTATION DE LA
! FISSURE PAR LEVEL SETS (METHODES UPWIND ET SIMPLEXE), CETTE ROUTINE
! DEFINIT UN DOMAINE DE CALCUL LOCALISE AUTOUR DU FOND DE LA FISSURE.
! CELA PERMET DE RENDRE LE CALCUL PLUS RAPIDE ET FIABLE.
!
!    ENTREE
!    ------
!      METHOD = METHODE UTILISE POUR LA REINITIALISATION ET LA
!               REORTHOGONALISATION DES LEVEL SETS (UPWIND/SIMPLEXE)
!      MODEL  = NOM DU MODELE SUR LEQUEL LES LEVEL SETS SONT DEFINIES
!      NOMA   = NOM DU MAILLAGE DU MODELE
!      CNXINV = CONNECTIVITE INVERSEE DU MAILLAGE NOMA
!      FISPRE = NOM DU CONCEPT FISSURE X-FEM DE LA FISSURE A PROPAGER
!      FISS   = NOM DU CONCEPT FISSURE X-FEM DE LA NOUVELLE FISSURE
!      VCN    = METHODE UPWIND SEULEMENT. VOIR XPRCNU.F POUR LA
!               DESCRIPTION DE CET OBJET.
!      GRLR   = METHODE UPWIND SEULEMENT. VOIR XPRCNU.F POUR LA
!               DESCRIPTION DE CET OBJET.
!      CNSLN  = CHAMP_NO_S DES VALEURS DE LA LEVEL SET NORMALE
!      GRLN   = CHAMP_NO_S DES VALEURS DU GRADIENT DE CNSLN
!      CNSLT  = CHAMP_NO_S DES VALEURS DE LA LEVEL SET TANGENTE
!      GRLT   = CHAMP_NO_S DES VALEURS DU GRADIENT DE CNSLT
!      TORE   = .TRUE. SI LE TORE DOIT ETRE CREE (LOCALISATION ACTIVE)
!               .FALSE. SI LE MAILLAGE ENTIER DOIT ETRE UTILISE
!      RADTOR = VALEUR MINIMALE DU RAYON DU TORE A UTILISER
!      RADIMP = VALEUR DU RAYON DU TORE A IMPOSER (PLUS GRANDE DE CELLE
!               QUI EST DONNEE PAR RADTOR)
!               SI LE CALCUL DOIT ETRE FAITE AUTOMATIQUEMENT, ON DONNE
!               UNE VALEUR NEGATIVE
!      CNSDIS = CHAM_NO_S VECTEUR DISTANCE ENTRE CHAQUE NODE DU
!               MAILLAGE ET SON PROJECTION SUR LE FOND DE FISSURE
!      DISFR  = VECTEUR INDIQUANT LA DISTANCE^2 ENTRE CHAQUE NODE DU
!               MAILLAGE NOMA ET LE FOND DU FISSURE
!      CNSBL  = CHAM_NO_S BASE LOCALE POUR CHAQUE NODE DU MAILLAGE
!               (AXE NORMALE ET AXE TANGENTE AU PLANE DE LA FISSURE)
!
!    SORTIE
!    ------
!      NODCAL = VECTEUR CONTENANT LE NUMERO DES NOEUDS DANS LE TORE
!      ELECAL = VECTEUR CONTENANT LE NUMERO DES ELEMENTS DANS LE TORE
!      LIGGRD = LIGREL DEFINISSENT LE TORE (A UTILISER POUR LE CALCUL
!               DES GRADIENT DES LEVEL SETS)
!      VCNT   = METHODE UPWIND SEULEMENT. VECTEUR CONNEXION NODALE TORE.
!               VOIR XPRCNU.F POUR LA DESCRIPTION DE CETTE OBJET.
!      GRLRT  = METHODE UPWIND SEULEMENT. VECTEUR DISTANCE NODALE TORE.
!               VOIR XPRCNU.F POUR LA DESCRIPTION DE CETTE OBJET.
!      CNSLN  = CHAMP_NO_S DES NOUVELLES VALEURS DE LA LEVEL SET
!               NORMALE
!      GRLN   = CHAMP_NO_S DES NOUVELLES VALEURS DU GRADIENT DE CNSLN
!      CNSLT  = CHAMP_NO_S DES NOUVELLES VALEURS DE LA LEVEL SET
!               TANGENTE
!      GRLT   = CHAMP_NO_S DES NOUVELLES VALEURS DU GRADIENT DE CNSLT
!      RADTOR = VALEUR DU RAYON DU TORE UTILISEE
!
!     ------------------------------------------------------------------
!
!
!     GENERAL PURPOSE
    integer :: ibid, iret, i, j, k, ndim, ifm, niv, eldim
    character(len=8) :: k8b
    real(kind=8) :: meserr(2)
!
!     TORUS
    integer :: jlisno, nnodgr, jnocal, jdisfr, nnodto, nbelno, jnoel, jconx1
    integer :: jconx2, nocur, numelm, jmai, itypma, nbma, jelcal, neleto, jeleca
    integer :: jaux, jtmdim, nodins
    integer :: jndsup
    character(len=19) :: listel
    real(kind=8) :: rdnew
!
!     LEVEL SETS AND LOCAL BASE
    integer :: jlsn, jlst, jbl, jdisv
!
!     NODAL CONNECTION TABLE
    integer :: jvcn, jvcnd, jvcnt, jvcndt, jlisol, jgrlr, jgrlrt
    character(len=24) :: lisold
!
!     EVALUATION OF THE GRADIENT OF THE LEVEL SET
    character(len=8) :: lpain(4), lpaout(2)
    character(len=19) :: cnoln, cnolt, celgls, chams
    character(len=24) :: lchin(4), lchout(2)
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
!     RETRIEVE THE NUMBER OF NODES DEFINING THE GRID
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nnodgr,&
                k8b, iret)
!
!     RETRIEVE THE NUMBER OF ELEMENTS IN THE MESH
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                k8b, iret)
!
!     RETRIEVE THE TYPE OF EACH ELEMENT IN THE MESH
    call jeveuo(noma//'.TYPMAIL', 'L', jmai)
!
!     RETRIEVE THE DIMENSIONS OF THE EXISTING ELEMENTS
    call jeveuo('&CATA.TM.TMDIM', 'L', jtmdim)
!
!     RETRIEVE THE PROBLEM DIMENSION
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                k8b, iret)
!
!     RETRIEVE THE DEFINITION OF THE ELEMENTS IN TERMS OF NODES
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
!     ELABORATE THE TORUS ONLY IF THE LOCALIZATION HAS BEEN REQUESTED
    if (.not.tore) then
!
!        THE LOCALIZATION HAS NOT BEEN REQUESTED BY THE USER. THEREFORE
!        ALL THE ELEMENTS OF THE GRID MUST BE USED FOR THE COMPUTATION.
!
!        CREATE THE BOOLEAN LIST TO MARK THE ELEMENTS AND THE NODES
!        THAT ARE INSIDE THE TORUS
        call wkvect('&&XPRTOR.LISTELE', 'V V L', nbma, jelcal)
        call wkvect('&&XPRTOR.LISTNOD', 'V V L', nnodgr, jlisno)
!
        call jeundf('&&XPRTOR.LISTELE')
        call jeundf('&&XPRTOR.LISTNOD')
!
        call jeveuo('&&XPRTOR.LISTELE', 'E', jelcal)
        call jeveuo('&&XPRTOR.LISTNOD', 'E', jlisno)
!
!        COUNTER FOR THE NUMBER OF NODES IN THE DOMAIN
        nnodto=0
!
!        COUNTER FOR THE NUMBER OF ELEMENTS IN THE DOMAIN
        neleto=0
!
        do 200 i = 1, nbma
!
!           WORK ONLY WITH THE ELEMENTS OF THE SAME DIMENSION OF
!           THE MODEL
            itypma=zi(jmai-1+i)
            eldim=zi(jtmdim-1+itypma)
!
            if (eldim .eq. ndim) then
!
!              MARK THE ELEMENT AS INSIDE THE DOMAIN
                zl(jelcal-1+i) = .true.
!
                neleto = neleto+1
!
!              RETRIEVE THE NODES DEFINING THE ELEMENT
                call jeveuo(jexnum('&CATA.TM.NBNO', itypma), 'L', jaux)
!
                do 201 k = 1, zi(jaux)
!
!                 SELECT EACH NODE OF THE ELEMENT
                    nocur = zi(jconx1-1+zi(jconx2-1+i)+k-1)
!
!                 MARK THE NODE AS INSIDE THE DOMAIN
                    if (.not.zl(jlisno-1+nocur)) then
                        zl(jlisno-1+nocur) = .true.
                        nnodto = nnodto+1
                    endif
!
201              continue
!
            endif
!
200      continue
!
!        BUILD THE LIST OF THE NODES OF THE DOMAIN
        call wkvect(nodcal, 'V V I', nnodto, jnocal)
        j=1
        do 202 i = 1, nnodgr
            if (zl(jlisno-1+i)) then
                zi(jnocal-1+j) = i
                j=j+1
            endif
202      continue
!
!        BUILD THE LIST OF THE ELEMENTS OF THE DOMAIN
        call wkvect(elecal, 'V V I', neleto, jeleca)
        j=1
        do 203 i = 1, nbma
            if (zl(jelcal-1+i)) then
                zi(jeleca-1+j) = i
                j=j+1
            endif
203      continue
!
!        CREATE THE LIGREL
        call exlim1(zi(jeleca), neleto, model, 'V', liggrd)
!
        call wkvect(fiss//'.PRO.NOEUD_TORE', 'G V L', nnodgr, jnocal)
        do 204 i = 1, nnodgr
            zl(jnocal-1+i) = zl(jlisno-1+i)
204      continue
!
        call jedetr('&&XPRTOR.LISTELE')
        call jedetr('&&XPRTOR.LISTNOD')
!
    else
!
!        RETREIVE THE DISTANCES OF THE NODES FROM THE FRONT
        call jeveuo(disfr, 'L', jdisfr)
!
!        RETRIEVE THE DISTANCE VECTOR AS WELL
        call jeveuo(cnsdis//'.CNSV', 'L', jdisv)
!
!        RETRIEVE THE LEVEL SETS
        call jeveuo(cnsln//'.CNSV', 'E', jlsn)
        call jeveuo(cnslt//'.CNSV', 'E', jlst)
!
!        RETRIEVE THE LOCAL BASE FOR EACH NODE OF THE GRID
        call jeveuo(cnsbl//'.CNSV', 'L', jbl)
!
!        CREATE THE BOOLEAN LIST TO MARK THE ELEMENTS THAT ARE INSIDE
!        THE TORUS
        listel='&&XPRTOR.LISTELE'
        call wkvect(listel, 'V V L', nbma, jelcal)
!
        do 400 i = 1, nbma
            zl(jelcal-1+i) = .false.
400      continue
!
!        COUNTER FOR THE NUMBER OF NODES IN THE NEW TORUS
        nnodto=0
!
!        COUNTER FOR THE NUMBER OF ELEMENTS IN THE NEW TORUS
        neleto=0
!
!        ***********************************************************
!        SELECT THE NODES INSIDE THE NEW TORUS
!        ***********************************************************
!
!        CREATE A COPY OF THE LOGICAL LIST OF THE NODES INSIDE THE
!        TORUS OF THE ACTUAL CRACK
        lisold='&&OP0010.TORE'
        call jeexin(fispre//'.PRO.NOEUD_TORE', ibid)
        if (ibid .eq. 0) then
!           THE DOMAIN LOCALISATION WAS NOT USED IN THE PREVIOUS
!           PROPAGATION STEP. ALL THE NODES WERE CONSIDERED IN THE
!           CALCULATION. A LIST WITH ALL THE NODES TO .TRUE. IS THEN
!           CREATED.
            call wkvect(lisold, 'G V L', nnodgr, jlisol)
!
            do 302 i = 1, nnodgr
                zl(jlisol-1+i) = .true.
302          continue
        else
!           THE DOMAIN LOCALISATION WAS USED IN THE PREVIOUS PROPAGATION
!           STEP. WE JUST NEED TO COPY IT.
            call jedupo(fispre//'.PRO.NOEUD_TORE', 'V', lisold, .false.)
            call jeveuo(lisold, 'L', jlisol)
        endif
!
!        CREATE THE LOGICAL LIST FOR THE TORUS OF THE PROPAGATED CRACK
        call wkvect(fiss//'.PRO.NOEUD_TORE', 'G V L', nnodgr, jlisno)
!
!        RESET THE LIST FOR THE NEW TORUS
        do 500 i = 1, nnodgr
            zl(jlisno-1+i) = .false.
500      continue
!
!        VARIABLE USED FOR THE COMPUTATION OF THE EFFECTIVE RADIUS OF
!        THE TORUS
        rdnew = radtor
!
!        ELABORATE EACH NODE OF THE GRID
        do 1000 i = 1, nnodgr
!
            if (zr(jdisfr-1+i) .le. radtor) then
!
!              RETRIEVE THE ELEMENTS CONTAINING THE NODE
                call jelira(jexnum(cnxinv, i), 'LONMAX', nbelno)
                call jeveuo(jexnum(cnxinv, i), 'L', jnoel)
!
!              ALL THE NODES OF THE ELEMENTS CONTAINING THE SELECTED
!              NODE MUST BE INCLUDED IN THE TORUS IN ORDER TO CORRECTLY
!              DEFINE THE TORUS
                do 1500 j = 1, nbelno
!
                    numelm=zi(jnoel-1+j)
!
!                 WORK ONLY WITH THE ELEMENTS OF THE SAME DIMENSION OF
!                 THE MODEL
                    itypma=zi(jmai-1+numelm)
                    eldim=zi(jtmdim-1+itypma)
!
                    if (eldim .eq. ndim) then
!
!                    RETRIEVE THE NODES DEFINING THE ELEMENT
                        call jeveuo(jexnum('&CATA.TM.NBNO', itypma), 'L', jaux)
!
                        do 1700 k = 1, zi(jaux)
!                       SELECT EACH NODE OF THE ELEMENT
                            nocur = zi(jconx1-1+zi(jconx2-1+numelm)+k- 1)
!                       UPDATE THE RADIUS OF THE TORUS
                            if (zr(jdisfr-1+nocur) .gt. rdnew) rdnew=zr( jdisfr-1+nocur)
1700                      continue
!
                    endif
!
1500              continue
!
            endif
!
1000      continue
!
!        ESTIMATE THE RADIUS OF THE TORUS THAT MUST BE IMPOSED, IF ITS
!        VALUE HAS NOT BEEN GIVEN AS INPUT
        if (radimp .lt. 0.d0) radimp = ( sqrt(radtor)+2*(sqrt(rdnew)- sqrt(radtor)) )**2
!
!        IF THE RADIUS OF THE TORUS THAT MUST BE IMPOSED IS LOWER THAN
!        THE RADIUS OF THE TORUS THAT MUST BE GUARANTEED, A FATAL ERROR
!        IS ISSUED
        if (radimp .lt. rdnew) then
!
            meserr(1) = sqrt(radimp)
            meserr(2) = sqrt(rdnew)
            call utmess('F', 'XFEM2_99', nr=2, valr=meserr)
!
        endif
!
!        UPDATE THE RADIUS OF THE TORUS
        radtor = radimp
!
!        OK. LET'S ELABORATE THE TORUS THAT MUST BE IMPOSED.
!        ELABORATE EACH NODE OF THE GRID
        do 1001 i = 1, nnodgr
!
            if (zr(jdisfr-1+i) .le. radimp) then
!
!              SET THE FLAG TO MARK THAT THE NODE MUST BE CONSIDERED
!              IN THE CALCULATIONS
                zl(jlisno-1+i) = .true.
            endif
!
1001      continue
!
!        CREATE A TEMPORARY LOGICAL LIST FOR THE NODES IN ORDER TO
!        FIND AND ELIMINATE THE NODES WHOSE SUPPORT IS NOT IN THE
!        LIST OF ELEMENTS INSIDE THE TORUS
        call wkvect('&&XPRTOR.NODSUPP', 'V V L', nnodgr, jndsup)
        call jeundf('&&XPRTOR.NODSUPP')
        call jeveuo('&&XPRTOR.NODSUPP', 'E', jndsup)
!
!        ALL THE ELEMENTS WHOSE NODES ARE INSIDE THE IMPOSED TORUS MUST
!        BE SELECTED IN ORDER TO CORRECTLY DEFINE THE DOMAIN
        do 1002 i = 1, nnodgr
!
!           ELABORATE ONLY THE SELECTED NODES
            if (zl(jlisno-1+i)) then
!
!              RETRIEVE THE ELEMENTS CONTAINING THE NODE
                call jelira(jexnum(cnxinv, i), 'LONMAX', nbelno)
                call jeveuo(jexnum(cnxinv, i), 'L', jnoel)
!
!              CHECK EACH ELEMENT CONTAINING THE NODE
                do 1502 j = 1, nbelno
!
                    numelm=zi(jnoel-1+j)
!
!                 WORK ONLY WITH THE ELEMENTS OF THE SAME DIMENSION OF
!                 THE MODEL
                    itypma=zi(jmai-1+numelm)
                    eldim=zi(jtmdim-1+itypma)
!
                    if (eldim .eq. ndim) then
!
!                    RETRIEVE THE NODES DEFINING THE ELEMENT
                        call jeveuo(jexnum('&CATA.TM.NBNO', itypma), 'L', jaux)
!
!                    RESET THE COUNTER FOR THE NUMBER OF NODES OF THE
!                    ELEMENT WHICH ARE INSIDE THE TORE
                        nodins=0
!
                        do 1702 k = 1, zi(jaux)
!
                            nocur = zi(jconx1-1+zi(jconx2-1+numelm)+k- 1)
                            if (zl(jlisno-1+nocur)) nodins=nodins+1
!
1702                      continue
!
!                    SELECT THE ELEMENT IF ALL OF ITS NODES ARE INSIDE
!                    THE TORUS
                        if (nodins .eq. zi(jaux)) then
                            zl(jndsup-1+i) = .true.
                            if (.not.zl(jelcal-1+numelm)) then
                                zl(jelcal-1+numelm) = .true.
                                neleto = neleto+1
                            endif
                        endif
!
                    endif
!
1502              continue
!
            endif
!
1002      continue
!
!        THE NODES FOR WHICH ALL THE ELEMENTS OF THE SUPPORT DO NOT
!        BELONG TO THE DOMAIN MUST BE REMOVED FROM THE LIST (UNDEFINED
!        GRADIENT!)
        do 1734 i = 1, nnodgr
            if (.not.zl(jndsup-1+i) .and. zl(jlisno-1+i)) then
                zl(jlisno-1+i)=.false.
            endif
1734      continue
!
        call jedetr('&&XPRTOR.NODSUPP')
!
!        ***********************************************************
!        CALCULATE THE NEW LEVEL SETS FOR EACH NODE IF NECESSARY
!        ***********************************************************
!
!        IF THE CHOSEN METHOD FOR CRACK PROPAGATION IS THE GEOMETRICAL
!        ONE, I NEED TO FILL IN ONLY THE JLISNO VECTOR: I DON NEED TO
!        CALCULATE THE NEW LEVEL SETS VALUES AT THE NEW POINTS ADDED
!        TO THE DOMAIN
        if (method .eq. 'GEOMETRI') then
!
!           ELABORATE EACH NODE OF THE GRID
            do 2001 i = 1, nnodgr
                if (zl(jlisno-1+i)) nnodto=nnodto+1
2001          continue
!
        else
!
!           ELABORATE EACH NODE OF THE GRID
            do 2000 i = 1, nnodgr
!
                if (zl(jlisno-1+i)) then
!
!                CHECK IF THE LEVEL SET MUST BE CALCULATED FOR THIS NODE
                    if (.not.zl(jlisol-1+i)) then
!
!                    YES, THE LEVEL SET VALUE MUST BE UPDATED
                        zr(jlsn-1+i) = 0.d0
                        zr(jlst-1+i) = 0.d0
!
!                    CALCULATE THE NORMAL AND TANGENTIAL DISTANCES AS
!                    A SCALAR PRODUCT BETWEEN THE DISTANCE VECTOR AND
!                    THE AXIS OF THE LOCAL BASE IN THE NODE
                        do 2500 j = 1, ndim
!
                            zr(jlsn-1+i)=zr(jlsn-1+i)+zr(jdisv-1+ndim*&
                            (i-1)+j)* zr(jbl-1+2*ndim*(i-1)+j)
!
                            zr(jlst-1+i)=zr(jlst-1+i)+zr(jdisv-1+ndim*&
                            (i-1)+j)* zr(jbl-1+2*ndim*(i-1)+j+ndim)
!
2500                      continue
!
                    endif
!
!                 INCREMENT THE COUNTER FOR THE NODES IN THE TORUS
                    nnodto = nnodto+1
!
                endif
!
2000          continue
!
        endif
!
!        ***********************************************************
!        - CREATE THE LIST OF THE NUMBER OF THE NODES IN THE TORUS
!        - CREATE THE LIST OF THE NUMBER OF THE ELEMENTS IN THE TORUS
!        - CREATE THE NODAL CONNECTION TABLE FOR THE TORUS
!        ***********************************************************
!
!        CREATE A VECTOR CONTAINING THE NUMBER OF THE NODES INSIDE THE
!        TORUS
        call wkvect(nodcal, 'V V I', nnodto, jnocal)
!
!        CREATE A VECTOR CONTAINING THE NUMBER OF THE ELEMENTS INSIDE
!        THE TORUS
        call wkvect(elecal, 'V V I', neleto, jeleca)
!
        if (method(1:6) .eq. 'UPWIND') then
!           CREATE THE VECTORS FOR THE NODAL CONNECTION TABLE OF THE
!           TORUS
            call wkvect(vcnt, 'V V I', 6*nnodto, jvcnt)
            call wkvect(grlrt, 'V V R', 10+6*nnodto, jgrlrt)
            jvcndt = jgrlrt+10
!
!           RETRIEVE THE NODAL CONNECTION TABLE OF THE GRID
            call jeveuo(vcn, 'L', jvcn)
            call jeveuo(grlr, 'L', jgrlr)
            jvcnd = jgrlr+10
!
!           COPY THE LOCAL BASE OF THE GRID AND THE VALUE OF THE
!            SHORTEST EDGE IN THE GRID
            do 2100 i = 1, 10
                zr(jgrlrt-1+i) = zr(jgrlr-1+i)
2100          continue
        endif
!
!        TEMPORARY POINTER
        j=1
!
        do 3000 i = 1, nnodgr
!
            if (zl(jlisno-1+i)) then
!
                ASSERT(j.le.nnodto)
!
!              STORE THE NUMBER OF THE NODE
                zi(jnocal-1+j) = i
!
                if (method(1:6) .eq. 'UPWIND') then
!                 STORE THE CONNECTION TABLE FOR THE NODE
                    do 3500 k = 1, 6
                        zi(jvcnt-1+6*(j-1)+k) = zi(jvcn-1+6*(i-1)+k)
                        zr(jvcndt-1+6*(j-1)+k) = zr(jvcnd-1+6*(i-1)+k)
3500                  continue
                endif
!
!              INCREMENT THE POINTER FOR THE ACTUAL NODE IN THE TORUS
                j = j+1
!
            endif
!
3000      continue
!
!        TEMPORARY POINTER
        j=1
!
        do 4700 i = 1, nbma
!
            if (zl(jelcal-1+i)) then
!
                ASSERT(j.le.neleto)
                zi(jeleca-1+j) = i
                j=j+1
!
            endif
!
4700      continue
!
!        ***********************************************************
!        UPDATE THE NODAL CONNECTION TABLE FOR THE TORUS
!        ***********************************************************
!
        if (method(1:6) .eq. 'UPWIND') then
!
            do 4000 i = 1, nnodto
!
                do 4500 j = 1, 6
!
!                 RETRIEVE THE Jth NEIGHBORING NODE OF THE Ith NODE OF
!                 THE TORUS
                    k = zi(jvcnt-1+6*(i-1)+j)
!
!                 IF THE NEIGHBORING NODE EXISTS, CHECK IF IT'S INSIDE
!                 THE TORUS
                    if (k .gt. 0) then
!                   IF NOT, IT MUST BE REMOVED FROM THE CONNECTION TABLE
!                   OF THE TORUS
                        if (.not.zl(jlisno-1+k)) zi(jvcnt-1+6*(i-1)+j) =0
!
                    endif
!
4500              continue
!
4000          continue
!
        endif
!
!        ***********************************************************
!        CREATE THE NEW LIGREL FOR THE TORUS IN ORDER TO SPEED UP
!        THE CALCULUS OF THE GRADIENT
!        ***********************************************************
!
!        CREATE THE LIGREL
        call exlim1(zi(jeleca), neleto, model, 'V', liggrd)
!
!        ***********************************************************
!        CALCULATE THE GRADIENTS OF THE LEVEL SETS
!        ***********************************************************
!
!        DECLARE SOME DATA STRUCTURES FOR THE EVALUATION OF THE GRADIENT
        cnoln = '&&XPRTOR.CNOLN'
        cnolt = '&&XPRTOR.CNOLT'
        celgls = '&&XPRTOR.CELGLS'
        chams = '&&XPRTOR.CHAMS'
!
!        EVALUATION OF THE GRADIENT OF THE NORMAL LEVEL SET
        call cnscno(cnsln, ' ', 'NON', 'V', cnoln,&
                    'F', ibid)
        lpain(1)='PGEOMER'
        lchin(1)=noma//'.COORDO'
        lpain(2)='PNEUTER'
        lchin(2)=cnoln
        lpaout(1)='PGNEUTR'
        lchout(1)=celgls
!
        call calcul('S', 'GRAD_NEUT_R', liggrd, 2, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
!
        call celces(celgls, 'V', chams)
        call cescns(chams, ' ', 'V', grln, ' ',&
                    ibid)
!
!        EVALUATION OF THE GRADIENT OF THE TANGENTIAL LEVEL SET
        call cnscno(cnslt, ' ', 'NON', 'V', cnolt,&
                    'F', ibid)
        lpain(1)='PGEOMER'
        lchin(1)=noma//'.COORDO'
        lpain(2)='PNEUTER'
        lchin(2)=cnolt
        lpaout(1)='PGNEUTR'
        lchout(1)=celgls
!
        call calcul('S', 'GRAD_NEUT_R', liggrd, 2, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
!
        call celces(celgls, 'V', chams)
        call cescns(chams, ' ', 'V', grlt, ' ',&
                    ibid)
!
!        DESTROY THE TEMPORARY JEVEUX OBJECTS
        call jedetr(cnoln)
        call jedetr(cnolt)
        call jedetr(celgls)
        call jedetr(chams)
        call jedetr(lisold)
        call jedetr(listel)
!
    endif
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
