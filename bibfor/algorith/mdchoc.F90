subroutine mdchoc(nbnli, nbchoc, nbflam, nbsism, nbrfis,&
                  nbpal, logcho, dplmod, parcho, noecho,&
                  intitu, ps1del, ps2del, numddl, nbmode,&
                  pulsat, masgen, lamor, amogen, bmodal,&
                  neq, nexcit, info, monmot, ier)
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/gloloc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdchge.h"
#include "asterfort/mdchst.h"
#include "asterfort/mdtr74grd.h"
#include "asterfort/resmod.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: nbnli, nbchoc, nbflam, nbsism(2), nbmode, neq
    integer :: nbrfis, nbpal
    integer :: logcho(nbnli, *), ier, nexcit, info
    real(kind=8) :: parcho(nbnli, *), pulsat(*), masgen(*), amogen(*)
    real(kind=8) :: dplmod(nbnli, nbmode, *), bmodal(neq, *)
    real(kind=8) :: ps1del(neq, nexcit), ps2del(nbnli, nexcit, *)
    character(len=8) :: noecho(nbnli, *), intitu(*), monmot
    character(len=14) :: numddl
    aster_logical :: lamor
! ----------------------------------------------------------------------
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
!
!     STOCKAGE DES INFORMATIONS DE CHOC DANS DES TABLEAUX
!     ------------------------------------------------------------------
! IN  : NBNLI  : DIMENSION DES TABLEAUX (NBCHOC+NBSISM(*)+NBFLAM+NBPAL)
! IN  : NBCHOC : NOMBRE DE POINTS DE CHOC
! IN  : NBFLAM : NOMBRE DE CHOCS AVEC FLAMBEMENT
! IN  : NBPAL : NOMBRE DE PALIERS (COUPLAGE EDYOS)
! OUT : LOGCHO : LOGIQUE CHOC: LOGCHO(I,1) = SI ADHERENCE    OU NON=0
!                              LOGCHO(I,4) = SI DISPO ANTI_S OU NON=0
!                              LOGCHO(I,5) = SI FLAMBEMENT   OU NON=0
!                              LOGCHO(I,6) = SI DISC_VIS=1   OU NON=0
!
! OUT : DPLMOD : DEPL MODAUX AUX NOEUDS DE CHOC APRES ORIENTATION
!                DPLMOD(I,J,1) = DEPL DX DU NOEUD_1 DE CHOC I - MODE J
!                DPLMOD(I,J,2) = DEPL DY
!                DPLMOD(I,J,3) = DEPL DZ
!                DPLMOD(I,J,4) = DEPL DX DU NOEUD_2 DE CHOC I - MODE J
!                DPLMOD(I,J,5) = DEPL DY
!                DPLMOD(I,J,6) = DEPL DZ
! OUT : PARCHO : PARAMETRE DE CHOC:
!                PARCHO(I, 1)= JEU AU NOEUD DE CHOC I
!                PARCHO(I, 2)= RIGI NORMALE
!                PARCHO(I, 3)= AMOR NORMAL
!                PARCHO(I, 4)= RIGI TANGENTIELLE
!                PARCHO(I, 5)= AMOR TANGENTIEL
!                PARCHO(I, 6)= COULOMB_DYNA
!                PARCHO(I, 7)= COULOMB_STAT
!                PARCHO(I, 8)= COOR INIT NOEUD_1 X REP GLOBAL
!                PARCHO(I, 9)= COOR INIT NOEUD_1 Y REP GLOBAL
!                PARCHO(I,10)= COOR INIT NOEUD_1 Z REP GLOBAL
!                PARCHO(I,11)= COOR INIT NOEUD_2 X REP GLOBAL
!                PARCHO(I,12)= COOR INIT NOEUD_2 Y REP GLOBAL
!                PARCHO(I,13)= COOR INIT NOEUD_2 Z REP GLOBAL
!                PARCHO(I,14)= COOR ORIGINE OBSTACLE X REP GLOBAL
!                PARCHO(I,15)= COOR ORIGINE OBSTACLE Y REP GLOBAL
!                PARCHO(I,16)= COOR ORIGINE OBSTACLE Z REP GLOBAL
!                PARCHO(I,17)= SIN A
!                PARCHO(I,18)= COS A
!                PARCHO(I,19)= SIN B
!                PARCHO(I,20)= COS B
!                PARCHO(I,21)= SIN G
!                PARCHO(I,22)= COS G
!                PARCHO(I,23)= X AVANT ADHERENCE
!                PARCHO(I,24)= Y AVANT ADHERENCE
!                PARCHO(I,25)= Z AVANT ADHERENCE
!                PARCHO(I,26)= FT1 AVANT ADHERENCE
!                PARCHO(I,27)= FT2 AVANT ADHERENCE
!                PARCHO(I,28)= VT1 PAS PRECEDENT
!                PARCHO(I,29)= VT2 PAS PRECEDENT
!                PARCHO(I,30)= DIST_1 DU NOEUD_1
!                PARCHO(I,31)= DIST_2 DU NOEUD_2
!                PARCHO(I,32)= COEF A FORCE FLUIDE
!                PARCHO(I,33)= COEF B FORCE FLUIDE
!                PARCHO(I,34)= COEF C FORCE FLUIDE
!                PARCHO(I,35)= COEF D FORCE FLUIDE
!                PARCHO(I,36)= COUCHE LIMITE
!                PARCHO(I,37)= SIGNE DE Y20LOC-Y10LOC
!                PARCHO(I,38)= SIGNE DE Z20LOC-Z10LOC
!                PARCHO(I,39)= COEF RIGI_K1 DISPO ANTI SISMIQUE
!                PARCHO(I,40)= COEF RIGI_K2 DISPO ANTI SISMIQUE
!                PARCHO(I,41)= COEF SEUIL_FX DISPO ANTI SISMIQUE
!                PARCHO(I,42)= COEF C DISPO ANTI SISMIQUE
!                PARCHO(I,43)= COEF PUIS_ALPHA DISPO ANTI SISMIQUE
!                PARCHO(I,44)= COEF DX_MAX DISPO ANTI SISMIQUE
!                PARCHO(I,45)= NORMALE X
!                PARCHO(I,46)= NORMALE Y
!                PARCHO(I,47)= NORMALE Z
!                PARCHO(I,48)= TAUX DE RESTITUTION (CALCULE DANS CRICHO)
!                PARCHO(I,49)= TAUX DE RESTITUTION (CALCULE DANS CRICHO)
!                PARCHO(I,50)= FORCE LIMITE DE FLAMBAGE
!                PARCHO(I,51)= PALIER FORCE DE REACTION APRES FLAMBAGE
!                PARCHO(I,52)= RIGIDITE APRES FLAMBAGE
!   DISC_VIS
!                PARCHO(I,53)   = S1 Souplesse en série avec les 2 autres branches.
!                PARCHO(I,54)   = K2 Raideur en parallèle de la branche visqueuse.
!                PARCHO(I,55)   = S3 Souplesse dans la branche visqueuse.
!                PARCHO(I,56)   = C  'Raideur' de la partie visqueuse.
!                PARCHO(I,57)   = A  Puissance de la loi visqueuse ]0.0, 1.0]
!                PARCHO(I,58)   = ITER_INTE_MAXI
!                PARCHO(I,59)   = RESI_INTE_RELA
!                PARCHO(I,60:63)= Variables internes
!
! OUT : NOECHO : NOEUD DE CHOC: NOECHO(I,1) = NOEUD_1
!                               NOECHO(I,2) = SOUS_STRUC_1
!                               NOECHO(I,3) = NUME_1
!                               NOECHO(I,4) = MAILLA_1
!                               NOECHO(I,5) = NOEUD_2
!                               NOECHO(I,6) = SOUS_STRUC_2
!                               NOECHO(I,7) = NUME_2
!                               NOECHO(I,8) = MAILLA_2
!                               NOECHO(I,9) = TYPE D'OBSTACLE
! OUT : INTITU : INTITULE DE CHOC
! IN  : PS1DEL : PSI*DELTA (MULTI-APPUI) = NOMRES//'.IPSD'
! OUT : PS2DEL : PSI*DELTA: PS2DEL(I,J,1)=  DX NOEUD_1 CHOC I - EXCIT J
!                           PS2DEL(I,J,2)=  DY NOEUD_1 CHOC I - EXCIT J
!                           PS2DEL(I,J,3)=  DZ NOEUD_1 CHOC I - EXCIT J
!                           PS2DEL(I,J,4)=  DX NOEUD_2 CHOC I - EXCIT J
!                           PS2DEL(I,J,5)=  DY NOEUD_2 CHOC I - EXCIT J
!                           PS2DEL(I,J,6)=  DZ NOEUD_2 CHOC I - EXCIT J
! IN  : NUMDDL : NOM DE LA NUMEROTATION
! IN  : NBMODE : NOMBRE DE MODES DE LA BASE DE PROJECTION
! IN  : PULSAT : PULSATIONS DES MODES
! IN  : MASGEN : MASSES GENERALISEES DES MODES
! IN  : LAMOR  : LOGIQUE POUR AMORTISSEMENTS MODAUX
! IN  : AMOGEN : MATRICE DES AMORTISSEMENTS GENERALISES
! IN  : BMODAL : VECTEURS MODAUX
! IN  : NEQ    : NOMBRE D'EQUATIONS
! IN  : NEXCIT : NOMBRE D'EXCITATIONS
! IN  : INFO   : NIVEAU D'IMPRESSION
! OUT : IER    : CODE RETOUR
! ----------------------------------------------------------------------
!
    integer :: imode, iamor, im, i, j, nbparcho, nblogcho
    integer :: vali
    real(kind=8) :: dpiloc(6), dpiglo(6), ddpilo(3), origob(3), un
    real(kind=8) :: valr(10)
    real(kind=8) :: sina, cosa, sinb, cosb, sing, cosg, xjeu, xmas, ctang
    character(len=8) :: noeud(3)
    character(len=16) :: typnum
    character(len=24) :: mdgene, numero
    character(len=24) :: valk
    integer, pointer :: ddlcho(:) => null()
    real(kind=8), pointer :: dplcho(:) => null()
    character(len=24), pointer :: refn(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
    ier = 0
    un = 1.d0
    numero = ' '
    mdgene = ' '
    call gettco(numddl, typnum)
    if (typnum(1:13) .eq. 'NUME_DDL_GENE') then
        if (nbsism(1)+nbsism(2)+nbflam .gt. 0) then
            call utmess('F', 'ALGORITH5_36')
        endif
    endif
!
! --- RECHERCHE DU MODE DE MASSE LA PLUS ELEVEE ---
!
    xmas = masgen(1)
    imode = 1
    do im = 2, nbmode
        if (masgen(im) .gt. xmas) then
            xmas = masgen(im)
            imode = im
        endif
    enddo
    if (lamor) then
        iamor = imode
    else
        iamor = imode + nbmode*( imode - 1 )
    endif
!
    nbparcho = mdtr74grd('PARCHO')
    nblogcho = mdtr74grd('LOGCHO')
    do i = 1, nbnli
        logcho(i,1:nblogcho) = 0
        noecho(i,1:9) = ' '
        parcho(i,1:nbparcho) = 0.d0
    enddo
!
    AS_ALLOCATE(vi=ddlcho, size=nbnli*6)
!
!   CALCUL DIRECT
    if (typnum .eq. 'NUME_DDL_SDASTER') then
        call mdchst(numddl, typnum, imode, iamor, pulsat,&
                    masgen, amogen, nbnli, nbpal, noecho,&
                    nbrfis, logcho, parcho, intitu, ddlcho,&
                    ier)
!
!   CALCUL PAR SOUS-STRUCTURATION
    else if (typnum(1:13).eq.'NUME_DDL_GENE') then
        call mdchge(numddl, typnum, imode, iamor, pulsat,&
                    masgen, amogen, nbnli, noecho, parcho,&
                    intitu, ddlcho, ier)
    endif
!
    do i = 1, nbnli - nbpal
        ctang = parcho(i,5)
        origob(1) = parcho(i,14)
        origob(2) = parcho(i,15)
        origob(3) = parcho(i,16)
        sina = parcho(i,17)
        cosa = parcho(i,18)
        sinb = parcho(i,19)
        cosb = parcho(i,20)
        sing = parcho(i,21)
        cosg = parcho(i,22)
!
        if (info .eq. 2) then
            vali = i
            valk = noecho(i,1)
            call utmess('I', 'ALGORITH16_2', sk=valk, si=vali)
            if (typnum(1:13) .eq. 'NUME_DDL_GENE') then
                valk = noecho(i,2)
                call utmess('I', 'ALGORITH16_3', sk=valk)
            endif
            valr (1) = parcho(i,8)
            valr (2) = parcho(i,9)
            valr (3) = parcho(i,10)
            call utmess('I', 'ALGORITH16_4', nr=3, valr=valr)
            if (noecho(i,9)(1:2) .eq. 'BI') then
                valk = noecho(i,5)
                call utmess('I', 'ALGORITH16_5', sk=valk)
                if (typnum(1:13) .eq. 'NUME_DDL_GENE') then
                    valk = noecho(i,6)
                    call utmess('I', 'ALGORITH16_3', sk=valk)
                endif
                valr (1) = parcho(i,11)
                valr (2) = parcho(i,12)
                valr (3) = parcho(i,13)
                call utmess('I', 'ALGORITH16_4', nr=3, valr=valr)
            endif
            valr (1) = ctang
            valr (2) = parcho(i,14)
            valr (3) = parcho(i,15)
            valr (4) = parcho(i,16)
            valr (5) = parcho(i,17)
            valr (6) = parcho(i,18)
            valr (7) = parcho(i,19)
            valr (8) = parcho(i,20)
            valr (9) = parcho(i,21)
            valr (10)= parcho(i,22)
            call utmess('I', 'ALGORITH16_8', nr=10, valr=valr)
            if (noecho(i,9)(1:2) .eq. 'BI') then
                xjeu = (&
                       parcho(i,11) - parcho(i,8))**2 + (parcho(i,12) - parcho(i,9))**2 + (parcho&
                       &(i,13) - parcho(i,10)&
                       )**2
                if (i .le. nbchoc) then
                    xjeu = sqrt(xjeu) - (parcho(i,30)+parcho(i,31))
                else
                    xjeu = sqrt(xjeu)
                endif
                valr (1) = xjeu
                call utmess('I', 'ALGORITH16_9', sr=valr(1))
            endif
            call utmess('I', 'VIDE_1')
        endif
!
!       POSITION INITIALE DU NOEUD 1 DANS LE REPERE GLOBAL
        dpiglo(1) = parcho(i,8)
        dpiglo(2) = parcho(i,9)
        dpiglo(3) = parcho(i,10)
!       --- PASSAGE DANS LE REPERE LOCAL --- POUR LE NOEUD 1
        call gloloc(dpiglo, origob, sina, cosa, sinb,&
                    cosb, sing, cosg, dpiloc)
!       POSITON INITIALE DIFFERENTIELLE = DPILOC SI 1 NOEUD
        ddpilo(1) = dpiloc(1)
        ddpilo(2) = dpiloc(2)
        ddpilo(3) = dpiloc(3)
!
        if (noecho(i,9)(1:2) .eq. 'BI') then
!          POSITION INITIALE DU NOEUD 2 DANS LE REPERE GLOBAL
            dpiglo(4) = parcho(i,11)
            dpiglo(5) = parcho(i,12)
            dpiglo(6) = parcho(i,13)
!          --- PASSAGE DANS LE REPERE LOCAL --- POUR LE NOEUD 2
            call gloloc(dpiglo(4), origob, sina, cosa, sinb,&
                        cosb, sing, cosg, dpiloc(4))
!          POSITION INITIALE DU NOEUD1 PAR RAPPORT AU NOEUD2
            ddpilo(1) = dpiloc(1)-dpiloc(4)
            ddpilo(2) = dpiloc(2)-dpiloc(5)
            ddpilo(3) = dpiloc(3)-dpiloc(6)
        endif
        parcho(i,37)= -sign(un,ddpilo(2))
        parcho(i,38)= -sign(un,ddpilo(3))
!
    enddo
!
! --- REMPLISSAGE DE DPLMOD(I,J,K) ---
!
    if (typnum .eq. 'NUME_DDL_SDASTER') then
!         ----------------------------
        do i = 1, nbnli - nbpal
            do j = 1, nbmode
                dplmod(i,j,1) = bmodal(ddlcho(6*(i-1)+1),j)
                dplmod(i,j,2) = bmodal(ddlcho(6*(i-1)+2),j)
                dplmod(i,j,3) = bmodal(ddlcho(6*(i-1)+3),j)
                if (noecho(i,9)(1:2) .eq. 'BI') then
                    dplmod(i,j,4) = bmodal(ddlcho(6*(i-1)+4),j)
                    dplmod(i,j,5) = bmodal(ddlcho(6*(i-1)+5),j)
                    dplmod(i,j,6) = bmodal(ddlcho(6*(i-1)+6),j)
                else
                    dplmod(i,j,4) = 0.d0
                    dplmod(i,j,5) = 0.d0
                    dplmod(i,j,6) = 0.d0
                endif
            enddo
        enddo
!       COUPLAGE AVEC EDYOS
        if (nbpal .gt. 0) then
            do i = nbnli - nbpal +1, nbnli
                do j = 1, nbmode
                    dplmod(i,j,1) = bmodal(ddlcho(6*(i-1)+1),j)
                    dplmod(i,j,2) = bmodal(ddlcho(6*(i-1)+2),j)
                    dplmod(i,j,3) = bmodal(ddlcho(6*(i-1)+3),j)
                    if (noecho(i,9)(1:2) .eq. 'BI') then
                        dplmod(i,j,4) = bmodal(ddlcho(6*(i-1)+4),j)
                        dplmod(i,j,5) = bmodal(ddlcho(6*(i-1)+5),j)
                        dplmod(i,j,6) = bmodal(ddlcho(6*(i-1)+6),j)
                    else
                        dplmod(i,j,4) = 0.d0
                        dplmod(i,j,5) = 0.d0
                        dplmod(i,j,6) = 0.d0
                    endif
                enddo
            enddo
        endif
!       FIN COUPLAGE AVEC EDYOS
!
!       ROTOR FISSURE
        if (nbrfis .gt. 0) then
            do i = nbnli - nbrfis + 1, nbnli
                do j = 1, nbmode
                    dplmod(i,j,1) = bmodal(ddlcho(6*(i-1)+1),j)
                    dplmod(i,j,2) = bmodal(ddlcho(6*(i-1)+2),j)
                    dplmod(i,j,3) = bmodal(ddlcho(6*(i-1)+3),j)
                    dplmod(i,j,4) = bmodal(ddlcho(6*(i-1)+4),j)
                    dplmod(i,j,5) = bmodal(ddlcho(6*(i-1)+5),j)
                    dplmod(i,j,6) = bmodal(ddlcho(6*(i-1)+6),j)
                enddo
            enddo
        endif
!       FIN ROTOR FISSURE
!
    else if (typnum(1:13).eq.'NUME_DDL_GENE') then
!             -------------------------------
        numero(1:14) = numddl
        call jeveuo(numddl//'.NUME.REFN', 'L', vk24=refn)
        mdgene = refn(1)
        do i = 1, nbnli - nbpal
            AS_ALLOCATE(vr=dplcho, size=nbmode*6)
            noeud(1) = noecho(i,1)
            noeud(2) = noecho(i,2)
            noeud(3) = noecho(i,3)
            call resmod(bmodal, nbmode, neq, numero, mdgene,&
                        noeud, dplcho)
            do j = 1, nbmode
                dplmod(i,j,1) = dplcho(j)
                dplmod(i,j,2) = dplcho(j+nbmode)
                dplmod(i,j,3) = dplcho(j+2*nbmode)
            enddo
            if (noecho(i,9)(1:2) .eq. 'BI') then
                noeud(1) = noecho(i,5)
                noeud(2) = noecho(i,6)
                noeud(3) = noecho(i,7)
                call resmod(bmodal, nbmode, neq, numero, mdgene,&
                            noeud, dplcho)
                do j = 1, nbmode
                    dplmod(i,j,4) = dplcho(j)
                    dplmod(i,j,5) = dplcho(j+nbmode)
                    dplmod(i,j,6) = dplcho(j+2*nbmode)
                enddo
            else
                do j = 1, nbmode
                    dplmod(i,j,4) = 0.d0
                    dplmod(i,j,5) = 0.d0
                    dplmod(i,j,6) = 0.d0
                enddo
            endif
            AS_DEALLOCATE(vr=dplcho)
        enddo
    endif
!
! --- REMPLISSAGE DE PS2DEL(I,J,K) ---
!
    if (monmot(1:3) .eq. 'OUI') then
        if (typnum .eq. 'NUME_DDL_SDASTER') then
            do i = 1, nbnli - nbpal
                do j = 1, nexcit
                    ps2del(i,j,1) = ps1del(ddlcho(6*(i-1)+1),j)
                    ps2del(i,j,2) = ps1del(ddlcho(6*(i-1)+2),j)
                    ps2del(i,j,3) = ps1del(ddlcho(6*(i-1)+3),j)
                    if (noecho(i,9)(1:2) .eq. 'BI') then
                        ps2del(i,j,4) = ps1del(ddlcho(6*(i-1)+4),j)
                        ps2del(i,j,5) = ps1del(ddlcho(6*(i-1)+5),j)
                        ps2del(i,j,6) = ps1del(ddlcho(6*(i-1)+6),j)
                    else
                        ps2del(i,j,4) = 0.d0
                        ps2del(i,j,5) = 0.d0
                        ps2del(i,j,6) = 0.d0
                    endif
                enddo
            enddo
        else if (typnum(1:13).eq.'NUME_DDL_GENE') then
            ier = ier + 1
            call utmess('E', 'ALGORITH5_37')
        endif
    endif
!
! --- VERIFICATION DE COHERENCE ENTRE CHOC ET FLAMBAGE ---
!
    if (nbchoc .ne. 0 .and. nbflam .ne. 0) then
        do i = 1, nbchoc
            j = nbchoc+nbsism(1)+nbsism(2)
130         continue
            j = j + 1
            if (j .le. nbnli - nbpal) then
                if (noecho(i,1) .ne. noecho(j,1)) goto 130
                if (noecho(i,5) .ne. noecho(j,5)) goto 130
                call utmess('A', 'ALGORITH5_38')
                parcho(i,2) = 0.d0
                parcho(i,4) = 0.d0
            endif
        enddo
    endif
!
    AS_DEALLOCATE(vi=ddlcho)
!
    call jedema()
end subroutine
