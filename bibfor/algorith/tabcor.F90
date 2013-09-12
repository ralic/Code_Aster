subroutine tabcor(model, mate, ma1, ma2, moint,&
                  num, ndble, icor)
    implicit none
#include "jeveux.h"
#include "asterfort/calflu.h"
#include "asterfort/crchno.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/jecreo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
!
    character(len=*) :: moint, mate
!---------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!---------------------------------------------------------------------
!     AUTEUR : G. ROUSSEAU
!     BUT : CREER UNE TABLE DE CORRESPONDANCE ENTRE NOEUDS FLUIDES
!           D'INTERFACE ET NOEUDS DE STRUCTURES POUR DES MAILLAGES
!           STRUCTURES ET FLUIDE DISTINCTS
!     IN: MATE: NOM DU MATERIAU FLUIDE
!     IN: MA1 : MAILLAGE DE LA STRUCTURE
!     IN: MA2 : MAILLAGE DU FLUIDE
!     IN: MOINT: NOM DU MODELE THERMIQUE D INTERFACE
!     IN: NUM: NUMEDDL ASSOCIE A L INTERFACE
!     IN: CMP: COMPOSANTE DE DEPL_R A PLONGER
!     IN: NDBLE: INDICATEUR DE NOEUDS DOUBLES
!     OUT:ICOR(2):ADRESSES JEVEUX DES TABLEAUX DE CORRESPONDANCES
!              NOEUDS SIMPLES
!
!
!---------------------------------------------------------------------
    integer :: nbvale, nbrefe, nbdesc, ibid, neq, nbno1
    integer :: ino1, ino2, icor(2), itb1, itb2, ncmp2, nbno2, ichnul
    integer :: nec2, igeom1, igeom2, ierd, iprn2, inueq2, nbid
    integer :: ndble, nbptr
    real(kind=8) :: epsi, x1, y1, z1, x2, y2, z2
    real(kind=8) :: tailmi, dista2, tailm2
    character(len=2) :: model
    character(len=8) :: gd2, ma1, ma2
    character(len=8) :: k8bid, repk
    character(len=14) :: num
    character(len=19) :: chnul, cn2, pchno2
    character(len=24) :: prchno
    integer :: iarg
! -----------------------------------------------------------------
    data epsi    /1.d-2/
!---------------------------------------------------------------------
!
! RECUPERATION DE LA TAILLE DE REFERENCE
!
    call getvr8(' ', 'DIST_REFE', scal=tailmi, nbret=nbid)
    call dismoi('F', 'NB_NO_MAILLA', ma2, 'MAILLAGE', nbno2,&
                repk, ierd)
    tailm2=(epsi*tailmi)**2
! ON CREE UN CHAMNO BIDON SUR L INTERFACE THERMIQUE
!
    chnul='&&TABCOR.CHNUL'
    call dismoi('F', 'NB_EQUA', num, 'NUME_DDL', neq,&
                k8bid, ierd)
    prchno = num//'.NUME'
    call crchno(chnul, prchno, 'TEMP_R', ma2, 'V',&
                'R', nbno2, neq)
    call jeveuo(chnul//'.VALE', 'E', ichnul)
!
!
    cn2='&&TABCOR.BIDON'
    call calflu(chnul, moint, mate, num, cn2,&
                nbdesc, nbrefe, nbvale, 'X')
!
!
!
! PARCOURS DU MAILLAGE THERMIQUE D INTERFACE
!
!
    call dismoi('F', 'NOM_GD', cn2, 'CHAM_NO', ibid,&
                gd2, ierd)
!
    call dismoi('F', 'NB_NO_MAILLA', ma1, 'MAILLAGE', nbno1,&
                repk, ierd)
!
! CREATION D'UNE TABLE DE CORRESPONDANCES NOEUDS DE STRUCTURE
! AVEC NOEUDS DU FLUIDE DE L'INTERFACE (NOEUDS SIMPLES)
!
    call jecreo('&&TABCOR.CORRE1', 'V V I')
    call jeecra('&&TABCOR.CORRE1', 'LONMAX', nbno1)
    call jeecra('&&TABCOR.CORRE1', 'LONUTI', nbno1)
    call jeveut('&&TABCOR.CORRE1', 'E', itb1)
    icor(1)=itb1
!
! SI IL Y A DES NOEUDS DOUBLES
!
    if (ndble .eq. 1) then
        call jecreo('&&TABCOR.CORRE2', 'V V I')
        call jeecra('&&TABCOR.CORRE2', 'LONMAX', nbno1)
        call jeecra('&&TABCOR.CORRE2', 'LONUTI', nbno1)
        call jeveut('&&TABCOR.CORRE2', 'E', itb2)
        icor(2)=itb2
    endif
!
!
!
!      DO 2 I=1,NBRF
!
!2     CONTINUE
!
!
! RECUPERATION DES CARACTERISTIQUES DU CHAMP AUX NOEUDS DE
! L'INTERFACE
!
    call dismoi('F', 'PROF_CHNO', cn2, 'CHAM_NO', ibid,&
                pchno2, ierd)
    call jenonu(jexnom(pchno2//'.LILI', '&MAILLA'), ibid)
    call jeveuo(jexnum(pchno2//'.PRNO', ibid), 'L', iprn2)
    call dismoi('F', 'NB_EC', gd2, 'GRANDEUR', nec2,&
                repk, ierd)
    call jeveuo(pchno2//'.NUEQ', 'L', inueq2)
!
! RECUPERATION DES COORDONNEES DU MAILLAGE
!
    call jeveuo(ma2//'.COORDO    .VALE', 'L', igeom2)
    call jeveuo(ma1//'.COORDO    .VALE', 'L', igeom1)
!
! PARCOURS SUR LES NOEUDS DU MAILLAGE STRUCTURE
! ON REPERE LES NOEUDS COINCIDENTS GEOMETRIQUEMENT
! AVEC LES NOEUDS DE L INTERFACE FLUIDE
!
    do 10, ino1 =1,nbno1
!
    x1 = zr(igeom1 -1 + (ino1 -1)*3 +1)
    y1 = zr(igeom1 -1 + (ino1 -1)*3 +2)
    if (model .eq. '3D') then
        z1 = zr(igeom1 -1 + (ino1 -1)*3 +3)
    endif
!
! PARCOURS SUR LES NOEUDS DU MAILLAGE FLUIDE
! ON REPERE LES NOEUDS COINCIDENTS GEOMETRIQUEMENT
! AVEC LES NOEUDS DE LA STRUCTURE ET ON RECOPIE
! LE NOEUD COINCIDENT DS LE TABLEAU DE CORRESPONDANCE
!
    nbptr=0
    do 20, ino2 =1,nbno2
!
    ncmp2= zi(iprn2-1+ (ino2-1)* (nec2+2)+2)
    if (ncmp2 .eq. 0) goto 20
!
! CRITERE GEOMETRIQUE DE PROXIMITE
!
    x2 = zr(igeom2 -1 + (ino2 -1)*3 +1)
    y2 = zr(igeom2 -1 + (ino2 -1)*3 +2)
    if (model .eq. '3D') then
        z2 = zr(igeom2 -1 + (ino2 -1)*3 +3)
        dista2 = (x1-x2)**2 + (y1-y2)**2 + (z1-z2)**2
    else
        dista2 = (x1-x2)**2 + (y1-y2)**2
    endif
!
    if (dista2 .lt. (tailm2)) then
!
!
        nbptr=nbptr+1
        if (nbptr .eq. 1) then
            zi(itb1+ino1-1)=ino2
            if (ndble .eq. 0) goto 10
        else
            zi(itb2+ino1-1)=ino2
            goto 10
        endif
!
! PAS DE RECHERCHE DE NOEUDS DOUBLES
!
!                 GOTO 10
    endif
!
20  continue
!
    10 end do
!
!
!      IF (NDBLE.EQ.1) THEN
!        DO 30 I=1,NBNO1
!
!30      CONTINUE
!      ELSE
!        DO 40 I=1,NBNO1
!
!40      CONTINUE
!      ENDIF
!
! --- MENAGE
    call detrsd('CHAM_NO', chnul)
    call detrsd('CHAM_NO', cn2)
!
end subroutine
