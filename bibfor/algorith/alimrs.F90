subroutine alimrs(mate, ma1, ma2, moint, ndble,&
                  num, cn1, chno, cmp, icor)
    implicit none
#include "jeveux.h"
#include "asterfort/calflu.h"
#include "asterfort/crchno.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
!
    character(len=*) :: moint, cmp, chno, mate
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
!     BUT : IMMERGER UN CHAMP AUX NOEUDS DE DEPLACEMENT
!           D UNE STRUCTURE APPARTENANT A UN MAILLAGE
!           DANS UN MODELE D INTERFACE THERMIQUE APPARTENANT
!           A UN AUTRE MAILLAGE
!     IN: MATE: NOM DU MATERIAU FLUIDE
!     IN: MA1 : MAILLAGE DE LA STRUCTURE
!     IN: MA2 : MAILLAGE DU FLUIDE
!     IN: MOINT: NOM DU MODELE THERMIQUE D INTERFACE
!     IN: NDBLE: INDICATEUR DE NOEUDS DOUBLES
!     IN: NUM: NUMEDDL ASSOCIE A L INTERFACE
!     IN: CN1: CHAMP AUX NOEUDS DE DEPL_R MODAL DE LA STRUCTURE
!     IN: CHNO: NOM DU CHAMP THERMIQUE A CREER
!     IN: CMP: COMPOSANTE DE DEPL_R A PLONGER
!     IN: ICOR(1):  ADRESSE JEVEUX D UN TABLEAU DE CORRESPONDANCE
!               NOEUDS FLUIDES - NOEUDS STRUCTURES (NOEUDS SIMPLES)
!     IN: ICOR(2):  ADRESSE JEVEUX D UN TABLEAU DE CORRESPONDANCE
!               NOEUDS FLUIDES - NOEUDS STRUCTURES (NOEUDS DOUBLES)
!
!
!
!---------------------------------------------------------------------
    logical :: test
    integer :: nbvale, nbrefe, nbdesc, ibid
    integer :: ino1, nocmp, icor(2), ichnul, ndble
    real(kind=8) :: tailmi
    character(len=8) :: gd1, gd2, ma1, ma2
    character(len=8) :: k8bid, repk
    character(len=14) :: num
    character(len=19) :: chnul, cn1, cn2, pchno1, pchno2
    character(len=24) :: prchno
    integer :: iarg
! -----------------------------------------------------------------
!---------------------------------------------------------------------
! TESTS PRELIMINAIRES : NUMERO DE COMPOSANTE A TRAITER
!
!-----------------------------------------------------------------------
    integer :: iadg1, iaval1, iaval2, ieq1, ieq2, ierd, igeom1
    integer :: igeom2, inueq1, inueq2, iprn1, iprn2, ival1, ival2
    integer :: nbid, nbno1, nbno2, ncmp2, nec1, nec2, neq
!
!-----------------------------------------------------------------------
    call jemarq()
    if (cmp .eq. 'DX') nocmp=1
    if (cmp .eq. 'DY') nocmp=2
    if (cmp .eq. 'DZ') nocmp=3
!
! RECUPERATION DE LA TAILLE DE REFERENCE
!
    call getvr8(' ', 'DIST_REFE', scal=tailmi, nbret=nbid)
!
!
! ON CREE UN CHAMNO BIDON SUR L INTERFACE THERMIQUE
!
    call dismoi('F', 'NB_NO_MAILLA', ma2, 'MAILLAGE', nbno2,&
                repk, ierd)
    chnul='&&ALIMRS.CHNUL'
    call dismoi('F', 'NB_EQUA', num, 'NUME_DDL', neq,&
                k8bid, ierd)
    prchno = num//'.NUME'
    call crchno(chnul, prchno, 'TEMP_R', ma2, 'V',&
                'R', nbno2, neq)
    call jeveuo(chnul//'.VALE', 'E', ichnul)
!
    cn2=chno
    call calflu(chnul, moint, mate, num, cn2,&
                nbdesc, nbrefe, nbvale, 'X')
!
!
!
! PARCOURS DU MAILLAGE STRUCTURE
!
!
    call dismoi('F', 'NOM_GD', cn2, 'CHAM_NO', ibid,&
                gd2, ierd)
    call dismoi('F', 'NOM_GD', cn1, 'CHAM_NO', ibid,&
                gd1, ierd)
!
    call dismoi('F', 'NB_NO_MAILLA', ma1, 'MAILLAGE', nbno1,&
                repk, ierd)
    call dismoi('F', 'PROF_CHNO', cn2, 'CHAM_NO', ibid,&
                pchno2, ierd)
!
!
!
!
!
    call dismoi('F', 'PROF_CHNO', cn1, 'CHAM_NO', ibid,&
                pchno1, ierd)
!
    call jenonu(jexnom(pchno1//'.LILI', '&MAILLA'), ibid)
    call jeveuo(jexnum(pchno1//'.PRNO', ibid), 'L', iprn1)
    call jenonu(jexnom(pchno1//'.LILI', '&MAILLA'), ibid)
    call jeveuo(jexnum(pchno2//'.PRNO', ibid), 'L', iprn2)
!
    call dismoi('F', 'NB_EC', gd1, 'GRANDEUR', nec1,&
                repk, ierd)
    call dismoi('F', 'NB_EC', gd2, 'GRANDEUR', nec2,&
                repk, ierd)
!
    call jeveuo(pchno1//'.NUEQ', 'L', inueq1)
    call jeveuo(pchno2//'.NUEQ', 'L', inueq2)
!
    call jeveuo(cn1//'.VALE', 'L', iaval1)
    call jeveuo(cn2//'.VALE', 'E', iaval2)
!
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
    if (zi(icor(1)+ino1-1) .eq. 0) goto 10
!
    ncmp2= zi(iprn2-1+ (zi(icor(1)+ino1-1)-1)* (nec2+2)+2)
    if (ncmp2 .eq. 0) goto 20
!
    ival2 = zi(iprn2-1+(zi(icor(1)+ino1-1)-1)* (nec2+2)+1)
    ieq2 = zi(inueq2-1+ival2-1+1)
!
    ival1 = zi(iprn1-1+ (ino1-1)* (nec1+2)+1)
    iadg1 = iprn1 - 1 + (ino1-1)* (nec1+2) + 3
!
! EXISTENCE DE LA COMPOSANTE A PLONGER DANS LE CHAMNO
! DE DEPL_R DE LA STRUCTURE : SI ELLE EXISTE, ON LA
! RECOPIE DANS LE CHAMNO DE TEMP_R CREE SUR
! L INTERFACE
    test = exisdg(zi(iadg1),nocmp)
!
    if (test) then
!
        ieq1 = zi(inueq1-1+ival1-1+nocmp)
!
! RECOPIE DE LA VALEUR DE DX, DY
! OU DZ DANS LA CMP DE TEMP DE CN2 DU NOEUD
! GEOMETRIQUEMENT COINCIDENT
!
        zr(iaval2-1+ieq2) = zr(iaval1-1+ieq1)
        goto 20
!
    endif
!
! PRECHERCHE DE NOEUDS DOUBLES
!
20  continue
    if (ndble .eq. 1) then
!
        if (zi(icor(2)+ino1-1) .eq. 0) goto 10
!
        ncmp2= zi(iprn2-1+ (zi(icor(2)+ino1-1)-1)* (nec2+2)+2)
        if (ncmp2 .eq. 0) goto 10
!
        ival2 = zi(iprn2-1+(zi(icor(2)+ino1-1)-1)* (nec2+2)+1)
        ieq2 = zi(inueq2-1+ival2-1+1)
!
        ival1 = zi(iprn1-1+ (ino1-1)* (nec1+2)+1)
        iadg1 = iprn1 - 1 + (ino1-1)* (nec1+2) + 3
!
! EXISTENCE DE LA COMPOSANTE A PLONGER DANS LE CHAMNO
! DE DEPL_R DE LA STRUCTURE : SI ELLE EXISTE, ON LA
! RECOPIE DANS LE CHAMNO DE TEMP_R CREE SUR
! L INTERFACE
        test = exisdg(zi(iadg1),nocmp)
!
        if (test) then
!
            ieq1 = zi(inueq1-1+ival1-1+nocmp)
!
! RECOPIE DE LA VALEUR DE DX, DY
! OU DZ DANS LA CMP DE TEMP DE CN2 DU NOEUD
! GEOMETRIQUEMENT COINCIDENT
!
            zr(iaval2-1+ieq2) = zr(iaval1-1+ieq1)
            goto 10
!
        endif
    endif
!
!
    10 end do
!
! --- MENAGE
!
    call detrsd('CHAM_NO', chnul)
!
    call jedema()
end subroutine
