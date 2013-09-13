subroutine xpoajc(nnm, inm, inmtot, nbmac, ise,&
                  npg, jcesd1, jcesd2, jcvid1, jcvid2,&
                  ima, ndim, ndime, iadc, iadv,&
                  jcesv1, jcesl2, jcesv2, jcviv1, jcvil2,&
                  jcviv2)
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
    integer :: nnm, inm, inmtot, nbmac, ise, ndime, npg
    integer :: jcesd1, jcesd2, ima, ndim, iadc, jcesv1, jcesl2, jcesv2
    integer :: jcvid1, jcvid2, jcviv1, jcvil2, jcviv2, idcalv, iadv
! ----------------------------------------------------------------------
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
!   ON AJOUTE UN CHAMP DE CONTRAINTES AU NOUVEAU RESU X-FEM
!
!   IN
!     NNM    : NOMBRE DE NOUVELLES MAILLES A CREER SUR LA MAILLE PARENT
!     NBMAC  : NOMBRE DE MAILLES CLASSIQUES DU MAILLAGE FISSURE
!     ISE    : COMPTEUR DE SOUS ELEMENT
!     IMA    : NUMÉRO DE LA MAILLE PARENT
!     NDIM   : DIMENSION DU MAILLAGE
!     NDIME  : DIMENSION TOPOLOGIQUE DE LA MAILLE
!     IADC   : DECALAGE DUE A IMA DANS LE CHAMP DE CONTRAINTES 1
!     JCESV1 : ADRESSE DU .CESV DU CHAM_ELEM_S DE CONTRAINTES ENTREE
!
!   OUT
!     INM    : COMPTEUR LOCAL DU NOMBRE DE NOUVELLES MAILLES CREEES
!     INMTOT : COMPTEUR TOTAL DU NOMBRE DE NOUVELLES MAILLES CREEES
!     JCESL2 : ADRESSE DU .CESL DU CHAM_ELEM_S DE CONTRAINTES SORTIE
!     JCESV2 : ADRESSE DU .CESV DU CHAM_ELEM_S DE CONTRAINTES SORTIE
!
!
!
!
!
    integer :: idecal
    integer :: ncmp1, ncmp2, npg1, npg2, ipg, icmp, iad2
    integer :: ncmv1, ncmv2, npgv2, ipt
!
    real(kind=8) :: val
!
    character(len=8) :: valk(2)
!
    data          valk /'MAILLES','XPOAJM'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    if (inmtot .ge. 999999) then
        call utmess('F', 'XFEM_8', sk=valk(1))
    endif
!
    inm = inm + 1
    inmtot = inmtot + 1
    ASSERT(inm.le.nnm)
!
    npg1 = npg
    ncmp1 = zi(jcesd1-1+5+4* (ima-1)+3)
    npg2 = zi(jcesd2-1+5+4* (nbmac +inmtot-1)+1)
!
!     PAS DE CONTRAINTES POUR LES ELEMENTS DE BORD
    if (ndime .ne. ndim) then
        ASSERT(npg2.eq.0)
        goto 999
    endif
!
    ASSERT(npg2.eq.1 .or. npg2.eq.3)
!
    ncmp2 = zi(jcesd2-1+5+4* (nbmac +inmtot-1)+3)
!
    ASSERT(ncmp1.eq.ncmp2)
!
    if ((jcvid1 .ne. 0) .and. (jcvid2 .ne. 0)) then
        ncmv1 = zi(jcvid1-1+5+4* (ima-1)+3)
        npgv2 = zi(jcvid2-1+5+4* (nbmac +inmtot-1)+1)
        ncmv2 = zi(jcvid2-1+5+4* (nbmac +inmtot-1)+3)
!
        ASSERT(npgv2.eq.1 .or.npgv2.eq.3)
        ASSERT(npg2.eq.npgv2)
        ASSERT(ncmv1.le.ncmv2)
    else
        ncmv1 = 0
        ncmv2 = 0
        npgv2 = 0
    endif
!
!     DECALAGE DANS LE CESV DU CHAMP DE CONTRAINTES 1 DU
!     AU FAIT QUE L'ON EST SUR LE SOUS-TETRA ISE
!     COMME DANS XMEL3D
!     CE DECALAGE NE PEUT PAS ETRE DONNE PAR CESEXI !!
    idecal = npg1*(ise-1)*ncmp1
    idcalv = npg1*(ise-1)*ncmv1
!
    do 30 icmp = 1, ncmp1
!       VAL : MOYENNE SUR LES POINTS DE GAUSS DU CHAMP 1
        val=0.d0
        do 20 ipg = 1, npg1
            val =val + zr(jcesv1-1+iadc-1+idecal+ncmp1*(ipg-1)+icmp)
20      continue
        val = val/npg1
        do 35 ipt = 1, npg2
            call cesexi('C', jcesd2, jcesl2, nbmac +inmtot, ipt,&
                        1, icmp, iad2)
            ASSERT(iad2.gt.0)
            zl(jcesl2-1+iad2) = .true.
            zr(jcesv2-1+iad2) = val
35      continue
30  end do
!
    if (ncmv1 .ne. 0) then
        do 50 icmp = 1, ncmv1
!         VAL : MOYENNE SUR LES POINTS DE GAUSS DU CHAMP 1
            val=0.d0
            do 40 ipg = 1, npg1
                val =val + zr(jcviv1-1+iadv-1+idcalv+ncmv1*(ipg-1)+&
                icmp)
40          continue
            val = val/npg1
            do 45 ipt = 1, npg2
                call cesexi('C', jcvid2, jcvil2, nbmac +inmtot, ipt,&
                            1, icmp, iad2)
                ASSERT(iad2.lt.0)
                iad2 = -iad2
                zl(jcvil2-1+iad2) = .true.
                zr(jcviv2-1+iad2) = val
45          continue
50      continue
    endif
!
999  continue
    call jedema()
end subroutine
