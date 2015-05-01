subroutine nmimre(numedd, sdimpr, sdconv, vrela, vmaxi,&
                  vrefe, vcomp, vfrot, vgeom, irela,&
                  imaxi, irefe, noddlm, icomp, nfrot,&
                  ngeom)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/impcmp.h"
#include "asterfort/impcom.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmimck.h"
#include "asterfort/nmimcr.h"
    character(len=24) :: numedd, sdimpr, sdconv
    integer :: irela, imaxi, irefe, icomp
    real(kind=8) :: vrela, vmaxi, vrefe, vcomp, vfrot, vgeom
    character(len=8) :: noddlm
    character(len=16) :: nfrot, ngeom
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! IMPRESSION DES INFORMATIONS SUR LES RESIDUS DANS LE TABLEAU DE
! CONVERGENCE
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NUMEROTATION NUME_DDL
! IN  SDIMPR : SD AFFICHAGE
! IN  SDCONV : SD GESTION DE LA CONVERGENCE
! IN  IRELA  : NUMERO DU DDL SUR LAQUELLE NORME_MAX (RESI_GLOB_RELA)
! IN  VRELA  : VALEUR NORME_MAX (RESI_GLOB_RELA)
! IN  IMAXI  : NUMERO DU DDL SUR LAQUELLE NORME_MAX (RESI_GLOB_MAXI)
! IN  VMAXI  : VALEUR NORME_MAX (RESI_GLOB_MAXI)
! IN  IREFE  : NUMERO DU DDL SUR LAQUELLE NORME_MAX (RESI_GLOB_REFE)
! IN  VREFE  : VALEUR NORME_MAX (RESI_GLOB_REFE)
! IN  ICOMP  : NUMERO DU NOEUD SUR LAQUELLE NORME_MAX (RESI_COMP_RELA)
! IN  VCOMP  : VALEUR NORME_MAX (RESI_COMP_RELA)
! IN  VFROT  : VALEUR NORME_MAX POUR RESI_FROT
! IN  NFROT  : LIEU OU VALEUR NORME_MAX POUR RESI_FROT
! IN  VGEOM  : VALEUR NORME_MAX POUR RESI_GEOM
! IN  NGEOM  : LIEU OU VALEUR NORME_MAX POUR RESI_GEOM
!
! ----------------------------------------------------------------------
!
    character(len=24) :: cnvlie, cnvval, cnvnco
    integer :: jcnvli, jcnvva, jcnvnc
    character(len=16) :: nrela, nmaxi, nrefe, ncomp
    integer :: iresi, nresi
    real(kind=8) :: vale
    character(len=16) :: lieu
    character(len=9) :: colonn
    aster_logical :: laffe
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD CONVERGENCE
!
    cnvlie = sdconv(1:19)//'.LIEU'
    cnvval = sdconv(1:19)//'.VALE'
    cnvnco = sdconv(1:19)//'.NCOL'
    call jeveuo(cnvlie, 'E', jcnvli)
    call jeveuo(cnvval, 'E', jcnvva)
    call jeveuo(cnvnco, 'L', jcnvnc)
    call jelira(cnvnco, 'LONMAX', ival=nresi)
!
! --- NOM DES DDLS
!
    call impcmp(irela, numedd, nrela)
    call impcmp(imaxi, numedd, nmaxi)
    call impcmp(irefe, numedd, nrefe)
    call impcom(icomp, noddlm, ncomp)
!
! --- BOUCLE SUR LES RESIDUS
!
    laffe = .true.
    do 10 iresi = 1, nresi
        colonn = zk16(jcnvnc-1+iresi)(1:9)
        lieu = ' '
        vale = r8vide()
        if (colonn .eq. 'RESI_RELA') then
            vale = vrela
            lieu = nrela
            call nmimcr(sdimpr, 'RESI_RELA', vrela, laffe)
            call nmimck(sdimpr, 'RELA_NOEU', nrela, laffe)
        else if (colonn.eq.'RESI_MAXI') then
            vale = vmaxi
            lieu = nmaxi
            call nmimcr(sdimpr, 'RESI_MAXI', vmaxi, laffe)
            call nmimck(sdimpr, 'MAXI_NOEU', nmaxi, laffe)
        else if (colonn.eq.'RESI_REFE') then
            vale = vrefe
            lieu = nrefe
            call nmimcr(sdimpr, 'RESI_REFE', vrefe, laffe)
            call nmimck(sdimpr, 'REFE_NOEU', nrefe, laffe)
        else if (colonn.eq.'RESI_COMP') then
            vale = vcomp
            lieu = ncomp
            call nmimcr(sdimpr, 'RESI_COMP', vcomp, laffe)
            call nmimck(sdimpr, 'COMP_NOEU', ncomp, laffe)
        else if (colonn.eq.'FROT_NEWT') then
            vale = vfrot
            lieu = nfrot
            call nmimcr(sdimpr, 'FROT_NEWT', vfrot, laffe)
            call nmimck(sdimpr, 'FROT_NOEU', nfrot, laffe)
        else if (colonn.eq.'GEOM_NEWT') then
            vale = vgeom
            lieu = ngeom
            call nmimcr(sdimpr, 'GEOM_NEWT', vgeom, laffe)
            call nmimck(sdimpr, 'GEOM_NOEU', ngeom, laffe)
        else
            ASSERT(.false.)
        endif
        zr(jcnvva-1+iresi) = vale
        zk16(jcnvli-1+iresi) = lieu
 10 end do
!
    call jedema()
end subroutine
