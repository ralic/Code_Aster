subroutine arlver(modele, lgma, nbgma, nomsd, model,&
                  cine)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
! ======================================================================
!
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/tri.h"
#include "asterfort/arlelt.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/jeexin.h"
#include "asterfort/jexnom.h"
#include "asterfort/jelira.h"
#include "asterfort/jexnum.h"
#include "asterfort/jenuno.h"
#include "asterfort/utmess.h"
#include "asterfort/jedetr.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
!
    character(len=8) :: modele
    character(len=8) :: lgma(*)
    integer :: nbgma
    character(len=10) :: nomsd
    character(len=8) :: model
    character(len=8) :: cine
!
! ----------------------------------------------------------------------
!
! ROUTINE ARLEQUIN
!
! FILTRE, REGROUPEMENT, VERIFICATION GROUPES DE MAILLES
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  LGMA   : NOMS DES GROUPES DE MAILLES
! I/O NBGMA  : EN ENTREE -> NOMBRE DE GROUPES DE MAILLES
!              EN SORTIE -> NOMBRE DE MAILLES DANS .GROUPEMA
! I/O NOMSD  : SD DOMAINE
! OUT MODEL  : MODELISATION ASSOCIEE AUX MAILLES ('3D')
! OUT CINE   : CINEMATIQUE ASSOCIEE AUX MAILLES
!              'SOLIDE' OU 'POUTRE'
!               UN POUR CHAQUE GROUPE
!
! SD DE SORTIE (NOMSD) :
! ======================
! .GROUPEMA : LISTE TRIEE DES MAILLES DU DOMAINE NOMSD (MA1,MA2,...)
!
!
    character(len=8) :: noma, k8bid
    character(len=16) :: nomte, modte, cinte
    integer :: nbma, ntot, nbligr, numa, ninit
    integer :: icompt, igma, ima, iligr, iret
    integer :: jmail, jrepe, jcompt, jte, jgma, jgroup, jtyel
    aster_logical :: eltok
    integer :: liste(nbgma)
!
    character(len=6) :: nompro
    parameter (nompro='ARLVER')
!
! ---------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ninit = 0
    cinte = ' '
    modte = ' '
!
! --- ACCES MODELE
!
    call jeveuo(modele(1:8)//'.MODELE    .LGRF', 'L', jmail)
    call jeveuo(modele(1:8)//'.MODELE    .REPE', 'L', jrepe)
    call jelira(modele(1:8)//'.MODELE    .LIEL', 'NMAXOC', nbligr, k8bid)
    call jeveuo(modele(1:8)//'.MAILLE', 'L', jtyel)
!
! --- NOM DU MAILLAGE
!
    noma = zk8(jmail)
!
! --- ALLOCATION OBJETS TEMPORAIRES
!
    call wkvect('&&'//nompro//'.COMPTEUR', 'V V I', nbligr, jcompt)
    call wkvect('&&'//nompro//'.TE', 'V V I', nbligr, jte)
!
! --- COMPTER LE NOMBRE DE MAILLES AFFECTEES A CHAQUE TYPE D'ELEMENT
! --- DONNER LE NUMERO ABSOLU DU NUMERO D'ELEMENT AFFECTE A CHAQUE TYPE
!
    do igma = 1, nbgma
        call jeexin(jexnom(noma(1:8)//'.GROUPEMA', lgma(igma)), iret)
        if (iret == 0) then
            ASSERT(.false.)
        endif
        call jeveuo(jexnom(noma(1:8)//'.GROUPEMA', lgma(igma)), 'L', jgma)
        call jelira(jexnom(noma(1:8)//'.GROUPEMA', lgma(igma)), 'LONMAX', nbma, k8bid)
        ninit = ninit + nbma
        do ima = 1, nbma
            numa = zi(jgma-1+ima)
            iligr = zi(jrepe+2*(numa-1))
            if (iligr /= 0) then
                if (zi(jcompt-1+iligr) == 0) then
                    zi(jte-1+iligr) = zi(jtyel-1+numa)
                endif
                zi(jcompt-1+iligr) = zi(jcompt-1+iligr) + 1
            endif
        end do
    end do
!
! --- VERIFICATION COHERENCE MODELISATION / CINEMATIQUE
!
    ntot = 0
    do 30 iligr = 1, nbligr
        if (zi(jcompt-1+iligr) /= 0) then
            call jenuno(jexnum('&CATA.TE.NOMTE', zi(jte-1+iligr)), nomte)
            eltok = arlelt(nomte,modte,cinte)
            if (.not. eltok) then
                zi(jcompt-1+iligr) = 0
                goto 30
            else
                if (ntot == 0) then
                    model = modte(1:8)
                    cine = cinte(1:8)
                else
                    if (modte /= model) then
                        ASSERT(.false.)
                    endif
                    if (cinte /= cine) then
                        ASSERT(.false.)
                    endif
                endif
                ntot = ntot + zi(jcompt-1+iligr)
            endif
        endif
 30 end do
!
! --- AUCUNE MAILLE DU GROUPE N'EST UTILISABLE
!
    if (ntot == 0) then
        ASSERT(.false.)
    endif
!
! --- ALLOCATION .GROUPEMA
!
    call wkvect(nomsd//'.GROUPEMA', 'V V I', ntot, jgroup)
!
! --- COPIE DES MAILLES VALIDES
!
    icompt = 0
    do igma = 1, nbgma
        call jeveuo(jexnom(noma//'.GROUPEMA', lgma(igma)), 'L', jgma)
        call jelira(jexnom(noma//'.GROUPEMA', lgma(igma)), 'LONMAX', nbma, k8bid)
        do ima = 1, nbma
            numa = zi(jgma-1+ima)
            iligr = zi(jrepe+2*(numa-1))
            if ((iligr /= 0) .and. (zi(jcompt-1+iligr) /= 0)) then
                zi(jgroup+icompt) = numa
                icompt = icompt + 1
            endif
        end do
    end do
!
    if (icompt /= ntot) then
        ASSERT(.false.)
    endif
    nbgma = ntot
    call tri(zi(jgroup), liste, 0, nbgma)
!
! --- MENAGE
!
    call jedetr('&&'//nompro//'.COMPTEUR')
    call jedetr('&&'//nompro//'.TE')
!
    call jedema()
!
end subroutine
