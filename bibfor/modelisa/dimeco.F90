subroutine dimeco(char, ndim, nzoco, nsuco, nmaco,&
                  nnoco)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfnben.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfi.h"
#include "asterfort/u2mess.h"
    character(len=8) :: char
    integer :: ndim
    integer :: nzoco
    integer :: nsuco
    integer :: nmaco
    integer :: nnoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! CONSTRUCTION DU VECTEUR D'INFORMATION SUR LES LONGUEURS
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
! IN  NSUCO  : NOMBRE TOTAL DE SURFACES DE CONTACT
! IN  NMACO  : NOMBRE TOTAL DE MAILLES DES SURFACES
! IN  NNOCO  : NOMBRE TOTAL DE NOEUDS DES SURFACES
!
!
!
!
    character(len=24) :: ndimco
    integer :: jdim
    integer :: ntmano
    integer :: ntpc, ntpt
    integer :: izone, ibid, imam, imae
    integer :: nbnoe, nbnom, nbmae, nbmam
    integer :: ntnoe, ntnom, ntmae, ntmam
    integer :: nbnoec, nbnomc, nbmaec, nbmamc
    integer :: ntnoec, ntnomc, ntmaec, ntmamc
    integer :: nbpt, nbpc
    integer :: nnomae, nnomam
    integer :: jdecme, jdecmm, posmae, posmam
    character(len=24) :: defico
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
!
! --- ACCES AUX STRUCTURES DE DONNEES DE CONTACT
!
    ndimco = defico(1:16)//'.NDIMCO'
    call jeveuo(ndimco, 'E', jdim)
!
! --- DIMENSION DE L'ESPACE
!
    if (ndim .gt. 3) then
        call u2mess('A', 'CONTACT_84')
        if (ndim .eq. 1003) then
            ndim = 3
        else if (ndim.eq.1002) then
            ndim = 2
        else if (ndim.eq.23) then
            ndim = 2
        else
            ASSERT(.false.)
        endif
    endif
!
! --- TABLEAU CONTENANT LES LONGUEURS DES DIFFERENTS VECTEURS
! --- ET LE NOMBRE DE NOEUDS ESCLAVES MAXIMUM POUR CHAQUE ZONE
!
    zi(jdim+1-1) = ndim
    zi(jdim+2-1) = nzoco
    zi(jdim+3-1) = nsuco
    zi(jdim+4-1) = nmaco
    zi(jdim+5-1) = nnoco
    zi(jdim+6-1) = 0
    zi(jdim+7-1) = 0
!
!
! --- CALCUL DU NOMBRE TOTAL DE NOEUDS/MAILLES ESCLAVES/MAITRES
!
    ntnoe = 0
    ntmae = 0
    ntnom = 0
    ntmam = 0
    do 20 izone = 1, nzoco
        nbmae = mminfi(defico,'NBMAE' ,izone )
        nbnoe = mminfi(defico,'NBNOE' ,izone )
        nbmam = mminfi(defico,'NBMAM' ,izone )
        nbnom = mminfi(defico,'NBNOM' ,izone )
        ntnoe = ntnoe + nbnoe
        ntmae = ntmae + nbmae
        ntnom = ntnom + nbnom
        ntmam = ntmam + nbmam
20  end do
!
    zi(jdim+8 -1) = ntnoe
    zi(jdim+9 -1) = ntmae
    zi(jdim+10-1) = ntnom
    zi(jdim+11-1) = ntmam
!
! --- CALCUL DU NOMBRE TOTAL DE NOEUDS/MAILLES ESCL/MAIT AVEC CALCUL
!
    ntnoec = 0
    ntmaec = 0
    ntnomc = 0
    ntmamc = 0
    do 21 izone = 1, nzoco
        nbmaec = mminfi(defico,'NBMAEC' ,izone )
        nbnoec = mminfi(defico,'NBNOEC' ,izone )
        nbmamc = mminfi(defico,'NBMAMC' ,izone )
        nbnomc = mminfi(defico,'NBNOMC' ,izone )
        ntnoec = ntnoec + nbnoec
        ntmaec = ntmaec + nbmaec
        ntnomc = ntnomc + nbnomc
        ntmamc = ntmamc + nbmamc
21  end do
!
    zi(jdim+12-1) = ntnoec
    zi(jdim+13-1) = ntmaec
    zi(jdim+14-1) = ntnomc
    zi(jdim+15-1) = ntmamc
!
! --- NOMBRE TOTAL DE POINTS DE CONTACT
!
    ntpt = 0
    do 41 izone = 1, nzoco
        nbpt = mminfi(defico,'NBPT' ,izone )
        ntpt = ntpt + nbpt
41  end do
!
    zi(jdim+16-1) = ntpt
!
! --- NOMBRE TOTAL DE POINTS DE CONTACT EFFECTIF (CALCUL)
!
    ntpc = 0
    do 42 izone = 1, nzoco
        nbpc = mminfi(defico,'NBPC' ,izone )
        ntpc = ntpc + nbpc
42  end do
!
    zi(jdim+17-1) = ntpc
!
! --- NOMBRE TOTAL DE NOEUD AUX ELEMENTS (ELNO)
!
    ntmano = 0
    do 30 izone = 1, nzoco
        jdecme = mminfi(defico,'JDECME',izone )
        nbmae = mminfi(defico,'NBMAE' ,izone )
        do 31 imae = 1, nbmae
            posmae = jdecme + imae
            call cfnben(defico, posmae, 'CONNEX', nnomae, ibid)
            ntmano = ntmano + nnomae
31      continue
        jdecmm = mminfi(defico,'JDECMM',izone )
        nbmam = mminfi(defico,'NBMAM' ,izone )
        do 32 imam = 1, nbmam
            posmam = jdecmm + imam
            call cfnben(defico, posmam, 'CONNEX', nnomam, ibid)
            ntmano = ntmano + nnomam
32      continue
30  end do
!
    zi(jdim+18-1) = ntmano
!
    call jedema()
end subroutine
