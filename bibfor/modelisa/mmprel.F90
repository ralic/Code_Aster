subroutine mmprel(char, noma, nomo, ligret)
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
    implicit none
#include "jeveux.h"
#include "asterfort/ajellt.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mmmaxi.h"
#include "asterfort/wkvect.h"
    character(len=8) :: char, noma, nomo
    character(len=19) :: ligret
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - LECTURE DONNEES)
!
! CREATION DES ELEMENTS ESCLAVES TARDIFS
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! OUT LIGRET : LIGREL D'ELEMENTS TARDIFS DU CONTACT
!
!
!
!
    character(len=24) :: paraci, contma
    integer :: jparci, jmaco
    logical :: lfrot, laxis, lveri
    character(len=24) :: lismae, defico
    character(len=16) :: modeli, phenom
    integer :: jdecme, jlist, izone
    integer :: nzoco, ndimg, nmaco, ntmaec
    integer :: imae, posmae, nummae, nbmae
    logical :: lallv
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    lismae = '&&MMPREL.LISTE_MAILLES'
    defico = char(1:8)//'.CONTACT'
!
! --- TYPES DE CONTACT
!
    lallv = cfdisl(defico,'ALL_VERIF')
    if (lallv) then
        goto 99
    endif
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    contma = defico(1:16)//'.MAILCO'
    paraci = defico(1:16)//'.PARACI'
    call jeveuo(contma, 'L', jmaco)
    call jeveuo(paraci, 'E', jparci)
!
! --- RECUPERATION DU NOM DU PHENOMENE ET DE LA  MODELISATION
!
    call dismoi('PHENOMENE', nomo, 'MODELE', repk=phenom)
!
! --- INFOS DE LA ZONE
!
    ndimg = cfdisi(defico,'NDIM' )
    nmaco = cfdisi(defico,'NMACO' )
    ntmaec = cfdisi(defico,'NTMAEC')
    nzoco = cfdisi(defico,'NZOCO' )
!
! --- MAILLES AXISYMETRIQUES ?
!
    laxis = .false.
    if (ndimg .eq. 2) then
        laxis = mmmaxi(nomo ,contma,nmaco)
    endif
    if (laxis) then
        zi(jparci+16-1) = 1
    else
        zi(jparci+16-1) = 0
    endif
!
! --- AJOUT DES ELEMENTS TARDIFS AU LIGREL
!
    call wkvect(lismae, 'V V I', ntmaec, jlist)
    do izone = 1, nzoco
        lfrot = mminfl(defico,'FROTTEMENT_ZONE',izone)
        lveri = mminfl(defico,'VERIF',izone )
        if (ndimg .eq. 2) then
            if (lfrot) then
                modeli = 'COFR_DVP_2D'
            else
                modeli = 'CONT_DVP_2D'
            endif
        else if (ndimg.eq. 3) then
            if (lfrot) then
                modeli = 'COFR_DVP_3D'
            else
                modeli = 'CONT_DVP_3D'
            endif
        else
            ASSERT(.false.)
        endif
!
        if (.not.lveri) then
            nbmae = mminfi(defico,'NBMAE' ,izone )
            jdecme = mminfi(defico,'JDECME' ,izone )
            do imae = 1, nbmae
                posmae = jdecme+imae
                nummae = zi(jmaco+posmae-1)
                zi(jlist+imae-1) = nummae
            end do
            call ajellt(ligret, noma, nbmae, lismae, ' ',&
                        phenom, modeli, 0, ' ')
        endif
    end do
!
 99 continue
!
    call jedetr(lismae)
    call jedema()
end subroutine
