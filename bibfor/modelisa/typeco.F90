subroutine typeco(char, noma)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cfnumm.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmelin.h"
#include "asterfort/mmelty.h"
#include "asterfort/mminfi.h"
#include "asterfort/mmssfr.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: noma, char
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! CONSTRUCTION DU TABLEAU POUR TYPE DE NOEUD/MAILLES
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: typeno, typema, maescl
    integer :: jtypno, jtypma, jmaesc
    integer :: ztypm, ztypn, zmaes
    integer :: nzoco, nnoco, nmaco, ntmae, iform
    integer :: izone
    integer :: jdecnm, jdecne, inoe, inom, nbnoe, nbnom
    integer :: jdecmm, jdecme, imae, imam, nbmae, nbmam
    integer :: posnom, posnoe
    integer :: posmam, posmae, nummae, nummam
    integer :: indmae, indmam
    integer :: ino, ima, posno, posma
    integer :: ndexfr(1), typint, nptm
    character(len=24) :: defico
    character(len=8) :: alias, nommae, nommam
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    nzoco = cfdisi(defico,'NZOCO' )
    nnoco = cfdisi(defico,'NNOCO' )
    nmaco = cfdisi(defico,'NMACO' )
    ntmae = cfdisi(defico,'NTMAE' )
    iform = cfdisi(defico,'FORMULATION')
!
! --- ACCES OBJETS JEVEUX
!
    typeno = defico(1:16)//'.TYPENO'
    typema = defico(1:16)//'.TYPEMA'
    maescl = defico(1:16)//'.MAESCL'
    ztypm = cfmmvd('ZTYPM')
    ztypn = cfmmvd('ZTYPN')
    zmaes = cfmmvd('ZMAES')
!
! --- CREATION DES TABLEAUX
!
    call wkvect(typeno, 'G V I', ztypn*nnoco, jtypno)
    call wkvect(typema, 'G V I', ztypm*nmaco, jtypma)
    call wkvect(maescl, 'G V I', zmaes*ntmae, jmaesc)
!
! --- REMPLISSAGE DU TABLEAU TYPE_NOEUD
!
    do izone = 1, nzoco
        nbnoe = mminfi(defico,'NBNOE' ,izone )
        nbnom = mminfi(defico,'NBNOM' ,izone )
        jdecne = mminfi(defico,'JDECNE',izone )
        jdecnm = mminfi(defico,'JDECNM',izone )
!
        do inom = 1, nbnom
            posnom = jdecnm + inom
            zi(jtypno+ztypn*(posnom-1)+1-1) = 1
            zi(jtypno+ztypn*(posnom-1)+2-1) = izone
        end do
!
        do inoe = 1, nbnoe
            posnoe = jdecne + inoe
            zi(jtypno+ztypn*(posnoe-1)+1-1) = -1
            zi(jtypno+ztypn*(posnoe-1)+2-1) = izone
        end do
!
    end do
!
! --- REMPLISSAGE DU TABLEAU TYPE_MAILLE - TYPE MAITRE OU ESCLAVE
!
    indmae = 0
    indmam = 0
    do izone = 1, nzoco
        nbmae = mminfi(defico,'NBMAE' ,izone )
        nbmam = mminfi(defico,'NBMAM' ,izone )
        jdecme = mminfi(defico,'JDECME',izone )
        jdecmm = mminfi(defico,'JDECMM',izone )
!
        do imam = 1, nbmam
            posmam = jdecmm + imam
            indmam = indmam + 1
            zi(jtypma+ztypm*(posmam-1)+1-1) = 1
            zi(jtypma+ztypm*(posmam-1)+2-1) = indmam
            if (iform .eq. 2) then
                call cfnumm(defico, posmam, nummam)
                call mmelty(noma, nummam, alias)
                if (alias .eq. 'PO1') then
                    call jenuno(jexnum(noma//'.NOMMAI', nummam), nommam)
                    call utmess('F', 'CONTACT3_2', sk=nommam)
                endif
            endif
        end do
!
        do imae = 1, nbmae
            posmae = jdecme + imae
            indmae = indmae + 1
            zi(jtypma+ztypm*(posmae-1)+1-1) = -1
            zi(jtypma+ztypm*(posmae-1)+2-1) = indmae
            if (iform .eq. 2) then
                call cfnumm(defico, posmae, nummae)
                call mmelty(noma, nummae, alias)
                if (alias .eq. 'PO1') then
                    call jenuno(jexnum(noma//'.NOMMAI', nummae), nommae)
                    call utmess('F', 'CONTACT3_2', sk=nommae)
                endif
            endif
        end do
    end do
!
! --- REMPLISSAGE DU TABLEAU DES MAILLES ESCLAVES MAESC
!
    indmae = 0
    do izone = 1, nzoco
        nbmae = mminfi(defico,'NBMAE' ,izone )
        jdecme = mminfi(defico,'JDECME',izone )
!
        do imae = 1, nbmae
            posmae = jdecme + imae
            indmae = indmae + 1
            call cfnumm(defico, posmae, nummae)
            if (iform .eq. 2) then
                typint = mminfi(defico,'INTEGRATION',izone )
                call mmelin(noma, nummae, typint, nptm)
                call mmssfr(defico, izone, posmae, ndexfr(1))
            else
                nptm = 0
                ndexfr(1) = 0
            endif
            zi(jmaesc+zmaes*(indmae-1)+1-1) = posmae
            zi(jmaesc+zmaes*(indmae-1)+2-1) = izone
            zi(jmaesc+zmaes*(indmae-1)+3-1) = nptm
            zi(jmaesc+zmaes*(indmae-1)+4-1) = ndexfr(1)
        end do
    end do
!
! --- VERIFS: TYPENO ET TYPEMA SANS TROUS !
!
    do ino = 1, nnoco
        posno = ino
        if (zi(jtypno+ztypn*(posno -1)+1-1) .eq. 0) then
            ASSERT(.false.)
        endif
    end do
!
    do ima = 1, nmaco
        posma = ima
        if (zi(jtypma+ztypm*(posma -1)+1-1) .eq. 0) then
            ASSERT(.false.)
        endif
    end do
!
    call jedema()
end subroutine
