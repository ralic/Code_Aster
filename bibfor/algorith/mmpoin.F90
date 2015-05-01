subroutine mmpoin(noma, defico, newgeo, sdappa)
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
!
#include "asterfort/apzoni.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmex.h"
#include "asterfort/cfnumm.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mcomce.h"
#include "asterfort/mmelin.h"
#include "asterfort/mmgaus.h"
#include "asterfort/mminfi.h"
#include "asterfort/mmnpoi.h"
#include "asterfort/mmnumn.h"
#include "asterfort/mmvalp.h"
    character(len=8) :: noma
    character(len=19) :: sdappa
    character(len=24) :: defico
    character(len=19) :: newgeo
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - SD APPARIEMENT)
!
! REMPLISSAGE DE LA SD APPARIEMENT - POINTS (COORD. ET NOMS)
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: appoin, apinfp
    integer :: jpoin, jinfp
    character(len=24) :: apnoms
    integer :: jpnoms
    integer :: suppok
    integer :: ip, iptm, izone, imae
    integer :: nbmae, nptm, nnomae, np
    integer :: posmae, nummae, numnoe
    integer :: jdecme
    integer :: typint
    real(kind=8) :: coorpt(3), coorme(27)
    real(kind=8) :: ksi1, ksi2, r8bid
    character(len=8) :: aliase, nommae
    character(len=16) :: nompt
    integer :: ndimg, nzoco
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... MISE A JOUR SD APPARIEMENT'
    endif
!
! --- ACCES SDAPPA
!
    appoin = sdappa(1:19)//'.POIN'
    call jeveuo(appoin, 'E', jpoin)
    apinfp = sdappa(1:19)//'.INFP'
    call jeveuo(apinfp, 'E', jinfp)
    apnoms = sdappa(1:19)//'.NOMS'
    call jeveuo(apnoms, 'E', jpnoms)
!
! --- INITIALISATIONS
!
    ndimg = cfdisi(defico,'NDIM' )
    nzoco = cfdisi(defico,'NZOCO')
!
! --- BOUCLE SUR LES ZONES
!
    ip = 1
    np = 0
    do 10 izone = 1, nzoco
!
! ----- INFORMATION SUR LA ZONE
!
        call apzoni(sdappa, izone, 'NBMAE', nbmae)
        call apzoni(sdappa, izone, 'JDECME', jdecme)
        typint = mminfi(defico,'INTEGRATION',izone )
!
! ----- BOUCLE SUR LES MAILLES
!
        do 20 imae = 1, nbmae
!
! ------- NUMERO ABSOLU DE LA MAILLE
!
            posmae = jdecme + imae
            call cfnumm(defico, posmae, nummae)
!
! ------- NOM DE LA MAILLE
!
            call jenuno(jexnum(noma//'.NOMMAI', nummae), nommae)
!
! ------- COORDONNEES DE LA MAILLE
!
            call mcomce(noma, newgeo, nummae, coorme, aliase,&
                        nnomae)
!
! ------- NOMBRE DE POINTS SUR LA MAILLE (DEPEND DU SCHEMA)
!
            call mmelin(noma, nummae, typint, nptm)
!
! ------- BOUCLE SUR LES POINTS
!
            do 30 iptm = 1, nptm
!
! --------- COORDONNNES DU POINT
!
                call mmgaus(aliase, typint, iptm, ksi1, ksi2,&
                            r8bid)
                call mmvalp(ndimg, aliase, nnomae, 3, ksi1,&
                            ksi2, coorme, coorpt)
                zr(jpoin + 3*(ip-1)+1-1) = coorpt(1)
                zr(jpoin + 3*(ip-1)+2-1) = coorpt(2)
                zr(jpoin + 3*(ip-1)+3-1) = coorpt(3)
!
! --------- NUMERO ABSOLU DU POINT DE CONTACT
!
                call mmnumn(noma, typint, nummae, nnomae, iptm,&
                            numnoe)
!
! --------- NOEUD EXCLU ?
!
                if (numnoe .gt. 0) then
                    call cfmmex(defico, 'CONT', izone, numnoe, suppok)
                    zi(jinfp+ip-1) = suppok
                endif
!
! --------- NOM DU POINT
!
                call mmnpoi(noma, nommae, numnoe, iptm, nompt)
                zk16(jpnoms+ip-1) = nompt
!
! --------- POINT SUIVANT
!
                ip = ip + 1
                np = np + 1
30          continue
20      continue
10  end do
!
    ASSERT(np.eq.cfdisi(defico, 'NTPT'))
!
    call jedema()
!
end subroutine
