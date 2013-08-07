subroutine cacoco(char, motfac, noma)
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
!
#include "asterc/getvid.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cfdisi.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    character(len=8) :: char, noma
    character(len=16) :: motfac
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! LECTURE DES CARACTERISTIQUES DE COQUE
! REMPLISSAGE DE LA SD DEFICO(1:16)//'.JEUCOQ'
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  NOMA   : NOM DU MAILLAGE
!
!
!
!
    integer :: iret, noc, nbnma, iacnex
    integer :: icesc, icesd, icesl, icesv, npmax
    integer :: rangr0, rangr1, iad1
    integer :: nzoco, nbmae
    integer :: posmae, nummae
    integer :: jdecme
    integer :: izone, imae, nmaco
    real(kind=8) :: ep, exc
    logical :: ya
    character(len=8) ::  carael, nommae
    character(len=24) :: defico
    character(len=24) :: contma, jeucoq
    integer :: jmaco, jjcoq
    character(len=19) :: carsd, carte
    logical :: ldcoq
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    nzoco = cfdisi(defico,'NZOCO')
    nmaco = cfdisi(defico,'NMACO')
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    contma = defico(1:16)//'.MAILCO'
    call jeveuo(contma, 'L', jmaco)
!
! --- CREATION VECTEUR
!
    jeucoq = defico(1:16)//'.JEUCOQ'
    call wkvect(jeucoq, 'G V R', nmaco, jjcoq)
!
! --- RECUPERATION DU CARA_ELEM
!
    ya = .false.
    do 10 izone = 1, nzoco
        ldcoq = mminfl(defico,'DIST_COQUE',izone )
        if (ldcoq) then
            ya = .true.
            call getvid(motfac, 'CARA_ELEM', izone, iarg, 1,&
                        carael, noc)
            if (noc .eq. 0) then
                ASSERT(.false.)
            endif
        endif
10  end do
!
    if (.not. ya) then
        goto 999
    endif
!
    carte = carael//'.CARCOQUE'
    carsd = '&&CACOCO.CARCOQUE'
    call carces(carte, 'ELEM', ' ', 'V', carsd,&
                'A', iret)
!
! --- RECUPERATION DES GRANDEURS (EPAIS, EXCENT)
! --- REFERENCEE PAR LA CARTE CARGEOPO
!
    call jeveuo(carsd//'.CESC', 'L', icesc)
    call jeveuo(carsd//'.CESD', 'L', icesd)
    call jeveuo(carsd//'.CESL', 'L', icesl)
    call jeveuo(carsd//'.CESV', 'L', icesv)
!
! --- ON RECUPERE L'EPAISSEUR DE LA COQUE
!
    npmax = zi(icesd-1+2)
    rangr0 = indik8(zk8(icesc),'EP      ',1,npmax)
    rangr1 = indik8(zk8(icesc),'EXCENT  ',1,npmax)
!
    do 20 izone = 1, nzoco
        ldcoq = mminfl(defico,'DIST_COQUE',izone )
        if (ldcoq) then
!
            nbmae = mminfi(defico,'NBMAE' ,izone )
            jdecme = mminfi(defico,'JDECME',izone )
!
            do 30 imae = 1, nbmae
!
                posmae = jdecme+imae
                nummae = zi(jmaco+posmae-1)
                call jenuno(jexnum(noma//'.NOMMAI', nummae), nommae)
!
! --- RECUPERATION EPAISSEUR
!
                call cesexi('C', icesd, icesl, nummae, 1,&
                            1, rangr0, iad1)
                if (iad1 .gt. 0) then
                    ep = zr(icesv-1+iad1)
                else
                    call u2mesk('F', 'CONTACT3_39', 1, nommae)
                endif
!
! --- RECUPERATION EXCENTRICITE
!
                call cesexi('C', icesd, icesl, nummae, 1,&
                            1, rangr1, iad1)
                if (iad1 .gt. 0) then
                    exc = zr(icesv-1+iad1)
                    if (exc .ne. 0.d0) then
                        call u2mesk('F', 'CONTACT3_40', 1, nommae)
                    endif
                else
                    call u2mesk('F', 'CONTACT3_41', 1, nommae)
                endif
!
! --- NOEUDS DE LA MAILLE
!
                call jeveuo(jexnum(noma//'.CONNEX', nummae), 'L', iacnex)
                call jelira(jexnum(noma//'.CONNEX', nummae), 'LONMAX', nbnma)
!
! --- STOCKAGE
!
                zr(jjcoq+posmae-1) = 0.5d0 * ep
30          continue
        endif
20  end do
!
    call detrsd('CHAM_ELEM_S', carsd)
!
999  continue
!
    call jedema()
!
end subroutine
