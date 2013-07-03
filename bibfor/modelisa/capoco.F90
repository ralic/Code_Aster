subroutine capoco(char, motfac)
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
#include "asterc/getvid.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cfdisi.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: char
    character(len=16) :: motfac
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! LECTURE DES CARACTERISTIQUES DE POUTRE
! REMPLISSAGE DE LA SD DEFICO(1:16)//'.JEUPOU'
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
!
!
!
!
    integer :: iret, noc
    integer :: icesc, icesd, icesl, icesv, npmax, isec
    integer :: posmae, nummae
    integer :: jdecme
    integer :: izone, imae
    integer :: nbmae
    integer :: rangr0, rangr1, rangr2, iad1, iad2
    real(kind=8) :: r1, r2, rayon
    logical :: ya
    integer :: nzoco, nmaco
    character(len=8) :: carael
    character(len=24) :: defico
    character(len=24) :: jeupou, contma
    integer :: jmaco, jjpou
    character(len=19) :: carsd, carte
    logical :: ldpou
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
    jeupou = defico(1:16)//'.JEUPOU'
    call wkvect(jeupou, 'G V R', nmaco, jjpou)
!
! --- RECUPERATION DU CARA_ELEM
!
    ya = .false.
    do 10 izone = 1, nzoco
        ldpou = mminfl(defico,'DIST_POUTRE',izone )
        if (ldpou) then
            ya = .true.
            call getvid(motfac, 'CARA_ELEM', izone, iarg, 1,&
                        carael, noc)
            if (noc .eq. 0) then
                call assert(.false.)
            endif
        endif
10  end do
!
    if (.not. ya) then
        goto 999
    endif
!
! --- TRANSFO. CARTE CARA_ELEM EN CHAM_ELEM_S
!
    carte = carael//'.CARGEOPO'
    carsd = '&&CAPOCO.CARGEOPO'
    call carces(carte, 'ELEM', ' ', 'V', carsd,&
                'A', iret)
!
! --- RECUPERATION DES GRANDEURS (TSEC, R1, R2)
! --- REFERENCEE PAR LA CARTE CARGEOPO
!
    call jeveuo(carsd//'.CESC', 'L', icesc)
    call jeveuo(carsd//'.CESD', 'L', icesd)
    call jeveuo(carsd//'.CESL', 'L', icesl)
    call jeveuo(carsd//'.CESV', 'L', icesv)
!
! --- ON RECUPERE LE RAYON EXTERIEUR DE LA POUTRE
!
    npmax = zi(icesd-1+2)
    rangr0 = indik8(zk8(icesc),'TSEC    ',1,npmax)
    rangr1 = indik8(zk8(icesc),'R1      ',1,npmax)
    rangr2 = indik8(zk8(icesc),'R2      ',1,npmax)
!
    do 20 izone = 1, nzoco
        ldpou = mminfl(defico,'DIST_POUTRE',izone )
!
!
        if (ldpou) then
!
            nbmae = mminfi(defico,'NBMAE' ,izone )
            jdecme = mminfi(defico,'JDECME',izone )
!
            do 30 imae = 1, nbmae
!
                posmae = jdecme+imae
                nummae = zi(jmaco+posmae-1)
!
! --- TYPE DE SECTION (UNIQUEMENT CIRCULAIRE !)
!
                call cesexi('C', icesd, icesl, nummae, 1,&
                            1, rangr0, iad1)
                if (iad1 .gt. 0) then
                    isec = nint( zr(icesv-1+abs(iad1)) )
                else
                    isec = 0
                endif
                if (isec .ne. 2) then
                    call u2mess('F', 'CONTACT3_32')
                endif
!
! --- RECUPERATION RAYON
!
                call cesexi('C', icesd, icesl, nummae, 1,&
                            1, rangr1, iad1)
                call cesexi('C', icesd, icesl, nummae, 1,&
                            1, rangr2, iad2)
!
                if (iad1 .gt. 0) then
                    r1 = zr(icesv-1+iad1)
                else
                    call assert(.false.)
                endif
!
                if (iad2 .gt. 0) then
                    r2 = zr(icesv-1+iad2)
                else
                    call assert(.false.)
                endif
!
                if (r1 .ne. r2) then
                    call u2mess('I', 'CONTACT3_37')
                endif
!
                rayon = (r1+r2)/2.d0
!
! --- STOCKAGE
!
                zr(jjpou+posmae-1) = rayon
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
