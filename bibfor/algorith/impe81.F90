subroutine impe81(nomres, impe, basemo)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/r8pi.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/infmaj.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: nomres, basemo
    character(len=19) :: impe
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     BUT:
!       REMPLIR
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NOMRES    : NOM DE LA SD_RESULTAT
! IN   IMPE      : NOM DE LA MATRICE D'IMPEDANCE
! IN   BASEMO    : NOM DE LA BASE MODALE DE PROJECTION
!
!      SORTIE :
!-------------
!
! ......................................................................
!
!
!
!
    integer :: i, j, nbmode
    integer :: ldblo, ldbloi, lddesa, lddesm, lddesr, ldrefa, ldrefm
    integer :: ldrefr, ldresa, ldresm, ldresr, ldresi, ldrefi
    integer :: nbdef, nbmodd, nbmods, nfr, nim, ntail
    integer :: nk, nc, nm, ldblok, ldbloc, ldblom
!
    real(kind=8) :: partr, parti, partr0, parti0
    real(kind=8) :: amso, dpi, freq
!
    character(len=8) :: blanc
    character(len=16) :: typres, nomcom
    character(len=19) :: impini
    character(len=19) :: impk, impm, impc
!
    data blanc /'        '/
!
!-----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
    call getres(nomres, typres, nomcom)
    dpi = 2.d0*r8pi()
!
! --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
!
    call getvr8(' ', 'FREQ_EXTR', scal=freq, nbret=nfr)
    call getvr8(' ', 'AMOR_SOL', scal=amso, nbret=nfr)
    call getvid(' ', 'MATR_IMPE_INIT', scal=impini, nbret=nim)
    call getvid(' ', 'MATR_IMPE_RIGI', scal=impk, nbret=nk)
    call getvid(' ', 'MATR_IMPE_MASS', scal=impm, nbret=nm)
    call getvid(' ', 'MATR_IMPE_AMOR', scal=impc, nbret=nc)
    if (nfr .ne. 0) amso = 2.d0*amso
!
    call wkvect(nomres//'.MAEL_RAID_REFE', 'G V K24', 2, ldrefr)
    zk24(ldrefr) = basemo
    zk24(ldrefr+1) = blanc
!
    call wkvect(nomres//'.MAEL_MASS_REFE', 'G V K24', 2, ldrefm)
    zk24(ldrefm) = basemo
    zk24(ldrefm+1) = blanc
!
    call wkvect(nomres//'.MAEL_AMOR_REFE', 'G V K24', 2, ldrefa)
    zk24(ldrefa) = basemo
    zk24(ldrefa+1) = blanc
!
!
    call dismoi('NB_MODES_DYN', basemo, 'RESULTAT', repi=nbmodd)
    call dismoi('NB_MODES_STA', basemo, 'RESULTAT', repi=nbmods)
    nbmode = nbmodd + nbmods
!
! --- RECUPERATION DES DIMENSIONS DE LA BASE MODALE
!
    nbdef = nbmode
!
! --- ALLOCATION DE LA MATRICE RESULTAT
!
    ntail = nbdef* (nbdef+1)/2
    call jecrec(nomres//'.MAEL_RAID_VALE', 'G V R', 'NU', 'DISPERSE', & 
                'CONSTANT',1)   
    call jeecra(nomres//'.MAEL_RAID_VALE', 'LONMAX', ntail)
    call jecroc(jexnum(nomres//'.MAEL_RAID_VALE', 1))
    call jeveuo(jexnum(nomres//'.MAEL_RAID_VALE', 1), 'E', ldresr)
!
    call jecrec(nomres//'.MAEL_MASS_VALE', 'G V R', 'NU', 'DISPERSE', & 
                'CONSTANT',1)   
    call jeecra(nomres//'.MAEL_MASS_VALE', 'LONMAX', ntail)
    call jecroc(jexnum(nomres//'.MAEL_MASS_VALE', 1))
    call jeveuo(jexnum(nomres//'.MAEL_MASS_VALE', 1), 'E', ldresm)
!
    call jecrec(nomres//'.MAEL_AMOR_VALE', 'G V R', 'NU', 'DISPERSE', & 
                'CONSTANT',1)   
    call jeecra(nomres//'.MAEL_AMOR_VALE', 'LONMAX', ntail)
    call jecroc(jexnum(nomres//'.MAEL_AMOR_VALE', 1))
    call jeveuo(jexnum(nomres//'.MAEL_AMOR_VALE', 1), 'E', ldresa)
!
!   call wkvect(nomres//'.MAEL_RAID_VALE', 'G V R', ntail, ldresr)
!   call wkvect(nomres//'.MAEL_MASS_VALE', 'G V R', ntail, ldresm)
!   call wkvect(nomres//'.MAEL_AMOR_VALE', 'G V R', ntail, ldresa)
!
!
!        BOUCLE SUR LES COLONNES DE LA MATRICE ASSEMBLEE
!
!
    call jeveuo(jexnum(impe//'.VALM', 1), 'L', ldblo)
    if (nim .ne. 0) call jeveuo(jexnum(impini//'.VALM', 1), 'L', ldbloi)
    if (nk .ne. 0) call jeveuo(jexnum(impk//'.VALM', 1), 'L', ldblok)
    if (nm .ne. 0) call jeveuo(jexnum(impm//'.VALM', 1), 'L', ldblom)
    if (nc .ne. 0) call jeveuo(jexnum(impc//'.VALM', 1), 'L', ldbloc)
    do i = 1, nbmode
!
! --------- BOUCLE SUR LES INDICES VALIDES DE LA COLONNE I
!
        do j = 1, i
!
!
            zr(ldresr+i*(i-1)/2+j-1) = 0.d0
            zr(ldresa+i*(i-1)/2+j-1) = 0.d0
            zr(ldresm+i*(i-1)/2+j-1) = 0.d0
            if (i .gt. nbmodd .and. j .gt. nbmodd) then
!
! ----------- STOCKAGE DANS LE .UALF A LA BONNE PLACE (1 BLOC)
!
                if ((nk+nm+nc) .eq. 0) then
                    partr = dble(zc(ldblo+i*(i-1)/2+j-1))
                    parti = dimag(zc(ldblo+i*(i-1)/2+j-1))
                    zr(ldresr+i*(i-1)/2+j-1)=partr
                    zr(ldresa+i*(i-1)/2+j-1)=(parti-amso*partr)/(dpi*&
                    freq)
                    if (nim .ne. 0) then
                        partr0 = dble(zc(ldbloi+i*(i-1)/2+j-1))
                        parti0 = dimag(zc(ldbloi+i*(i-1)/2+j-1))
                        zr(ldresr+i*(i-1)/2+j-1) = partr0
                        zr(ldresa+i*(i-1)/2+j-1) = (parti-parti0)/( dpi*freq)
                        zr(ldresm+i*(i-1)/2+j-1) = (partr0-partr)/( dpi*freq)**2
                    endif
                else
                    if (nk .ne. 0) zr( ldresr+i*(i-1)/2+j-1)=dble(zc( ldblok+i*(i-1)/2+j-1) )
                    if (nm .ne. 0) zr( ldresm+i*(i-1)/2+j-1)=dble(zc( ldblom+i*(i-1)/2+j-1) )
                    if (nc .ne. 0) zr( ldresa+i*(i-1)/2+j-1)=dble(zc( ldbloc+i*(i-1)/2+j-1) )
                endif
            endif
!
        end do
    end do
!
! --- CREATION DU .DESC
!
    call wkvect(nomres//'.MAEL_RAID_DESC', 'G V I', 3, lddesr)
    zi(lddesr) = 2
    zi(lddesr+1) = nbdef
    zi(lddesr+2) = 2
    call wkvect(nomres//'.MAEL_MASS_DESC', 'G V I', 3, lddesm)
    zi(lddesm) = 2
    zi(lddesm+1) = nbdef
    zi(lddesm+2) = 2
    call wkvect(nomres//'.MAEL_AMOR_DESC', 'G V I', 3, lddesa)
    zi(lddesa) = 2
    zi(lddesa+1) = nbdef
    zi(lddesa+2) = 2
!     INER
    call wkvect(nomres//'.MAEL_INER_REFE', 'G V K24', 2, ldrefi)
    zk24(ldrefr) = basemo
    zk24(ldrefr+1) = blanc
    call wkvect(nomres//'.MAEL_INER_VALE', 'G V R', 3*nbdef, ldresi)
!
    call jedema()
end subroutine
