subroutine recmod(modmec, nbmode, nbamor, bande, tymmec,&
                  grdmod)
    implicit none
#include "jeveux.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveut.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbmode, nbamor
    real(kind=8) :: bande(2)
    character(len=8) :: modmec, tymmec
    character(len=16) :: grdmod
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!  BUT: RECUPERER LES INFORMATIONS DE TYPE MODE DYNAMIQUE POUR
!        LE CALCUL DYNAMIQUE ALEATOIRE
!
! OUT : MODMEC : NOMBRE DE MODES DYNAMIQUES UTILISES DANS LE CALCUL
! OUT : NBMODE : NOMBRE DE MODES DYNAMIQUES UTILISES DANS LE CALCUL
! OUT : NBAMOR : NOMBRE D AMORTISSEMENTS MODAUX DONNES
! OUT : BANDE  : LES DEUX BORNES DE L INTERVALLE DE FREQUENCES
! OUT : TYMMEC : TPYE R OU C POUR LES MODES MECAS
! OUT : GRDMOD : TYPE DE GRANDEUR A RECUPERER DANS LES MODES DYN ET STA
!-----------------------------------------------------------------------
!
    integer :: ibid, nbtrou, nbmod1, lnumor, ilmode, iad, imod1, im, iret
    integer :: iadrmg, ilamod, ilamor, na1
    real(kind=8) :: rbid, freq1, amunif
    complex(kind=8) :: c16b
    character(len=8) :: k8b
    character(len=24) :: nomcha
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
!-----MODES RETENUS
!
    call getvid('BASE_MODALE', 'MODE_MECA', iocc=1, scal=modmec, nbret=ibid)
!
    call rsorac(modmec, 'LONUTI', ibid, rbid, k8b,&
                c16b, 0.0d0, k8b, nbmod1, 1,&
                nbtrou)
    call wkvect('&&RECMOD.NUMERO.ORDRE', 'V V I', nbmod1, lnumor)
    call rsorac(modmec, 'TOUT_ORDRE', ibid, rbid, k8b,&
                c16b, 0.0d0, k8b, zi(lnumor), nbmod1,&
                nbtrou)
!
    call getvis('BASE_MODALE', 'NUME_ORDRE', iocc=1, nbval=0, nbret=nbmode)
    nbmode = -nbmode
    if (nbmode .eq. 0) then
        call getvr8('BASE_MODALE', 'BANDE', iocc=1, nbval=2, vect=bande,&
                    nbret=ibid)
        call wkvect('&&OP0131.LISTEMODES', 'V V I', nbmod1, ilmode)
        do 126 im = 1, nbmod1
            imod1 = zi(lnumor+im-1)
            call rsadpa(modmec, 'L', 1, 'FREQ', imod1,&
                        0, iad, k8b)
            freq1 = zr(iad)
            if ((freq1-bande(1))*(freq1-bande(2)) .le. 0.d0) then
                nbmode = nbmode + 1
                zi(ilmode-1+nbmode) = imod1
            endif
126      continue
        if (nbmode .eq. 0) then
            call u2mess('F', 'ALGORITH10_31')
        endif
    else
        call wkvect('&&OP0131.LISTEMODES', 'V V I', nbmode, ilmode)
        call getvis('BASE_MODALE', 'NUME_ORDRE', iocc=1, nbval=nbmode, vect=zi(ilmode),&
                    nbret=ibid)
        do 232 im = 1, nbmode
            if (zi(ilmode-1+im) .gt. nbmod1) then
                call u2mess('F', 'ALGORITH10_32')
            endif
232      continue
    endif
!
!----AMORTISSEMENTS MODAUX RETENUS
!
    call wkvect('&&OP0131.LISTEAMOR', 'V V R8', nbmode, ilamor)
    call getvr8('BASE_MODALE', 'AMOR_REDUIT', iocc=1, nbval=0, nbret=na1)
    nbamor = - ( na1 )
    if (nbamor .ne. 0) then
        if (na1 .ne. 0) then
            call getvr8('BASE_MODALE', 'AMOR_REDUIT', iocc=1, nbval=nbmode, vect=zr(ilamor),&
                        nbret=na1)
        endif
    else
        call getvr8('BASE_MODALE', 'AMOR_UNIF', iocc=1, scal=amunif, nbret=ibid)
        do 127 im = 1, nbmode
            zr(ilamor-1+im) = amunif
127      continue
    endif
!
!------CONSITUTION DE LA LISTE DES ADRESSES DES MODES DYNAMIQUES
!
    grdmod = 'DEPL'
    call wkvect('&&OP0131.LISTADRMODE', 'V V I', nbmode, ilamod)
    do 211 im = 1, nbmode
        imod1 = zi(ilmode+im-1)
        call rsexch('F', modmec, grdmod, imod1, nomcha,&
                    iret)
        call jeveut(nomcha(1:19)//'.VALE', 'L', zi(ilamod+im-1))
211  end do
    call jelira(nomcha(1:19)//'.VALE', 'TYPE', cval=tymmec)
!
!-----RECUPERATION DE LA MASSE GENERALISEE
!
    call wkvect('&&OP0131.MASSEGENE', 'V V R8', nbmode, iadrmg)
    do 231 im = 1, nbmode
        imod1 = zi(ilmode+im-1)
        call rsadpa(modmec, 'L', 1, 'MASS_GENE', imod1,&
                    0, iad, k8b)
        zr(iadrmg+im-1) = zr(iad)
231  end do
!
    call jedetr('&&RECMOD.NUMERO.ORDRE')
    call jedema()
end subroutine
