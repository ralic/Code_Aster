subroutine rscopi(base, sd1, sd2)
    implicit none
#include "jeveux.h"
!
#include "asterfort/copich.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsnopa.h"
    character(len=*) :: base, sd1, sd2
! ----------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
!
!   BUT:
!   DUPLIQUER UNE STRUCTURE DE DONNEES "RESULTAT".
!             EN DUPLIQUANT TOUS LES CHAMPS CONTENU DANS LE .TACH
!
!     IN:
!     BASE     : 'G' , 'V' , ... : BASE DE CREATION DE SD2
!     SD1 (K*) : NOM DE LA SD A DUPPLIQUER
!     SD2 (K*) : NOM DE LA SD A CREER
!
!     OUT:
!     SD2 EST CREEE ET A LE MEME CONTENU QUE SD1
!
!-----------------------------------------------------------------------
!
    integer ::  i, j, nbcham, nbordr, iret, nbac, nbpara, nbpa, jpa, ipara
    integer :: iatava
    logical :: dejfai
    character(len=1) :: bas2
    character(len=4) :: type, typacc
    character(len=5) :: nomobj
    character(len=16) :: nopara, nomsy
    character(len=19) :: sdr1, sdr2, ch1, ch2
    character(len=24) :: nompar
    integer, pointer :: ordr(:) => null()
!
! DEB-------------------------------------------------------------------
!
    call jemarq()
    bas2 = base
!
    sdr1 = sd1
    sdr2 = sd2
    call jelira(sdr1//'.DESC', 'NOMMAX', nbcham)
    call jelira(sdr1//'.ORDR', 'LONUTI', nbordr)
    call jeveuo(sdr1//'.ORDR', 'L', vi=ordr)
!
!     --- LE .DESC, .NOVA, .TAVA, .ORDR ---
!
    call jedupo(sdr1//'.DESC', bas2, sdr2//'.DESC', .false.)
    call jedupo(sdr1//'.NOVA', bas2, sdr2//'.NOVA', .false.)
    call jedupo(sdr1//'.TAVA', bas2, sdr2//'.TAVA', .false.)
    call jedupo(sdr1//'.ORDR', bas2, sdr2//'.ORDR', .false.)
    call jedupo(sdr1//'.REFD', bas2, sdr2//'.REFD', .false.)
    call jedupo(sdr1//'.INDI', bas2, sdr2//'.INDI', .false.)

!
!     --- LE .TACH ---
!
    call jecrec(sdr2//'.TACH', 'G V K24', 'NU', 'CONTIG', 'CONSTANT',&
                nbcham)
    call jeecra(sdr2//'.TACH', 'LONMAX', nbordr)
!
!     --- ON DUPLIQUE LES CHAMPS ---
!
    do 20 i = 1, nbcham
        call jenuno(jexnum(sdr1//'.DESC', i), nomsy)
        call jecroc(jexnum(sdr2//'.TACH', i))
        do 10 j = 0, nbordr - 1
            call rsexch(' ', sd1, nomsy, ordr(1+j), ch1,&
                        iret)
            if (iret .eq. 0) then
                call rsexch(' ', sd2, nomsy, ordr(1+j), ch2,&
                            iret)
                call copich(bas2, ch1, ch2)
                call rsnoch(sd2, nomsy, ordr(1+j))
            endif
10      continue
20  continue
!
!     --- LES VARIABLES ET PARAMETRES D'ACCES ---
!
    nompar = '&&RSCOPI.NOMS_PARA '
    call rsnopa(sdr1, 2, nompar, nbac, nbpa)
    nbpara = nbac + nbpa
    call jeveuo(nompar, 'L', jpa)
!
    dejfai = .false.
    do 30 j = 1, nbpara
        nopara = zk16(jpa+j-1)
        call jenonu(jexnom(sdr1//'.NOVA', nopara), ipara)
        call jeveuo(jexnum(sdr1//'.TAVA', ipara), 'L', iatava)
        nomobj = zk8(iatava-1+1) (1:5)
        typacc = zk8(iatava-1+4) (1:4)
!
        call jelira(sdr1//nomobj, 'TYPE', cval=type(1:1))
!
        if (typacc .eq. 'PARA' .and. type(1:1) .eq. 'R') then
            if (dejfai) goto 30
            dejfai = .true.
            nomobj = '.PARA'
        endif
        call jedupo(sdr1//nomobj, bas2, sdr2//nomobj, .false.)
!
30  continue
    call jedetr(nompar)
!
    call jedema()
end subroutine
