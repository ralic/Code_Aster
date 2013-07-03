subroutine i2repr(conec, type, maille, chemin, ptchm,&
                  nbchm, m1, m2)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!**********************************************************************
!
!     RECHERCHE DES MAILLES SURFACIQUES SUR LESQUELLES S' APPUIT
!     UN ENSEMBLE DE MAILLES SURFACIQUES
!
!       CONEC  (IN)  : NOM DE L' OBJET CONNECTIVITE DU MAILLAGE
!
!       TYPE   (IN)  : NOM DE L' OBJET CONTENANT LE TYPE DES MAILLES
!
!       MAILLE (IN)  : TABLE DES MAILLES DE L' ENSEMBLE TRAITE
!
!       CHEMIN (IN)  : TABLE DES CHEMINS DE L' ENSEMBLE
!
!       PTCHM  (IN)  : TABLE D' ACCES A CHEMIN
!
!       NBCHM  (IN)  : NBR DE CHEMINS
!
!       M1     (OUT) : TABLE DES PREMIERES MAILLES D' APPUI
!
!       M2     (OUT) : TABLE DES SECONDES MAILLES D' APPUI
!
!**********************************************************************
!
#include "jeveux.h"
!
#include "asterfort/i2extf.h"
#include "asterfort/i2nbrf.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    character(len=24) :: type, conec
    integer :: maille(*), chemin(*), ptchm(*), nbchm, m1(*), m2(*)
!
    integer :: nbm, nbc, nbs, nbn
    integer :: ncd, ncg, nsd
    integer :: m, c, s, debchm, finchm, sgteff, sfini
    integer :: atypm, chm, afini, i, adrs, ansg, ansd
    character(len=8) :: typm
!
!
!
    character(len=1) :: k1bid
!
!
!
!-----------------------------------------------------------------------
    integer :: iatyma, nsg
!-----------------------------------------------------------------------
    call jemarq()
    call jelira(conec(1:15), 'NMAXOC', nbm, k1bid)
!
    nbs = ptchm(nbchm+1) - 1
    sgteff = nbs - nbchm
    sfini = 0
    m = 0
    s = 0
    c = 0
    nbn = 0
    nbc = 0
    nbn = 0
    ncd = 0
    ncg = 0
    nsd = 0
    nsg = 0
    debchm = 0
    finchm = 0
    atypm = 0
    chm = 0
    afini = 0
    typm = ' '
    adrs = 0
    ansd = 0
    ansg = 0
!
    call jecreo('&INTFINI', 'V V L')
    call jeecra('&INTFINI', 'LONMAX', nbs, ' ')
    call jeveuo('&INTFINI', 'E', afini)
!
    call jecreo('&INTNSG', 'V V I')
    call jeecra('&INTNSG', 'LONMAX', sgteff, ' ')
    call jeveuo('&INTNSG', 'E', ansg)
!
    call jecreo('&INTNSD', 'V V I')
    call jeecra('&INTNSD', 'LONMAX', sgteff, ' ')
    call jeveuo('&INTNSD', 'E', ansd)
!
    do 10, i = 1, nbs, 1
!
    zl(afini + i-1) = .false.
!
    10 end do
!
    do 20, i = 1, nbchm
!
    zl(afini + ptchm(chm+1)-2) = .true.
!
    20 end do
!
    do 30, i = 1, sgteff, 1
!
    call jeveuo(jexnum(conec(1:15), maille(i)), 'L', adrs)
!
    zi(ansg + i-1) = zi(adrs)
    zi(ansd + i-1) = zi(adrs + 1)
!
    30 end do
!
    do 31, i = 1, sgteff, 1
!
!
    31 end do
!
500  continue
    if ((sfini .lt. sgteff) .and. (m .lt. nbm)) then
!
        m = m + 1
!
        call jeveuo(type, 'L', iatyma)
        atypm=iatyma-1+m
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(atypm)), typm)
!
        if ((typm .ne. 'POI1') .and. (typm .ne. 'SEG2') .and. (typm .ne. 'SEG3')) then
!
            call jelira(jexnum(conec(1:15), m), 'LONMAX', nbn, k1bid)
            call i2nbrf(nbn, nbc)
!
            do 100, c = 1, nbc, 1
!
            call i2extf(m, c, conec(1:15), type(1:16), ncg,&
                        ncd)
!
            do 110 chm = 1, nbchm, 1
!
                debchm = ptchm(chm)
                finchm = ptchm(chm+1) - 2
!
                do 120, s = debchm, finchm, 1
!
                if (.not. zl(afini + s-1)) then
!
!
                    nsg = zi(ansg + chemin(s)-1)
                    nsd = zi(ansd + chemin(s)-1)
!
                    if (( (ncd .eq. nsd) .and. (ncg .eq. nsg) ) .or.&
                        ( (ncd .eq. nsg) .and. (ncg .eq. nsd) )) then
!
                        if (m1(chemin(s)) .eq. 0) then
!
                            m1(chemin(s)) = m
!
                        else
!
                            m2(chemin(s)) = m
!
                            sfini = sfini + 1
!
                            zl(afini + s-1) = .true.
!
                        endif
!
                    endif
!
                endif
!
120              continue
!
110          continue
!
100          continue
!
        endif
!
        goto 500
!
    endif
!
    call jedetr('&INTFINI')
    call jedetr('&INTNSG')
    call jedetr('&INTNSD')
!
    call jedema()
end subroutine
