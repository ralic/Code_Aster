subroutine op0009()
    implicit none
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     COMMANDE:  CALC_MATR_ELEM
!
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/chpver.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/meamac.h"
#include "asterfort/meamgy.h"
#include "asterfort/meamme.h"
#include "asterfort/mecact.h"
#include "asterfort/medome.h"
#include "asterfort/meimme.h"
#include "asterfort/memaac.h"
#include "asterfort/memame.h"
#include "asterfort/meonme.h"
#include "asterfort/meriac.h"
#include "asterfort/merifs.h"
#include "asterfort/merige.h"
#include "asterfort/merigy.h"
#include "asterfort/merime.h"
#include "asterfort/meriro.h"
#include "asterfort/merith.h"
#include "asterfort/redetr.h"
#include "asterfort/sdmpic.h"
#include "asterfort/wkvect.h"
!
    real(kind=8) :: time, tps(6)
    character(len=1) :: base
    character(len=4) :: ctyp, kmpic
    character(len=8) :: modele, cara, sigg, nomcmp(6), blan8, strx
    character(len=8) :: rigi8, mass8
    character(len=16) :: type, oper, suropt
    character(len=19) :: kcha, matel, rigiel, massel, resuel
    character(len=24) :: time2, mate, compor
    logical :: exitim
    integer :: nchar, n1, jrecc, n2, n3, n4, ier, n5, nh, n6, ncha, icha
    integer :: ibid, nbresu, jrelr, iresu, iexi, n7
    data nomcmp/'INST    ','DELTAT  ','THETA   ','KHI     ',&
     &     'R       ','RHO     '/
    data tps/0.d0,2*1.d0,3*0.d0/
! DEB ------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    base = 'G'
!
    call getres(matel, type, oper)
    blan8 = '        '
    rigi8 = ' '
    mass8 = ' '
    sigg = ' '
    call getvid(' ', 'RIGI_MECA', scal=rigi8, nbret=n1)
    rigiel=rigi8
    call getvid(' ', 'MASS_MECA', scal=mass8, nbret=n2)
    massel=mass8
    call getvtx(' ', 'OPTION', scal=suropt, nbret=n3)
    call getvid(' ', 'SIEF_ELGA', scal=sigg, nbret=n4)
    if (n4 .ne. 0) then
        call chpver('F', sigg, 'ELGA', 'SIEF_R', ier)
    endif
    call getvid(' ', 'STRX_ELGA', scal=strx, nbret=n7)
    if (n7 .ne. 0) then
        call chpver('F', strx, 'ELGA', 'STRX_R', ier)
    endif
    call getvr8(' ', 'INST', scal=time, nbret=n5)
    if (n5 .eq. 0) time = 0.d0
    call getvis(' ', 'MODE_FOURIER', scal=nh, nbret=n6)
    kcha = '&&OP0009.CHARGES'
    call medome(modele, mate, cara, kcha, ncha,&
                ctyp, blan8)
! POUR LES MULTIFIBRES ON SE SERT DE COMPOR
    compor=mate(1:8)//'.COMPOR'
    call jeveuo(kcha, 'E', icha)
    exitim = .true.
    time2 = '&TIME'
!
    tps(1) = time
!
    if (suropt .eq. 'RIGI_MECA') then
        call merime(modele, ncha, zk8(icha), mate, cara,&
                    exitim, time, compor, matel, nh,&
                    base)
!
    else if (suropt.eq.'RIGI_FLUI_STRU') then
        call merifs(modele, ncha, zk8(icha), mate, cara,&
                    exitim, time, matel, nh)
!
    else if (suropt.eq.'RIGI_GEOM') then
        call merige(modele, cara, sigg, strx, matel,&
                    'G', nh)
!
    else if (suropt.eq.'RIGI_ROTA') then
        call meriro(modele, cara, ncha, zk8(icha), mate,&
                    exitim, time, compor, matel)
!
    else if (suropt.eq.'MECA_GYRO') then
        call meamgy(modele, mate, cara, compor, matel,&
                    ncha, zk8(icha))
!
    else if (suropt.eq.'RIGI_GYRO') then
        call merigy(modele, mate, cara, compor, matel,&
                    ncha, zk8(icha))
!
    else if (suropt.eq.'MASS_MECA') then
!        COMPOR = ' '
        call memame(suropt, modele, ncha, zk8(icha), mate,&
                    cara, exitim, time, compor, matel,&
                    base)
!
    else if (suropt.eq.'MASS_FLUI_STRU') then
!        COMPOR = ' '
        call memame(suropt, modele, ncha, zk8(icha), mate,&
                    cara, exitim, time, compor, matel,&
                    base)
!
    else if (suropt.eq.'MASS_MECA_DIAG') then
!        COMPOR = ' '
        call memame(suropt, modele, ncha, zk8(icha), mate,&
                    cara, exitim, time, compor, matel,&
                    base)
!
    else if (suropt(1:9).eq.'AMOR_MECA') then
        call meamme(suropt, modele, ncha, zk8(icha), mate,&
                    cara, exitim, time, 'G', rigiel,&
                    massel, matel)
!
    else if (suropt.eq.'IMPE_MECA') then
        call meimme(modele, ncha, zk8(icha), mate, matel)
!
    else if (suropt.eq.'ONDE_FLUI') then
        call meonme(modele, ncha, zk8(icha), mate, matel)
!
    else if (suropt.eq.'RIGI_MECA_HYST') then
        call meamme(suropt, modele, ncha, zk8(icha), mate,&
                    cara, exitim, time, 'G', rigiel,&
                    massel, matel)
!
    else if (suropt.eq.'RIGI_THER') then
        call mecact('V', time2, 'MODELE', modele//'.MODELE', 'INST_R',&
                    ncmp=6, lnomcmp=nomcmp, vr=tps)
        call merith(modele, ncha, zk8(icha), mate, cara,&
                    time2, matel, nh, base)
!
    else if (suropt.eq.'RIGI_ACOU') then
        call meriac(modele, ncha, zk8(icha), mate, matel)
!
    else if (suropt.eq.'MASS_ACOU') then
        call memaac(modele, mate, matel)
!
    else if (suropt.eq.'AMOR_ACOU') then
        call meamac(modele, ncha, zk8(icha), mate, matel)
!
    endif
    goto 20
!
 20 continue
!
!
!     -- CREATION DE L'OBJET .RECC :
!     ------------------------------
    call getvid(' ', 'CHARGE', nbval=0, nbret=n1)
    if (n1 .lt. 0) then
        nchar=-n1
        call wkvect(matel//'.RECC', 'G V K8', nchar, jrecc)
        call getvid(' ', 'CHARGE', nbval=nchar, vect=zk8(jrecc), nbret=n1)
    endif
!
!
!     -- SI MATEL N'EST PAS MPI_COMPLET, ON LE COMPLETE :
!     ----------------------------------------------------
    call jeexin(matel//'.RELR', iexi)
    if (iexi .gt. 0) then
        call jelira(matel//'.RELR', 'LONMAX', nbresu)
        call jeveuo(matel//'.RELR', 'L', jrelr)
        do iresu = 1, nbresu
            resuel=zk24(jrelr+iresu-1)
            call jeexin(resuel//'.RESL', iexi)
            if (iexi .eq. 0) goto 101
            call dismoi('F', 'MPI_COMPLET', resuel, 'RESUELEM', ibid,&
                        kmpic, ibid)
            ASSERT((kmpic.eq.'OUI').or.(kmpic.eq.'NON'))
            if (kmpic .eq. 'NON') call sdmpic('RESUELEM', resuel)
101         continue
        end do
    endif
!
!
!
!     -- DESTRUCTION DES RESUELEM NULS :
!     ----------------------------------
    call redetr(matel)
!
!
    call jedetr('&MEAMAC2           .RELR')
    call jedetr('&MEAMAC2           .RERR')
!
    call jedetr('&MERIAC1           .RELR')
    call jedetr('&MERIAC1           .RERR')
    call jedetr('&MERIAC2           .RELR')
    call jedetr('&MERIAC2           .RERR')
!
    call jedetr('&MERITH1           .RELR')
    call jedetr('&MERITH2           .RELR')
    call jedetr('&MERITH3           .RELR')
    call jedetr('&MERITH1           .RERR')
    call jedetr('&MERITH2           .RERR')
    call jedetr('&MERITH3           .RERR')
!
    call jedema()
end subroutine
